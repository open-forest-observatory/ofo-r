# Purpose: Read a CSV file containing the EXIF data for all images and extract standardized
# metadata. The sensor/platform is used to correctly interpret the metadata conventions.

# sf needs to be included or else the bind_rows call to create polygon_perdataset fails. I don't
# understand the concept of dispatching and why this is required.
library(sf)
library(tidyverse)
library(ofo)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
FOLDER_BASEROW_CROSSWALK_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting/"
EXIF_PATH = "/ofo-share/drone-imagery-organization/3b_exif-unprocessed/"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"


# Derived constants
exif_filepath = file.path(EXIF_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv"))
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))

metadata_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))
polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))
metadata_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))

## Workflow

# Read in the EXIF
exif = read.csv(exif_filepath)
# Read in the baserow metadata
baserow_submission_metadata = read.csv(metadata_sub_mission_filepath)

# Compute the unique sub mission IDs. The metadata is extracted per-image, but the sub-mission
# gives us information about the platform/sensor that is neccessary for interpreting the raw
# metadata
unique_submission_ids = unique(exif$submission_id)

# For parallelizing, make a list of subsets of the exif dataframe, one for each submission
exif_per_submission <- lapply(
  unique_submission_ids,
  function(unique_submission_id) {
    # Extract the exif rows matching that sub-mission ID
    submission_exif <- exif |>
      filter(submission_id == unique_submission_id)
    return(submission_exif)
  }
)

# Get aircraft model name to use for determining the appropriate image-level metadata extractor
# functions to use
aircraft_model_names <- lapply(
  unique_submission_ids,
  function(unique_submission_id) {
    # Extract the baserow entries for this sub-mission ID
    baserow_for_submission <- baserow_submission_metadata[
      baserow_submission_metadata$sub_mission_id == unique_submission_id,
    ]

    # There should only be one matching row
    if (nrow(baserow_for_submission) != 1) {
      stop(paste("Error: there was not one corresponding baserow entry: ", baserow_for_submission))
    }

    # Extract the aircraft model name
    aircraft_model_name = baserow_for_submission$aircraft_model_name

    return(aircraft_model_name)
  }
)

# Extract the metadata in a standardized manner no matter the platform
# This is run on a per-image basis, but requires the per-sub-mission platform name to understand how
# to interpret the information.
future::plan("future::multisession")
print("Started extracting metadata per image to a standardized format")
metadata_per_submission = furrr::future_map2(
  .x = exif_per_submission,
  .y = aircraft_model_names,
  .f = extract_imagery_perimage_metadata,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE)
)
# Bind the rows together across all submissions
metadata_perimage = bind_rows(metadata_per_submission)

# Copy the sub-mission and mission ID to the output metadata
metadata_perimage$submission_id = exif$submission_id
metadata_perimage$mission_id = exif$mission_id
# Pad the mission ID since it starts as an integer
metadata_perimage = metadata_perimage |> mutate(mission_id = str_pad(mission_id, 6, pad = "0", side = "left"))

# Create the folder to save the image metadata in
create_dir(dirname(metadata_perimage_filepath))
# Write out the standardized metadata per image
write_csv(metadata_perimage, metadata_perimage_filepath)
