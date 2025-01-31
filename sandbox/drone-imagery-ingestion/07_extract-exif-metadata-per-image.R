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


## Functions
# Note that `exif` will be defined in the global environment
extract_sub_mission_exif = function(unique_sub_mission_id) {
  # Extract the exif rows matching that sub-mission ID
  sub_mission_exif <- exif |>
    filter(sub_mission_id == unique_sub_mission_id)
  return(sub_mission_exif)
}

# Note that `baserow_sub_mission_metadata` will be defined in the global environment
extract_aircraft_model_name = function(unique_sub_mission_id) {
  # Extract the baserow entries for this sub-mission ID
  baserow_for_sub_mission <- baserow_sub_mission_metadata[
    baserow_sub_mission_metadata$sub_mission_id == unique_sub_mission_id,
  ]

  # There should only be one matching row
  if (nrow(baserow_for_sub_mission) == 1) {
    # Extract the aircraft model name
    aircraft_model_name = baserow_for_sub_mission$aircraft_model_name
  } else {
    print(paste("There were ", nrow(baserow_for_sub_mission), " rows in baserow matching ", unique_sub_mission_id))
    # Default value indicating a single corresponding entry could not be found
    aircraft_model_name = NaN
  }

  return(aircraft_model_name)
}

## Workflow
# Read in the EXIF
exif = read.csv(exif_filepath)
# Read in the baserow metadata
baserow_sub_mission_metadata = read.csv(metadata_sub_mission_filepath)

# Compute the unique sub mission IDs. The metadata is extracted per-image, but the sub-mission
# gives us information about the platform/sensor that is neccessary for interpreting the raw
# metadata
unique_sub_mission_ids = unique(exif$sub_mission_id)

# For parallelizing, make a list of subsets of the exif dataframe, one for each sub-mission
exif_per_sub_mission <- lapply(
  unique_sub_mission_ids,
  extract_sub_mission_exif
)

# Get aircraft model name to use for determining the appropriate image-level metadata extractor
# functions to use
aircraft_model_names <- lapply(
  unique_sub_mission_ids,
  extract_aircraft_model_name
)

# Only retain sub-missions for which there was valid baserow data identifying the aircraft model
exif_per_sub_mission = exif_per_sub_mission[!is.na(aircraft_model_names)]
aircraft_model_names = aircraft_model_names[!is.na(aircraft_model_names)]

# Extract the metadata in a standardized manner no matter the platform
# This is run on a per-image basis, but requires the per-sub-mission platform name to understand how
# to interpret the information.
future::plan("future::multisession")
print("Started extracting metadata per image to a standardized format")
metadata_per_sub_mission = furrr::future_map2(
  .x = exif_per_sub_mission,
  .y = aircraft_model_names,
  .f = extract_imagery_perimage_metadata,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE)
)
# Bind the rows together across all sub-missions
metadata_perimage = bind_rows(metadata_per_sub_mission)
# Ensure that the exif data is ordered in the same way and any dropped sub-missions are reflected
exif = bind_rows(exif_per_sub_mission)

# Copy the sub-mission and mission ID to the output metadata
metadata_perimage$sub_mission_id = exif$sub_mission_id
metadata_perimage$mission_id = exif$mission_id
# Pad the mission ID since it starts as an integer
metadata_perimage = metadata_perimage |> mutate(mission_id = str_pad(mission_id, 6, pad = "0", side = "left"))

# Create the folder to save the image metadata in
create_dir(dirname(metadata_perimage_filepath))
# Write out the standardized metadata per image
write_csv(metadata_perimage, metadata_perimage_filepath)
