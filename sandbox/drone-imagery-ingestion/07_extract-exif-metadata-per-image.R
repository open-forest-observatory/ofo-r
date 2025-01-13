# Purpose: Read a CSV file containing the EXIF data for all images in a dataset (multiple missions),
# *at the sub-mission level* (i.e., a single date if a two-date mission; a single orientation of
# a two-part grid mission) and extract/process into human-readable metadata at the image level and
# sub-mission level using the metadata extraction functions of the ofo package.

# This needs to be included or else the bind_rows call to create polygon_perdataset fails. I don't
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
baserow_dataset_metadata = read.csv(metadata_sub_mission_filepath)

# Assign the "dataset_id" parameter that is used in the metadata extraction functions. This is done
# here as opposed to in the metadata extraction to keep those functions flexible as to how a dataset
# is defined (e.g. a "mission" or a "sub-mission"). Here we are defining a dataset as a
# "sub-mission".
exif$dataset_id = exif$submission_id
# Pad the mission ID
exif = exif |> mutate(mission_id = str_pad(mission_id, 6, pad = "0", side = "left"))

# Compute the unique sub mission IDs
unique_datasets = unique(exif$dataset_id)

# For parallelizing, make a list of subsets of the exif dataframe, one for each dataset
exif_per_dataset <- lapply(
  unique_datasets,
  function(unique_dataset) {
    # Extract the exif rows matching that sub-mission ID
    dataset_exif <- exif |>
      filter(dataset_id == unique_dataset)
    return(dataset_exif)
  }
)

# Get aircraft model name to use for determining the appropriate image-level metadata extractor
# functions to use
aircraft_model_names <- lapply(
  unique_datasets,
  function(dataset_ID) {
    # Extract the baserow entries for this sub-mission ID
    baserow_for_dataset <- baserow_dataset_metadata[
      baserow_dataset_metadata$sub_mission_id == dataset_ID,
    ]

    # There should only be one matching row
    if (nrow(baserow_for_dataset) != 1) {
      stop(paste("Error: there was not one corresponding baserow entry: ", baserow_for_dataset))
    }

    # Extract the aircraft model name
    aircraft_model_name = baserow_for_dataset$aircraft_model_name

    return(aircraft_model_name)
  }
)

# Extract the metadata in a standardized manner no matter the platform
# This is run on a per-image basis, but requires the per-sub-mission platform name to understand how
# to interpret the information.
future::plan("future::multisession")
print("Started extracting metadata per image to a standardized format")
metadata_per_dataset = furrr::future_map2(
  .x = exif_per_dataset,
  .y = aircraft_model_names,
  .f = extract_imagery_perimage_metadata,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE)
)
# Bind the rows together across all datasets
metadata_perimage = bind_rows(metadata_per_dataset)

# Filter the extracted metadata to only include images that were retained in the dataset-level
# metadata extraction based on intersection with the mission polygon

# Create the folder to save the image metadata in
create_dir(dirname(metadata_perimage_filepath))

# Write out the standardized metadata per image
write_csv(metadata_perimage, metadata_perimage_filepath)
