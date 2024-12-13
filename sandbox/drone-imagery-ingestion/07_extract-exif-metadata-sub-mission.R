# Purpose: Read a CSV file containing the EXIF data for all images in a dataset (multiple missions),
# *at the sub-mission level* (i.e., a single date if a two-date mission; a single orientation of
# a two-part grid mission) and extract/process into human-readable metadata at the image level and
# sub-mission level using the metadata extraction functions of the ofo package.

# This needs to be included or else the bind_rows call to create polygon_perdataset fails. I don't
# understand the concept of dispatching and why this is required.
library(sf)
library(tidyverse)
library(ofo)

IMAGERY_PROJECT_NAME = "2020-ucnrs"

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
FOLDER_BASEROW_CROSSWALK_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting/"
EXIF_PATH = "/ofo-share/drone-imagery-organization/3b_exif-unprocessed/"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"


# Derived constants
exif_filepath = file.path(EXIF_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv"))
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))

metadata_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_perdataset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
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
    # Extract the baserow entries for this dataset ID
    baserow_for_dataset <- baserow_dataset_metadata[
      baserow_dataset_metadata$dataset_id == dataset_ID,
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
# This is run on a per-image basis, but requires the per-dataset platform name to understand how to
# interpret the information.
future::plan("future::multisession")
print("Started extracting metadata per image to a standardized format")
metadata_per_dataset = furrr::future_map2(
  .x = exif_per_dataset,
  .y = aircraft_model_names,
  .f = extract_imagery_perimage_metadata,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE)
)
# Additional print since the progress bar doesn't end the line
print("")
print("Started computing dataset-level summary statistics")
# Use the standardized image-level metadata extracted in previous step to compute dataset-level summary statistics
# TODO there may not be a performance benefit if this process is bottlenecked by network constraints
# when downloading the DEM for "extract_flight_terrain_correlation".
# The alternative sequential call with purr is provided below
summary_statistics = furrr::future_map(
  metadata_per_dataset,
  extract_imagery_dataset_metadata,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE)
)
# summary_statistics = purrr::map(
#  metadata_per_dataset,
#  extract_imagery_dataset_metadata,
# )
print("Finished computing dataset-level summary statistics")
# Extract the elements of the summary statistics
metadata_perdataset = dplyr::bind_rows(map(summary_statistics, "dataset_metadata"))
polygon_perdataset = dplyr::bind_rows(map(summary_statistics, "mission_polygon"))
images_retained = unlist(map(summary_statistics, "images_retained"))

metadata_perimage = dplyr::bind_rows(metadata_per_dataset)
# Filter the extracted metadata to only include images that were retained in the dataset-level
# metadata extraction based on intersection with the mission polygon
metadata_perimage = metadata_perimage |>
  filter(image_id %in% images_retained)

# Save the metadata
folders = c(metadata_perimage_filepath, metadata_perdataset_filepath, polygons_filepath)
folders = dirname(folders)

purrr::walk(
  folders,
  create_dir
)

write_csv(metadata_perimage, metadata_perimage_filepath)
write_csv(metadata_perdataset, metadata_perdataset_filepath)
sf::st_write(polygon_perdataset, polygons_filepath, delete_dsn = TRUE)
