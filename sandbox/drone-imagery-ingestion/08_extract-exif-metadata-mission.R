# Purpose: Read a CSV file containing the EXIF data for all images in a dataset (multiple missions),
# *at the mission level* (i.e., combining both dates of a two-date mission; both orientations of a
# two-part grid mission) and extract/process into human-readable metadata at the image level and
# sub-mission level using the metadata extraction functions of the ofo package.

# TODO: This script is very similar to script 07_extract-exif-metadata-sub-mission.R, but it
# operates at the mission level instead of the sub-mission level. Consider refactoring into (more)
# shared functions to reduce repetition.

library(tidyverse)
library(sf)

# devtools::document(); devtools::install()
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

## Derived constants
exif_filepath = file.path(EXIF_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv"))
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))

# This per-image metadata was saved out in the last step
metadata_perimage_input_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))

# Output files per
metadata_per_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
mission_polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))
# Select only the images that were retained in the mission polygons
metadata_perimage_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))

metadata_per_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
sub_mission_polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))
# Select only the images that were retained in the sub-mission polygons
metadata_perimage_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))

# Functions
compute_summary_statistics = function(
    image_metadata,
    column_to_split_on,
    metadata_perimage_filepath,
    metadata_perdataset_filepath,
    polygons_filepath) {
  # Set the dataset_id based on the provided column, either the submission_id or mission_id
  image_metadata$dataset_id = image_metadata[[column_to_split_on]]
  # For parallelizing, make a list of subsets of the metadata dataframe, one for each dataset
  # (either a mission or sub-mission)
  unique_dataset_ids = unique(image_metadata$dataset_id)

  metadata_per_dataset <- lapply(unique_dataset_ids, function(unique_dataset_id) {
    dataset_metadata <- image_metadata |>
      filter(dataset_id == unique_dataset_id)
    return(dataset_metadata)
  })
  # Run dataset-level metadata extraction across each subset
  print("Started computing dataset-level summary statistics")
  future::plan("multisession")
  summary_statistics = furrr::future_map(
    metadata_per_dataset,
    extract_imagery_dataset_metadata,
    crop_to_contiguous = TRUE,
    min_contig_area = 10000,
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )
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

  # Write out the results
  ## The per-dataset summary statistics
  write_csv(metadata_perdataset, metadata_perdataset_filepath)
  # The per-image data for retained images
  write_csv(metadata_perimage, metadata_perimage_filepath)
  # The polygon bounds
  sf::st_write(polygon_perdataset, polygons_filepath, delete_dsn = TRUE)
}


## Workflow

# Read in image-level metadata that was parsed in step 07
image_metadata = read_csv(metadata_perimage_input_filepath)

# Compute the summary statistics based on missions
compute_summary_statistics(
  image_metadata,
  "mission_id",
  metadata_perimage_mission_filepath,
  metadata_per_mission_filepath,
  mission_polygons_filepath
)
# Compute the summary statistics based on sub-missions
compute_summary_statistics(
  image_metadata,
  "submission_id",
  metadata_perimage_sub_mission_filepath,
  metadata_per_sub_mission_filepath,
  sub_mission_polygons_filepath
)
