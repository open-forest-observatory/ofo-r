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
# Data for the subset of images retained in both the mission polygons and sub-mission polygons are saved out here
metadata_perimage_subset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("exif-metadata_perimage_subset_", IMAGERY_PROJECT_NAME, ".csv"))

# Output files per mission or sub-mission
metadata_per_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
mission_polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))

metadata_per_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
sub_mission_polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))

## Workflow
# Read in image-level metadata that was parsed in step 07
image_metadata = read_csv(metadata_perimage_input_filepath)

# Compute the summary statistics based on missions and save to the provided file paths
images_retained_by_mission = compute_summary_statistics(
  image_metadata = image_metadata,
  column_to_split_on = "mission_id",
  metadata_perdataset_filepath = metadata_per_mission_filepath,
  polygons_filepath = mission_polygons_filepath
)
# Compute the summary statistics based on sub-missions and save to the provided file paths
images_retained_by_sub_mission = compute_summary_statistics(
  image_metadata = image_metadata,
  column_to_split_on = "sub_mission_id",
  metadata_perdataset_filepath = metadata_per_sub_mission_filepath,
  polygons_filepath = sub_mission_polygons_filepath
)

# Compute the images that were retained in both the mission polygons and the sub-mission polygons
images_retained_in_both = intersect(images_retained_by_mission, images_retained_by_sub_mission)

# Filter the image metadata to only include data for those images
image_metadata = image_metadata |> filter(image_id %in% images_retained_in_both)
# Write out the the filtered subset
write_csv(image_metadata, metadata_perimage_subset_filepath)
