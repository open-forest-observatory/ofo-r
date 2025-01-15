# Purpose: Create metadata-annotated polygons (for dataset footrpint) and points (for dataset
# images) at the mission and sub-mission levels.

library(tidyverse)
library(sf)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = readr::read_lines(IMAGERY_PROJECT_NAME_FILE)

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

# Derived constants
metadata_sub_mission_perdataset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-full-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_mission_perdataset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-full-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("exif-metadata_perimage_subset_", IMAGERY_PROJECT_NAME, ".csv"))
polygons_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))
polygons_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))



# Load the metadata and polygons
metadata_sub_mission_perdataset = read_csv(metadata_sub_mission_perdataset_filepath)
metadata_mission_perdataset = read_csv(metadata_mission_perdataset_filepath)
metadata_perimage = read_csv(metadata_perimage_filepath)
polygons_mission = st_read(polygons_mission_filepath)
polygons_sub_mission = st_read(polygons_sub_mission_filepath)


# Create image geospatial points, annotated with image-level metadata
metadata_perimage = st_as_sf(metadata_perimage, coords = c("lon", "lat"), remove = FALSE, crs = 4326)

# Create dataset footprint polygons, annotated with dataset-level metadata
mission_polygons = polygons_mission |>
  st_transform(4326) |>
  left_join(metadata_mission_perdataset, by = c("dataset_id" = "dataset_id")) |>
  # Convert non-permitted column types (e.g. time) to character
  mutate(across(where(~ is(.x, "hms")), ~ as.character(.x)))

sub_mission_polygons = polygons_sub_mission |>
  st_transform(4326) |>
  left_join(metadata_sub_mission_perdataset, by = c("dataset_id" = "dataset_id")) |>
  # Convert non-permitted column types (e.g. time) to character
  mutate(across(where(~ is(.x, "hms")), ~ as.character(.x)))

# Write the geospatial files
st_write(metadata_perimage, file.path(EXTRACTED_METADATA_PATH, paste0("points-w-metadata_", IMAGERY_PROJECT_NAME, ".gpkg")), driver = "GPKG", delete_dsn = TRUE)
st_write(mission_polygons, file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons-w-metadata_", IMAGERY_PROJECT_NAME, ".gpkg")), driver = "GPKG", delete_dsn = TRUE)
st_write(sub_mission_polygons, file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons-w-metadata_", IMAGERY_PROJECT_NAME, ".gpkg")), driver = "GPKG", delete_dsn = TRUE)
