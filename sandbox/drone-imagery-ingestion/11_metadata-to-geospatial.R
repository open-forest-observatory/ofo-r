# Purpose: Create metadata-annotated polygons (for dataset footrpint) and points (for dataset
# images) at the mission and sub-mission levels.

library(tidyverse)
library(sf)

IMAGERY_PROJECT_NAME = "2019-focal"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

# Derived constants
metadata_sub_mission_perdataset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-full-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_mission_perdataset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-full-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
meadata_sub_mission_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_mission_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))
polygons_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))
polygons_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))



# Load the metadata and polygons
metadata_sub_mission_perdataset = read_csv(metadata_sub_mission_perdataset_filepath)
metadata_mission_perdataset = read_csv(metadata_mission_perdataset_filepath)
metadata_sub_mission_perimage = read_csv(meadata_sub_mission_perimage_filepath)
metadata_mission_perimage = read_csv(metadata_mission_perimage_filepath)
polygons_mission = st_read(polygons_mission_filepath)
polygons_sub_mission = st_read(polygons_sub_mission_filepath)


# Create image geospatial points, annotated with image-level metadata
mission_points = st_as_sf(metadata_mission_perimage, coords = c("lon", "lat"), remove = FALSE, crs = 4326)
sub_mission_points = st_as_sf(metadata_sub_mission_perimage, coords = c("lon", "lat"), remove = FALSE, crs = 4326)

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
st_write(mission_points, file.path(EXTRACTED_METADATA_PATH, paste0("mission-points-w-metadata_", IMAGERY_PROJECT_NAME, ".gpkg")), driver = "GPKG", delete_dsn = TRUE)
st_write(sub_mission_points, file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-points-w-metadata_", IMAGERY_PROJECT_NAME, ".gpkg")), driver = "GPKG", delete_dsn = TRUE)
st_write(mission_polygons, file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons-w-metadata_", IMAGERY_PROJECT_NAME, ".gpkg")), driver = "GPKG", delete_dsn = TRUE)
st_write(sub_mission_polygons, file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons-w-metadata_", IMAGERY_PROJECT_NAME, ".gpkg")), driver = "GPKG", delete_dsn = TRUE)
