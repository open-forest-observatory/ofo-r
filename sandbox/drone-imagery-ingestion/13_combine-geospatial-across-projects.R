# Purpose: Each project (i.e., set of drone missions, such as from a specific year) has its own
# .gpkg files for mission and sub-mission polygons and image points. This script combines them into
# a single polygon and single point dataset across all projects.

library(tidyverse)
library(sf)
library(furrr)

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

## Functions

# Function to convert all columns to character except geometry columns
non_geom_cols_to_character = function(df) {
  df |> mutate(across(-any_of(c("geom", "geometry")), as.character))
}

# Function to apply readr::type_convert to a sf object, ignoring geometry cols
type_convert_sf = function(x) {
  geom = x |> st_geometry()
  nongeom = x |> st_drop_geometry()
  nongeom = type_convert(nongeom)

  # Turn times (interpreted by type_convert as difftime) to character
  nongeom = nongeom |> mutate(across(where(is.difftime), as.character))

  x_converted = st_sf(nongeom, geom = geom)

  return(x_converted)
}


## Workflow

# Get the filenames of the polygon files
mission_polygon_files = list.files(EXTRACTED_METADATA_PATH, pattern = "^mission-polygons-w-metadata_.*\\.gpkg$", full.names = TRUE)
sub_mission_polygon_files = list.files(EXTRACTED_METADATA_PATH, pattern = "^sub-mission-polygons-w-metadata_.*\\.gpkg$", full.names = TRUE)

# Get the filenames of the image point files
# mission_point_files = list.files(EXTRACTED_METADATA_PATH, pattern = "^mission-points-w-metadata_.*\\.gpkg$", full.names = TRUE)
sub_mission_point_files = list.files(EXTRACTED_METADATA_PATH, pattern = "^sub-mission-points-w-metadata_.*\\.gpkg$", full.names = TRUE)

future::plan("multisession")

# Read in the polygons, convert non-geometry columns to character, convert cols to natural type, and
# bind them together
mission_polygons = future_map(mission_polygon_files, st_read) |>
  future_map(non_geom_cols_to_character, .options = furrr_options(seed = TRUE)) |>
  bind_rows() |>
  type_convert_sf()

sub_mission_polygons = future_map(sub_mission_polygon_files, st_read) |>
  future_map(non_geom_cols_to_character, .options = furrr_options(seed = TRUE)) |>
  bind_rows() |>
  type_convert_sf()

# Read in the points, convert non-geometry columns to character, convert cols to natural type, and
# bind them together
# mission_points = future_map(mission_point_files, st_read) |>
#  future_map(non_geom_cols_to_character, .options = furrr_options(seed = TRUE)) |>
#  bind_rows() |>
#  type_convert_sf()

sub_mission_points = future_map(sub_mission_point_files, st_read) |>
  future_map(non_geom_cols_to_character, .options = furrr_options(seed = TRUE)) |>
  bind_rows() |>
  type_convert_sf()

# Write the combined files
st_write(mission_polygons, file.path(EXTRACTED_METADATA_PATH, "all-mission-polygons-w-metadata.gpkg"), driver = "GPKG", delete_dsn = TRUE)
st_write(sub_mission_polygons, file.path(EXTRACTED_METADATA_PATH, "all-sub-mission-polygons-w-metadata.gpkg"), driver = "GPKG", delete_dsn = TRUE)
# st_write(mission_points, file.path(EXTRACTED_METADATA_PATH, "all-mission-points-w-metadata.gpkg"), driver = "GPKG", delete_dsn = TRUE)
st_write(sub_mission_points, file.path(EXTRACTED_METADATA_PATH, "all-sub-mission-points-w-metadata.gpkg"), driver = "GPKG", delete_dsn = TRUE)
