# Purpose: Take the mission geospatial footprints and image points (which are in a series of files
# that combines all missions within one imagery project) and save them into per-mission .gpkg files in the
# expected directory structure. NOTE that when we convert this pipeline to per-mission, most of this
# combining etc logic will be unnecessary, since ultimately everything just gets split out anyway

library(tidyverse)
library(sf)
library(furrr)

## Constants

# File paths

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"
PUBLISHABLE_MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-processed/01/mission-footprints-publish"
PUBLISHABLE_SUB_MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-processed/01/sub-mission-footprints-publish"
PUBLISHABLE_MISSION_POINTS_PATH = "/ofo-share/drone-imagery-processed/01/mission-points-publish"



## Processing constants



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
point_files = list.files(EXTRACTED_METADATA_PATH, pattern = "^points-w-metadata_.*\\.gpkg$", full.names = TRUE)

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
points = future_map(point_files, st_read) |>
  future_map(non_geom_cols_to_character, .options = furrr_options(seed = TRUE)) |>
  bind_rows() |>
  type_convert_sf()


# Get all the mission IDs
mission_ids = mission_polygons$mission_id |> unique()

# Split out the footprint of each mission and save to file in the expected directory structure
for (mission_id_foc in mission_ids) {
  mission_footprint = mission_polygons |>
    filter(mission_id == mission_id_foc) |>
    st_as_sfc()

  mission_footprint_path = file.path(PUBLISHABLE_MISSION_FOOTPRINTS_PATH, mission_id_foc, "footprint", "footprint.gpkg")

  # Make sure the directory exists
  dir.create(dirname(mission_footprint_path), recursive = TRUE, showWarnings = FALSE)

  # Write the footprint to file
  st_write(mission_footprint, mission_footprint_path, delete_dsn = TRUE)

}


# NOTE: Commented out because for now we are not publishing sub-mission footprints. If we decide to,
# this will require a little reworking to save the sub-mission footprints beneath the mission
# footprint folder (now, the sub-mission folders are parallel to the mission folders)
# # Get all the sub-mission IDs
# sub_mission_ids = sub_mission_polygons$sub_mission_id |> unique()

# # Split out the footprint of each sub-mission and save to file in the expected directory structure
# for (sub_mission_id_foc in sub_mission_ids) {
#   sub_mission_footprint = sub_mission_polygons |>
#     filter(sub_mission_id == sub_mission_id_foc) |>
#     st_as_sfc()

#   sub_mission_footprint_path = file.path(PUBLISHABLE_SUB_MISSION_FOOTPRINTS_PATH, sub_mission_id_foc, "footprint", "footprint.gpkg")

#   # Make sure the directory exists
#   dir.create(dirname(sub_mission_footprint_path), recursive = TRUE, showWarnings = FALSE)

#   # Write the footprint to file
#   st_write(sub_mission_footprint, sub_mission_footprint_path, delete_dsn = TRUE)

# }


# Get all mission IDs according to the points
point_mission_ids = points$mission_id |> unique()

# Split out the points of each mission and save to file in the expected directory structure
for (mission_id_foc in point_mission_ids) {
  mission_points = points |>
    filter(mission_id == mission_id_foc)

  mission_points_path = file.path(PUBLISHABLE_MISSION_POINTS_PATH, mission_id_foc, "points", "points.gpkg")

  # Make sure the directory exists
  dir.create(dirname(mission_points_path), recursive = TRUE, showWarnings = FALSE)

  # Write the points to file
  st_write(mission_points, mission_points_path, delete_dsn = TRUE)

}
