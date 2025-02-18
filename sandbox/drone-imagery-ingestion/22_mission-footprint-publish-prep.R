# Purpose: Take the mission geospatial footprints and save them into per-mission .gpkg files in the
# expected directory structure

library(tidyverse)
library(sf)

## Constants

# File paths

MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/all-mission-polygons-w-metadata.gpkg"
PUBLISHABLE_MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-processed/01/mission-footprints-publish"


# Processing constants



## Functions


## Workflow

# Read in the mission footptints (both with metadata)
mission_footprints = st_read(MISSION_FOOTPRINTS_PATH)

# Get all the mission IDs
mission_ids = mission_footprints$mission_id |> unique()


# Split out the footprint of each mission and save to file in the expected directory structure

for (mission_id_foc in mission_ids) {
  mission_footprint = mission_footprints |>
    filter(mission_id == mission_id_foc) |>
    st_as_sfc()

  mission_footprint_path = file.path(PUBLISHABLE_MISSION_FOOTPRINTS_PATH, mission_id_foc, "footprint", "footprint.gpkg")

  # Make sure the directory exists
  dir.create(dirname(mission_footprint_path), recursive = TRUE, showWarnings = FALSE)

  # Write the footprint to file
  st_write(mission_footprint, mission_footprint_path, delete_dsn = TRUE)

}
