# Purpose: Take the imagery and associated metadata products (image points and mission polygons) for
# each mission and hardlink them all into a single file tree that is ready to be published
# (transferred to cyverse).

library(furrr)
library(tidyverse)

## Constants

# File paths

# In
PROCESSED_MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-organization/metadata/3_final/1_full-metadata-per-mission"
PROCESSED_MISSION_POINTS_PATH = "/ofo-share/drone-imagery-organization/metadata/3_final/3_parsed-exif-per-image"
PROCESSED_IMAGES_PATH = "/ofo-share/drone-imagery-organization/4_to-publish"

# Out
PUBLISHABLE_DATA_TREE = "/ofo-share/drone-data-publish/02/"


# Processing constants
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")



## Functions


## Workflow

# Determine which missions to process
mission_ids_to_process = read_csv(MISSIONS_TO_PROCESS_LIST_PATH) |>
  pull(mission_id)

mission_id_foc = mission_ids_to_process[2]

copy_raw_imagery_to_publishable_tree = function(mission_id_foc) {
  # For tracking down the mission ID(s) that produces warnings when this function is called inside a
  # map() function, you can include this line and see which mission ID warning was printed just
  # before the real warning. warning(paste("Parsing EXIF for mission ID:", mission_id_foc))
  cat("Processing mission", mission_id_foc, "\n")

  # MISSION FOOTPRINTS

  # Hardlink all files from the publishable mission footprints dir to the unified publishable tree
  # First make the directories
  infile = file.path(PROCESSED_MISSION_FOOTPRINTS_PATH, paste0(mission_id_foc, "_mission-metadata.gpkg"))
  outfile = file.path(PUBLISHABLE_DATA_TREE, mission_id_foc, "mission-metadata", paste0(mission_id_foc, "_mission-metadata.gpkg"))
  outdir = dirname(outfile)
  dir.create(outdir, recursive = TRUE)

  file.link(infile, outfile)


  # MISSION POINTS

  # Hardlink all files from the publishable mission points dir to the unified publishable tree
  # First make the directories
  infile = file.path(PROCESSED_MISSION_POINTS_PATH, paste0(mission_id_foc, "_image-metadata.gpkg"))
  outfile = file.path(PUBLISHABLE_DATA_TREE, mission_id_foc, "image-metadata", paste0(mission_id_foc, "_image-metadata.gpkg"))
  outdir = dirname(outfile)
  dir.create(outdir, recursive = TRUE)

  file.link(infile, outfile)


  # IMAGES

  # Hardlink all files from the publishable images dir to the unified publishable tree
  # First make the directories
  infiles = list.files(file.path(PROCESSED_IMAGES_PATH, mission_id_foc), full.names = FALSE, recursive = TRUE)
  infiles_full = file.path(PROCESSED_IMAGES_PATH, mission_id_foc, infiles)
  outfiles = file.path(PUBLISHABLE_DATA_TREE, mission_id_foc, infiles)
  outdirs = unique(dirname(outfiles))

  walk(outdirs, dir.create, recursive = TRUE)

  file.link(infiles_full, outfiles)

  return(TRUE)

}

future::plan("multisession", workers = future::availableCores() * 1.9)
future_map(
  mission_ids_to_process,
  copy_raw_imagery_to_publishable_tree,
  .progress = TRUE
)
