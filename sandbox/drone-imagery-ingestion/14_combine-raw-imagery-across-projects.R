# Purpose: Each project (i.e., set of drone missions, such as from a specific year) has its own
# folder, under which there are folders for each mission containing raw imagery. We want to put all
# this imagery at the same level across projects.

library(tidyverse)
library(purrr)
library(furrr)

# Temporarily, we are using the "notcleaned" (i.e., not curated) imagery, but once the cureated raw
# imagery folders are ready, we should switch this to that path.
RAW_IMAGES_PATH = "/ofo-share/drone-imagery-organization/3_sorted-notcleaned-combined"

# Folder to copy (as hardlinks) the combined imagery to, all missions directly under this folder
# regardless of project
COMBINED_IMAGES_PATH = "/ofo-share/drone-imagery-organization/6_combined-across-projects"

## Functions

# Function to copy all the images from a mission folder (within a specified folder) to the combined
# folder (that has all missions, across all projects, all at the top level)
copy_images_to_combined_folder = function(mission_folder, project_folder, raw_images_path, combined_images_path) {

  in_path = file.path(raw_images_path, project_folder, mission_folder)
  infiles = list.files(in_path, full.names = FALSE, recursive = TRUE)
  infile_paths = file.path(in_path, infiles)
  outfile_paths = file.path(combined_images_path, mission_folder, infiles)

  outdirs = unique(dirname(outfile_paths))
  walk(outdirs, dir.create, recursive = TRUE)

  file.link(infile_paths, outfile_paths)

}


## Workflow

future::plan(future::multisession(workers = future::availableCores() * 3))

# Get all the project folders
project_folders = list.dirs(RAW_IMAGES_PATH, full.names = FALSE, recursive = FALSE)
# Make sure they follow the expected naming convention, like 2021-tahoe-aspen (so we don't include other ancillary folders
# like "archive" that may be in this folder)
project_folders = project_folders[grepl(pattern = "^20[1-2][0-9]-[a-z-]+$", project_folders)]

# Loop through each project folder to list and copy over the images
for (project_folder in project_folders) {

  cat("Processing project folder:", project_folder, "\n")

  # Get all the mission folders
  mission_folders = list.dirs(file.path(RAW_IMAGES_PATH, project_folder), full.names = FALSE, recursive = FALSE)

  # Loop through each mission folder to list and copy over the images
  future_walk(mission_folders, copy_images_to_combined_folder, project_folder = project_folder, raw_images_path = RAW_IMAGES_PATH, combined_images_path = COMBINED_IMAGES_PATH)

}
