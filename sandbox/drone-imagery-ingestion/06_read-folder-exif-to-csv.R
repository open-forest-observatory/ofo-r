# Purpose: Read in the sorted, uncleaned drone images in mission folders and extract all the EXIF
# data, to compile into the dataset metadata. Save to a file for further processing in next script.
# In constrast to the previous EXIF read script in this workflow, this script reads the full EXIF
# data and it operates on the sorted (not unsorted) imagery folder.

library(tidyverse)
library(exifr)
library(furrr)

devtools::load_all()

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)

IMAGERY_INPUT_PATH = "/ofo-share/drone-imagery-organization/3_sorted-notcleaned-combined/"
EXIF_OUTPUT_PATH = "/ofo-share/drone-imagery-organization/3b_exif-unprocessed/"


# Derived constants
project_imagery_path = file.path(IMAGERY_INPUT_PATH, IMAGERY_PROJECT_NAME)
exif_output_filepath = file.path(EXIF_OUTPUT_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv"))


# Get a list of all image files in the project directory
image_paths = list.files(project_imagery_path,
  recursive = TRUE,
  pattern = ".(jpg|JPG|jpeg|JPEG)$",
  full.names = TRUE
)

# Extract the EXIF from all of them
future::plan("multisession")
exif_rows = furrr::future_map(image_paths, read_exif_drop_thumbnails, .progress = TRUE)
exif = bind_rows(exif_rows)

# Add the mission ID and sub_mission ID to the EXIF data, assuming that the folder organization is:
# {any abs path}/<mission_id>/<submission_id>/<incrementing number per 10000 images>/<image>.jpg

sub_mission_path = dirname(dirname(image_paths))
mission_path = dirname(sub_mission_path)

sub_mission_id = basename(sub_mission_path)
mission_id = basename(mission_path)

exif$mission_id = mission_id
exif$sub_mission_id = sub_mission_id

write_csv(exif, file.path(EXIF_OUTPUT_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv")))
