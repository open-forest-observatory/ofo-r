# Purpose: Read in the sorted, uncleaned drone images in mission folders and extract all the EXIF
# data, to compile into the mission metadata. Save to a file for further processing in next script.
# In constrast to the previous EXIF read script in this workflow, this script reads the full EXIF
# data and it operates on the sorted (not unsorted) imagery folder.

library(tidyverse)
library(exifr)
library(furrr)

# devtools::install()

library(ofo)

IMAGERY_INPUT_PATH = "/ofo-share/drone-imagery-organization/3_sorted-mission"
EXIF_OUTPUT_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/3_raw-exif"
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")



## Workflow

if(!dir.exists(EXIF_OUTPUT_PATH)) {
  dir.create(EXIF_OUTPUT_PATH, recursive = TRUE)
}

# Determine which missions to process
missions_to_process = read_csv(MISSIONS_TO_PROCESS_LIST_PATH) |>
  pull(mission_id)


# Function to read EXIF data from images and save to CSV, for a specified mission ID
extract_and_save_mission_exif = function(mission_id_foc) {

  # Create the output path for the EXIF data
  exif_output_path = file.path(EXIF_OUTPUT_PATH, paste0(mission_id_foc, ".csv"))

  # Path to the mission imagery folder
  mission_imagery_path = file.path(IMAGERY_INPUT_PATH, mission_id_foc)

  # Get a list of all image files
  image_paths = list.files(mission_imagery_path,
    recursive = TRUE,
    pattern = ".(jpg|JPG|jpeg|JPEG)$",
    full.names = TRUE
  )

  # Extract EXIF data from the images
  exif = read_exif_drop_thumbnails(image_paths)

# Add the mission ID and sub_mission ID to the EXIF data, assuming that the folder organization is:
# {any abs path}/<mission_id>/<submission_id>/<incrementing number per 10000 images>/<image>.jpg

  sub_mission_path = dirname(dirname(image_paths))
  mission_path = dirname(sub_mission_path)

  sub_mission_id = basename(sub_mission_path)
  mission_id = basename(mission_path)

  exif$mission_id = mission_id
  exif$sub_mission_id = sub_mission_id

  # Write to file
  file_out = file.path(EXIF_OUTPUT_PATH, paste0(mission_id_foc, ".csv"))
  write_csv(exif, file_out)

  gc()
  rm(exif)
  return(TRUE)

}

# Process each mission in parallel
future::plan(future::multisession, workers = future::availableCores() * 0.9)
exif_list = future_map(missions_to_process, extract_and_save_mission_exif, .progress = TRUE,
                       .options = furrr_options(scheduling = Inf))
