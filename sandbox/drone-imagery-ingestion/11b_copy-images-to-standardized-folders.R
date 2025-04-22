# Purpose: Read the processed image-level metadata data which contains the plan for sorting the
# images (origin and destination filepaths). Copy the images to the new folder structure based on
# the plan.

library(tidyverse)
library(furrr)
library(sf)

# In
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")
PARSED_EXIF_FOR_RETAINED_IMAGES_PATH = "/ofo-share/drone-imagery-organization/metadata/3_final/3_parsed-exif-per-image"
IMAGERY_INPUT_PATH = "/ofo-share/drone-imagery-organization/1_manually-cleaned"

# Out
SORTED_IMAGERY_OUT_FOLDER = "/ofo-share/drone-imagery-organization/2_sorted"

## Workflow

# Create the output folder
if (!dir.exists(SORTED_IMAGERY_OUT_FOLDER)) {
  dir.create(SORTED_IMAGERY_OUT_FOLDER, recursive = TRUE)
}

# Determine which missions to process
missions_to_process = read_csv(MISSIONS_TO_PROCESS_LIST_PATH) |>
  pull(mission_id)


copy_mission_images = function(mission_id_foc) {
  image_metadata_file = file.path(PARSED_EXIF_FOR_RETAINED_IMAGES_PATH, paste0(mission_id_foc, ".gpkg"))
  image_metadata = st_read(image_metadata_file)


  # Perform the file copy, specifically as hardlinks

  # Determine the absolute input and output paths
  image_metadata$image_path_contrib_abs = file.path(
    IMAGERY_INPUT_PATH,
    image_metadata$image_path_contrib
  )

  image_metadata$image_path_ofo_abs = file.path(
    SORTED_IMAGERY_OUT_FOLDER,
    image_metadata$image_path_ofo
  )



  # Create the output folder(s)
  folders_out_abs = unique(dirname(image_metadata$image_path_ofo_abs))
  sapply(folders_out_abs, dir.create, recursive = TRUE, showWarnings = FALSE)

  # Copy files as hardlinks
  walk2(image_metadata$image_path_contrib_abs, image_metadata$image_path_ofo_abs, file.link)
}


future::plan(multisession, workers = future::availableCores() * 1.9)
furrr::future_walk(missions_to_process, copy_mission_images, .progress = TRUE)
