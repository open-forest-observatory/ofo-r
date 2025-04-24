# Purpose: For all the images in the sorted missions folder (including all subdirectories), set the
# "Orientation" flag to `1`. Note that you must have exiftool installed first, such as by `sudo apt
# install libimage-exiftool-perl`.

library(furrr)
library(tidyverse)
library(exifr)

SORTED_MISSIONS_FOLDER = "/ofo-share/drone-imagery-organization/2_sorted"
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")

# Workflow


# Determine which missions to process
missions_to_process = read_csv(MISSIONS_TO_PROCESS_LIST_PATH) |>
  pull(mission_id)


fix_orientation_flag = function(mission_id_foc) {
  # Get each mission folder (for parallelizing)
  folder = file.path(SORTED_MISSIONS_FOLDER, mission_id_foc)

  # Get all images in the folder
  image_filepaths = list.files(folder, full.names = TRUE, recursive = TRUE, pattern = "(.jpg$)|(.jpeg$)|(.JPG$)|(.JPEG$)")

  # Ask exiftool what their orientation is
  exif = read_exif(image_filepaths, tags = c("Orientation"))

  exif_to_fix = exif |>
    filter(Orientation != 1)

  # If there are no images to fix, skip
  if (nrow(exif_to_fix) == 0) {
    cat("No orientation flags to fix in", mission_id_foc, "\n")
    return(FALSE)
  } else {
    cat("Fixing", nrow(exif_to_fix), "orientation flags in", mission_id_foc, "\n")
  }

  # Construct exiftool command to fix the orientation flag
  paths_to_fix_string = paste0(exif_to_fix$SourceFile, collapse = " ")
  cmd = paste0("exiftool -n -r -fast4 -overwrite_original -Orientation=1 ", paths_to_fix_string)

  # Run the command
  system(cmd)
}

# Run the commands in parallel
future::plan(future::multisession, workers = future::availableCores() * 1.9)
res = future_map(missions_to_process, fix_orientation_flag, .progress = TRUE)
