# Purpose: Some EXIF attributes for some images may be problematic for downstream photogrammetry or
# other analyses, depending on how the downstream software handles them. This script fixes the EXIF
# to avoid the poblems we have encountered. For all the images in the sorted missions folder
# (including all subdirectories), set the "Orientation" flag to `1`. Also, for all images that have
# GPSTimeStamp with decimal seconds, remove the GPSTimeStamp flag. Note that you must have exiftool
# installed first, such as by `sudo apt install libimage-exiftool-perl`.

library(furrr)
library(tidyverse)
library(exifr)

SORTED_MISSIONS_FOLDER = "/ofo-share/drone-imagery-organization/2_sorted"
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")
IMAGE_EXIF_W_SORTING_PLAN_FOLDER = "/ofo-share/drone-imagery-organization/metadata/1_reconciling-contributions/2_exif-w-sorting-plan/"


# Function to run a system command on a list of files, but break it into smaller chunks
# to avoid command line length limits
run_cmd_chunks = function(cmd, filepaths, chunk_size = 500) {
  # Split the filepaths into chunks
  chunks = split(filepaths, ceiling(seq_along(filepaths) / chunk_size))

  # Run the command on each chunk
  for (chunk in chunks) {
    file_string = paste0(chunk, collapse = " ")
    system(paste0(cmd, " ", file_string))
  }
}


# Workflow


# Determine which missions to process
missions_to_process = read_csv(MISSIONS_TO_PROCESS_LIST_PATH) |>
  pull(mission_id)


fix_orientation_flag = function(mission_id_foc) {
  # Get each mission folder (for parallelizing)
  folder = file.path(SORTED_MISSIONS_FOLDER, mission_id_foc)

  # Get all images in the folder
  image_filepaths = list.files(folder, full.names = TRUE, recursive = TRUE, pattern = "(.jpg$)|(.jpeg$)|(.JPG$)|(.JPEG$)")

  # One approach is to pull the previously extracted EXIF data
  image_filenames = basename(image_filepaths)

  images = data.frame(filename = image_filenames,
                      filepath = image_filepaths)

  # Load the previously extracted EXIF data and keep only the relevant columns
  exif = read.csv(file.path(IMAGE_EXIF_W_SORTING_PLAN_FOLDER, paste0(mission_id_foc, ".csv"))) |>
    select(any_of(c("image_filename_out", "Orientation", "GPSTimeStamp")))

  # Sort the exif to match the image filepaths
  exif = left_join(images, exif, by = c("filename" = "image_filename_out"))

  # Redo in base R
  exif$fix_orientation = ifelse(is.null(exif$Orientation), FALSE, (exif$Orientation != 1))
  exif$fix_gpstimestamp = ifelse(is.null(exif$GPSTimeStamp), FALSE, (grepl("[0-9]+:[0-9]+:[0-9]+\\.[0-9]+", exif$GPSTimeStamp)))

  # Encode as one-hot between orientation, gpstimestamp, or both
  exif = exif |>
    mutate(fix_both = (fix_orientation & fix_gpstimestamp)) |>
    # if both are true, then set the other two to false
    mutate(fix_orientation = ifelse(fix_both, FALSE, fix_orientation),
           fix_gpstimestamp = ifelse(fix_both, FALSE, fix_gpstimestamp))




  # # Alternatively: 
  # # Ask exiftool what their orientation and GPSTimeStamp
  # exif_newread = read_exif(image_filepaths, tags = c("Orientation", "GPSTimeStamp"))

  # Fix orientation only
  exif_to_fix_orientation = exif |>
    filter(fix_orientation == TRUE)

  if (nrow(exif_to_fix_orientation) > 0) {

    cat("Fixing orientation flag for ", nrow(exif_to_fix_orientation), " images in ", mission_id_foc, "\n")

    # Construct exiftool command to fix the orientation flag

      command = "exiftool -n -r -fast4 -overwrite_original -Orientation=1 "
    run_cmd_chunks(command, exif_to_fix_orientation$filepath)
  }

  # Fix GPSTimeStamp only
  exif_to_fix_gpstimestamp = exif |>
    filter(fix_gpstimestamp == TRUE)

  if (nrow(exif_to_fix_gpstimestamp) > 0) {
    cat("Fixing GPSTimeStamp for ", nrow(exif_to_fix_gpstimestamp), " images in ", mission_id_foc, "\n")

    # Construct exiftool command to fix the GPSTimeStamp
    command = "exiftool -n -r -fast4 -overwrite_original -GPSTimeStamp= "
    run_cmd_chunks(command, exif_to_fix_gpstimestamp$filepath)
  }

  # Fix both
  exif_to_fix_both = exif |>
    filter(fix_both == TRUE)

  if (nrow(exif_to_fix_both) > 0) {
    cat("Fixing both orientation flag and GPSTimeStamp for ", nrow(exif_to_fix_both), " images in ", mission_id_foc, "\n")

    # Construct exiftool command to fix the orientation flag
    paths_to_fix_string = paste0(exif_to_fix_both$filepath, collapse = " ")
    cmd = paste0("exiftool -n -r -fast4 -overwrite_original -Orientation=1 -GPSTimeStamp= ", paths_to_fix_string)

    cmd = "exiftool -n -r -fast4 -overwrite_original -Orientation=1 -GPSTimeStamp= "
    run_cmd_chunks(cmd, exif_to_fix_both$filepath)
  }

}

# Run in parallel across all specified missions
future::plan(future::multisession, workers = future::availableCores() * 1.9)
res = future_map(missions_to_process, fix_orientation_flag, .progress = TRUE)
