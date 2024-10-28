# Purpose: Read in the manually cleaned drone images in mission folders and extract relevant EXIF
# data needed for sorting. Save to a file for further processing in next script.

library(tidyverse)
library(exifr)
library(furrr)

IMAGERY_PROJECT_NAME = "2020-ucnrs"

IMAGERY_INPUT_PATH = "/ofo-share/drone-imagery-organization/1_manually-cleaned"
EXIF_OUTPUT_PATH = "/ofo-share/drone-imagery-organization/1b_exif-unprocessed/"


get_image_data = function(dataset_folder) {

  # Get the full path to the dataset folder
  base_folder = basename(dataset_folder)

  # Get list of image files
  image_filepaths = list.files(dataset_folder, full.names = TRUE, recursive = TRUE, pattern = "(.jpg$)|(.jpeg$)|(.JPG$)|(.JPEG$)")

  # Get exif data
  exif = read_exif(image_filepaths)

  # Compile relevant per-image info (including potential ways to distinguish two drones) into data
  # frame

  # Check if nay of the relevant columns are null
  date_null = is.null(exif$DateTimeOriginal)
  model_null = is.null(exif$Model)
  serialnumber_null = is.null(exif$SerialNumber)
  if(date_null || model_null || serialnumber_null) {
    warning("Null values (likely complete image corruption) found in ", base_folder, ". Skipping.")
    return(NULL)
  }

  image_data_onefolder = data.frame(folder_in = base_folder,
                           image_path = image_filepaths,
                           date = exif$DateTimeOriginal,
                           model = exif$Model,
                           serialnumber = exif$SerialNumber)

  return(image_data_onefolder)

}


imagery_input_path = file.path(IMAGERY_INPUT_PATH, IMAGERY_PROJECT_NAME)

# All folders
folders = list.dirs(imagery_input_path, full.names = TRUE, recursive = FALSE)

plan = future::plan(multicore)
l = future_map(folders, get_image_data)

image_data = bind_rows(l)

exif_output_path = file.path(EXIF_OUTPUT_PATH, paste0(IMAGERY_PROJECT_NAME, ".csv"))
write_csv(image_data, file.path(exif_output_path))
