# This script has two purposes: 1. Demonstrate how to run functions to extract metadata from imagery
# dataset EXIF data, and 2. Demonstrate how to write a new metadata extraction function.

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()
library(dplyr)

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "derek-map-imagery-js.txt"))

# Which folder contans folders of image sets that we want to extract EXIF from?
focal_folder = file.path(datadir, "0_raw", "2023-tahoe-aspen")

# Which folder do we want to save the extracted data to?
out_folder = file.path(datadir, "extracted-exif", "2023-tahoe-aspen")

# Define sets to skip because corrupted
sets_to_skip = c("20220621-0037", "20220622-0047")


# Extract the EXIF data from single image set (folder of images)
extract_singleset_exif = function(image_folder, out_folder, sets_to_skip) {

  if(basename(image_folder) %in% sets_to_skip) {
    message("Skipping ", image_folder, " because it is known to be corrupted")
    return(TRUE)
  }

  out_filepath = file.path(out_folder, paste0(basename(image_folder), ".csv"))

  if (file.exists(out_filepath)) {
    message("EXIF data already extracted for ", image_folder)
    return(TRUE)
  }

  message("Extracting EXIF data for ", image_folder, "...", appendLF = FALSE)

  image_paths = list.files(image_folder,
                          recursive = TRUE,
                          pattern = ".(jpg|JPG|jpeg|JPEG)$",
                          full.names = TRUE)

  exif = exifr::read_exif(image_paths)

  # drop the ThumbnailImage and PreviewImage
  exif = exif |>
    select(-ThumbnailImage, -PreviewImage)

  if (!dir.exists(dirname(out_filepath))) dir.create(dirname(out_filepath), recursive = TRUE)

  message("done")

  readr::write_csv(exif, out_filepath)

  return(TRUE)

}


# Extract the EXIF data from a directory of image sets
extract_dir_exif = function(focal_folder, out_folder, sets_to_skip) {

  # Get the subfolders, which are the folders we want to get image EXIF from
  image_folders = list.dirs(file.path(focal_folder), full.names = TRUE, recursive = FALSE)

  future::plan(future::multisession())

  furrr::future_map(image_folders, extract_singleset_exif, out_folder = out_folder, sets_to_skip = sets_to_skip,
                    .options = furrr::furrr_options(chunk_size = 1))
  future::plan(future::sequential)

}

extract_dir_exif(focal_folder, out_folder, sets_to_skip)



# # Troubleshooting

# a = exifr::read_exif("/ofo-share/drone-imagery-all//1_manually-cleaned/2022-early-regen/20220621-0037_and_20220622-0047", recursive = TRUE)


# Combine all extracted EXIF into one data frame, to see if there are sets with missing data (e.g.
# corrupted images)

files = list.files(file.path(out_folder), pattern = ".csv$", full.names = TRUE, recursive = TRUE)
files

d = purrr::map(files, readr::read_csv)

all_cols_to_char = function(x) {
  x |> dplyr::mutate(across(everything(), as.character))
}

d2 = purrr::map(d, all_cols_to_char)

d3 = bind_rows(d2)

readr::write_csv(d3, file.path(datadir, "temp", "combined-exif-inspect_2023-aspen.csv"))

# get the directories of the images that are missing data

d3 |>
  filter(is.na(ExposureTime)) %>%
  pull(Directory) %>%
  dirname() %>%
  table()
