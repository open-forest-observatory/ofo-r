# This script has two purposes: 1. Demonstrate how to run functions to extract metadata from imagery
# dataset EXIF data, and 2. Demonstrate how to write a new metadata extraction function.

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "derek-metadata-laptop.txt"))

# --- 1. Workflow for running metadata extraction ---

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
exif_filepath = exif_files[6]

imagery_metadata_single = extract_imagery_dataset_metadata(exif_filepath,
                                                           plot_flightpath = TRUE,
                                                           crop_to_contiguous = TRUE)


imagery_metadata_all = purrr::map(exif_files, extract_imagery_dataset_metadata,
                                  plot_flightpath = TRUE,
                                  crop_to_contiguous = TRUE) |>
  dplyr::bind_rows()
