# Script to run functions to extract metadata from imagery dataset EXIF data

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "derek-metadata-laptop.txt"))


# --- Workflow ---

# Get a list of the files containing the test EXIF data (one file per image dataset)
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Which test EXIF file to use run the functions on
exif_file = exif_files[1]

# Run for that one EXIF file
extract_metadata_dy(exif_file)

# Run on all EXIF files
metadata = purrr::map_dfr(exif_files, extract_metadata_dy)
metadata

# Write results to file (creating directory if it doesn't exist)
out_dir = file.path(datadir, "extracted-metadata", "dataset-level-tabular")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write.csv(metadata, file.path(out_dir, "dataset-metadata_dy.csv"), row.names = FALSE)
