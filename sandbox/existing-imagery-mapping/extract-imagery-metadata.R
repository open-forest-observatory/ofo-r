# Take EXIF data and extract metadata (e.g. altitude, gimbal pitch) and mission polygon and write to
# gpkg

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()
library(terra)

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "derek-map-imagery-js.txt"))


# --- 1. Workflow for running metadata extraction ---

# Get a list of the files containing the EXIF data (one file per image dataset).
exif_files = list.files(file.path(datadir, "extracted-exif"), pattern = "^exif.+\\.csv$", recursive = TRUE, full.names = TRUE)


# Define which test EXIF file to run the functions on
exif_file = exif_files[1]

# Run for that one EXIF file
extract_metadata_dy(exif_file, plot_flightpath = TRUE)





exif_file = exif_files[29]
exif = prep_exif(exif_file)
exif$dataset_id |> unique()

