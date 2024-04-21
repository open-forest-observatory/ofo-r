# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "steven-metadata-laptop.txt"))

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
exif_file = exif_files[1]
exif = prep_exif(exif_file)

# Filter out zero pitch values from smart oblique missions
filtered_pitch_values = exif$CameraPitch[exif$CameraPitch != 0]

# Calculate camera_pitch_derived
camera_pitch_derived = mean(filtered_pitch_values)

# Print the result
print(paste("Mean of filtered EXIF pitch values (camera_pitch_derived):", camera_pitch_derived, "degrees."))
