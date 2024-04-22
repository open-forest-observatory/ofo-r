# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "steven-metadata-laptop.txt"))

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define a function to process pitch values
process_pitch_values <- function(pitch_values) {
  # Exclude pitch values within ±2 degrees of Nadir (-90 degrees)
  filtered_pitch_values = pitch_values[abs(pitch_values + 90) > 2]

  # Check if the filtered_pitch_values contains any NA values
  if (any(is.na(filtered_pitch_values))) {
    # Print a warning message and return NA if NA values are present
    warning("Some pitch values are NA. Skipping calculation for this file.")
    return(NA)
  }

  # Check if the mission is smart oblique based on the presence of three different pitch values
  if (length(unique(filtered_pitch_values)) == 3) {
    # Compute the absolute values and then average them for smart oblique shots
    processed_pitch = mean(abs(filtered_pitch_values), na.rm = TRUE)
  } else {
    # Use the regular mean for non-smart oblique missions
    processed_pitch = mean(filtered_pitch_values, na.rm = TRUE)
  }

  return(processed_pitch)
}

# Process each EXIF file
for (exif_file in exif_files) {
  exif = prep_exif(exif_file)
  camera_pitch_values = exif$CameraPitch
  camera_pitch_derived = process_pitch_values(camera_pitch_values)
  if (!is.na(camera_pitch_derived)) {
    print(paste("Processed camera_pitch_derived for", basename(exif_file), ":", camera_pitch_derived, "degrees."))
  }
}

