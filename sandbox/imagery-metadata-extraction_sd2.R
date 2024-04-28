# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Load necessary libraries
library(lubridate)
library(sf)

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "steven-metadata-laptop.txt"))

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define a function to process pitch values
process_pitch_values <- function(pitch_values) {
  # Exclude pitch values within ±2 degrees of Nadir (-90 degrees)
  filtered_pitch_values = pitch_values[abs(pitch_values + 90) > 2]

  # Compute 0.1, 0.5, and 0.9 quantiles
  quantiles = quantile(filtered_pitch_values, c(0.1, 0.5, 0.9), na.rm = TRUE)  # Add na.rm = TRUE to handle NA values

  # Check if quantiles contains valid values
  if (!any(is.na(quantiles))) {
    # Check if the range between 0.1 and 0.9 quantiles is at least 10 degrees
    pitch_range = quantiles[3] - quantiles[1]
    if (pitch_range >= 10) {
      # Check if the absolute values of 0.1 and 0.9 quantiles are similar
      if (abs(quantiles[1]) - abs(quantiles[3]) < 5) {
        # Use 0.1 or 0.9 quantile directly as the mission pitch
        processed_pitch = quantiles[1]
      } else {
        # Compute the average of absolute pitch values for smart oblique missions
        processed_pitch = mean(abs(filtered_pitch_values), na.rm = TRUE)
      }
    } else {
      # Use the regular mean for non-smart oblique missions
      processed_pitch = mean(filtered_pitch_values, na.rm = TRUE)
    }
  } else {
    processed_pitch = NA  # Set processed_pitch to NA if quantiles are not valid
  }

  return(processed_pitch)
}

# Define a function to extract datetime and pitch values from EXIF data
extract_metadata <- function(exif) {
  # Convert DateTimeOriginal to datetime objects
  exif$DateTimeOriginal <- ymd_hms(gsub(":", " ", exif$DateTimeOriginal), tz = "UTC")

  # Calculate start and end datetime_local
  start_datetime_local <- min(exif$DateTimeOriginal)
  end_datetime_local <- max(exif$DateTimeOriginal)

  # Process pitch values
  camera_pitch_values = exif$CameraPitch
  camera_pitch_values = as.numeric(camera_pitch_values)  # Convert to numeric if necessary
  camera_pitch_derived = process_pitch_values(camera_pitch_values)

  # Convert start and end datetime_local to the desired format (YYYYMMDD HHMMSS)
  start_datetime_local_formatted <- format(start_datetime_local, "%Y%m%d %H%M%S")
  end_datetime_local_formatted <- format(end_datetime_local, "%Y%m%d %H%M%S")

  # Return the extracted values
  return(list(start_datetime = start_datetime_local_formatted,
              end_datetime = end_datetime_local_formatted,
              pitch = camera_pitch_derived))
}

# Process each EXIF file and extract metadata
extracted_metadata <- lapply(exif_files, function(exif_file) {
  exif <- prep_exif(exif_file)
  metadata <- extract_metadata(exif)
  metadata$file_name <- basename(exif_file)
  return(metadata)
})

# Combine the extracted metadata into a data frame
extracted_metadata_df <- do.call(rbind, extracted_metadata)

# Print the extracted metadata
print(extracted_metadata_df)
