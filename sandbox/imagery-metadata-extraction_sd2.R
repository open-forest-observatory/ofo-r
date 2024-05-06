# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Load necessary libraries
library(lubridate)
library(sf)

# Define the root of the local data directory
datadir <- readLines(file.path("sandbox", "data-dirs", "steven-metadata-laptop.txt"))

# Define a function to process pitch values
process_pitch_values <- function(exif) {
  # Extract CameraPitch directly from exif and adjust pitch values
  camera_pitch_values <- as.numeric(exif$CameraPitch) + 90  # Adjust pitch values
  adjusted_pitch_values <- camera_pitch_values  # Adjusted pitch values to represent 0 as nadir (down)

  # Compute 0.1, 0.5, and 0.9 quantiles
  quantiles <- quantile(adjusted_pitch_values, c(0.1, 0.5, 0.9), na.rm = TRUE)

  # Check if quantiles contains valid values
  if (!any(is.na(quantiles))) {
    # Check if the range between 0.1 and 0.9 quantiles is at least 10 degrees
    pitch_range <- quantiles[3] - quantiles[1]
    if (pitch_range >= 10) {
      # Use 0.1 or 0.9 quantile directly as the mission pitch
      processed_pitch <- quantiles[1]
    } else {
      # Compute the average of absolute pitch values for non-smart oblique missions
      processed_pitch <- mean(abs(c(quantiles[1], quantiles[3])), na.rm = TRUE)
    }
  } else {
    processed_pitch <- NA  # Set processed_pitch to NA if quantiles are not valid
  }

  return(processed_pitch)
}

# Define a function to process datetime values
process_datetime_values <- function(exif) {
  # Convert DateTimeOriginal to datetime objects
  exif$DateTimeOriginal <- ymd_hms(gsub(":", " ", exif$DateTimeOriginal), tz = "UTC")

  # Calculate start and end datetime_local
  start_datetime_local <- min(exif$DateTimeOriginal)
  end_datetime_local <- max(exif$DateTimeOriginal)

  # Convert start and end datetime_local to the desired format (YYYYMMDD HHMMSS)
  start_datetime_local_formatted <- format(start_datetime_local, "%Y%m%d %H%M%S")
  end_datetime_local_formatted <- format(end_datetime_local, "%Y%m%d %H%M%S")

  return(list(start_datetime = start_datetime_local_formatted,
              end_datetime = end_datetime_local_formatted))
}

# Define a function to extract datetime and pitch values from EXIF data
extract_metadata <- function(exif) {
  # Process datetime values
  datetime_values <- process_datetime_values(exif)

  # Process pitch values
  camera_pitch_derived <- process_pitch_values(exif)  # Use the modified process_pitch_values function

  # Return the extracted values
  return(c(datetime_values, pitch = camera_pitch_derived))
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

# User: I've updated the code based on your feedback. Please review and let me know if everything looks good or if there are any further adjustments needed.

