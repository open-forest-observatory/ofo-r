# Load necessary packages
library(sf)
library(lubridate)

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files <- list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
exif_file <- exif_files[1]
exif <- prep_exif(exif_file)

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "steven-metadata-laptop.txt"))

# Get a list of the files containing the test EXIF data (one file per image dataset)
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
exif_file = exif_files[1]
exif = prep_exif(exif_file)

# Define the function to extract image frequency
extract_image_frequency <- function(exif) {
  # Convert DateTimeOriginal to datetime object
  exif$DateTimeOriginal <- as.POSIXct(exif$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")

  # Calculate time difference between consecutive images
  time_diff <- diff(exif$DateTimeOriginal)

  # Calculate time difference in seconds
  time_diff_seconds <- as.numeric(time_diff, units = "secs")

  # Remove NA and infinity values
  time_diff_seconds <- time_diff_seconds[is.finite(time_diff_seconds)]

  # Calculate median time difference
  median_time_diff <- median(time_diff_seconds)

  # Calculate 10th and 90th percentiles for outlier detection
  time_diff_10th <- quantile(time_diff_seconds, 0.1)
  time_diff_90th <- quantile(time_diff_seconds, 0.9)

  # Exclude outliers based on quantiles
  filtered_time_diff <- time_diff_seconds[time_diff_seconds >= time_diff_10th & time_diff_seconds <= time_diff_90th]

  # Calculate image frequency (images per second) from filtered values
  image_frequency <- 1 / filtered_time_diff

  # Calculate mean image frequency from filtered values
  mean_image_frequency <- mean(image_frequency, na.rm = TRUE)

  return(mean_image_frequency)
}

# Define the function to extract resolution and aspect ratio
extract_resolution_and_aspect_ratio <- function(exif) {
  # Get Xresolution and Yresolution from the EXIF data
  resolution_x <- unique(exif$XResolution)
  resolution_y <- unique(exif$YResolution)

  # Calculate mode resolution
  mode_resolution_x <- as.numeric(names(sort(table(resolution_x), decreasing = TRUE)[1]))
  mode_resolution_y <- as.numeric(names(sort(table(resolution_y), decreasing = TRUE)[1]))

  # Get aspect ratio
  aspect_ratio <- mode_resolution_x / mode_resolution_y

  return(list(resolution_x = mode_resolution_x,
              resolution_y = mode_resolution_y,
              aspect_ratio = aspect_ratio))
}

# Define the function to extract image file type
extract_file_type <- function(exif) {
  # Get image file format
  image_file_format <- unique(exif$FileType)

  # Check if image file format is consistent
  if (length(unique(image_file_format)) > 1) {
    # If not identical, return a warning
    warning("Image file format varies across images.")
  }

  # Calculate mode file format
  mode_image_file_format <- names(sort(table(image_file_format), decreasing = TRUE)[1])

  return(mode_image_file_format)
}

# Define a function to process pitch values
extract_pitch <- function(exif) {
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
      # Infer the mission is a smart-oblique mission
      # Use 0.1 or 0.9 quantile directly as the mission pitch
      processed_pitch <- quantiles[1]
    } else {
      # Infer the mission is not a smart-oblique mission
      # Compute the average of absolute pitch values for non-smart oblique missions
      processed_pitch <- median(adjusted_pitch_values)
    }
  } else {
    processed_pitch <- NA  # Set processed_pitch to NA if quantiles are not valid
  }

  return(processed_pitch)
}

# Define a function to process datetime values
extract_start_end_datetime <- function(exif) {
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
  datetime_values <- extract_start_end_datetime(exif)

  # Process pitch values
  camera_pitch_derived <- extract_pitch(exif)  # Use the modified process_pitch_values function

  # Return the extracted values
  return(c(datetime_values, pitch = camera_pitch_derived))
}

# Define the overall function to extract metadata
extract_metadata_sd <- function(exif_file_path) {
  exif <- prep_exif(exif_file_path)
  image_frequency <- extract_image_frequency(exif)
  resolution_aspect <- extract_resolution_and_aspect_ratio(exif)
  file_type <- extract_file_type(exif)
  start_end_datetime <- extract_start_end_datetime(exif)
  pitch <- extract_pitch(exif)
  extracted_metadata <- list(
    image_frequency = image_frequency,
    resolution_aspect = resolution_aspect,
    file_type = file_type,
    start_end_datetime = start_end_datetime,
    pitch = pitch
  )}

