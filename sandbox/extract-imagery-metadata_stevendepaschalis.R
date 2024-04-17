# Load all the functions (and package dependencies) of this R package
devtools::load_all()
library(sf)

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "steven-metadata-laptop.txt"))

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
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

# Define the overall function to extract metadata
extract_metadata_sd <- function(exif) {
  # Extract image frequency
  mean_image_frequency <- extract_image_frequency(exif)

  # Extract resolution and aspect ratio
  resolution_aspect_ratio <- extract_resolution_and_aspect_ratio(exif)

  # Extract image file type
  image_file_format <- extract_file_type(exif)

  # Return metadata
  return(list(resolution_x = resolution_aspect_ratio$resolution_x,
              resolution_y = resolution_aspect_ratio$resolution_y,
              aspect_ratio = resolution_aspect_ratio$aspect_ratio,
              image_file_format = image_file_format,
              mean_image_frequency = mean_image_frequency))
}

# Test the function
metadata <- extract_metadata_sd(exif)
metadata
