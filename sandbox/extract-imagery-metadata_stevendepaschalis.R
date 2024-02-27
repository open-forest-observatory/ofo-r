# This script has two purposes: 1. Demonstrate how to run functions to extract metadata from imagery
# dataset EXIF data, and 2. Demonstrate how to write a new metadata extraction function.

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "steven-metadata-laptop.txt"))

exif_file = exif_files[1]
exif = prep_exif(exif_file)

# Load necessary packages
library(sf)

# Define the function to extract metadata including resolution, aspect ratio, image file format, and mean image frequency
extract_metadata = function(exif) {
  # Check if DateTimeOriginal is present
  if (!"DateTimeOriginal" %in% colnames(exif)) {
    stop("DateTimeOriginal attribute is not present in the data.")
  }

  # Check for missing or NA values in DateTimeOriginal
  if (any(is.na(exif$DateTimeOriginal))) {
    stop("DateTimeOriginal contains NA values.")
  }

  # Convert DateTimeOriginal to datetime object
  exif$DateTimeOriginal <- as.POSIXct(exif$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")

  # Identify invalid datetime values
  invalid_datetime <- is.na(exif$DateTimeOriginal)

  # Print out invalid datetime values
  if (any(invalid_datetime)) {
    print("Invalid datetime values detected:")
    print(exif$DateTimeOriginal[invalid_datetime])
    stop("Please correct the invalid datetime values.")
  }

  # Calculate time difference between consecutive images
  time_diff <- diff(exif$DateTimeOriginal)

  # Calculate time difference in seconds
  time_diff_seconds <- as.numeric(time_diff, units = "secs")

  # Remove NA and infinity values
  time_diff_seconds <- time_diff_seconds[is.finite(time_diff_seconds)]

  # Calculate image frequency (images per second)
  image_frequency <- 1 / time_diff_seconds

  # Check if there are any non-finite image frequencies
  if (any(!is.finite(image_frequency))) {
    stop("Invalid image frequencies detected.")
  }

  # Calculate mean image frequency
  mean_image_frequency <- mean(image_frequency, na.rm = TRUE)

  # Get Xresolution and Yresolution from the EXIF data
  resolution_x <- unique(exif$Xresolution)
  resolution_y <- unique(exif$Yresolution)

  # Get aspect ratio
  aspect_ratio <- unique(exif$imagewidth / exif$imageheight)

  # Get image file format
  image_file_format <- unique(exif$FileType)

  # Check if resolution attributes are identical across all images
  if (length(resolution_x) > 1 | length(resolution_y) > 1) {
    # If not identical, compute average
    avg_resolution_x <- mean(resolution_x)
    avg_resolution_y <- mean(resolution_y)

    # Record a warning
    warning("Resolution attributes are not identical across all images. Average resolution computed.")
  } else {
    # If identical, use the single values
    avg_resolution_x <- resolution_x
    avg_resolution_y <- resolution_y
  }

  # Check if image file format is consistent
  if (length(image_file_format) > 1) {
    # If not identical, return a warning
    warning("Image file format varies across images.")
  }

  # Return metadata
  return(list(resolution_x = avg_resolution_x,
              resolution_y = avg_resolution_y,
              aspect_ratio = aspect_ratio,
              image_file_format = image_file_format,
              mean_image_frequency = mean_image_frequency))
}

# Test the function
metadata <- extract_metadata(exif)
metadata

