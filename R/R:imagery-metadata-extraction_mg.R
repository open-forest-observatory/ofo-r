# Functions to extract metadata from a drone imagery dataset, specifically using a data frame of
# EXIF data that has already been created by extracting EXIF data from all the images. Note that the
# last function defined in this script is a function that wraps all the individual extraction
# functions to extract all the metadata attributes at once.

# Flight speed
extract_flight_speed = function(exif) {

  # Get distance from each image to the next, in meters
  start_image = exif[1:(nrow(exif) - 1), ]
  end_image = exif[2:nrow(exif), ]
  distance = sf::st_distance(start_image, end_image, by_element = TRUE)

  # Get time from each image to the next, in seconds
  interval = end_image$capture_datetime - start_image$capture_datetime

  # Compute speed in meters per second
  speed = as.numeric(distance) / as.numeric(interval)

  # Compute the median speed as the value to report, to avoid influence of outliers (such as battery
  # swaps)
  speed = median(speed, na.rm = TRUE)

  return(speed)

}

# Wrapper for Derek's metadata extraction functions. Preps the EXIF data for passing to the
# extraction functions, then calls all the individual extraction functions to extract the respecive attributes.
extract_metadata_dy = function(exif_filepath, plot_flightpath = FALSE) {

  # Prep the EXIF data for extraction of metadata attributes
  exif = prep_exif(exif_filepath, plot_flightpath = plot_flightpath)

  # Extract/compute metadata attributes
  flight_speed_derived = extract_flight_speed(exif)

  # Return extracted/computed metadata as a data frame row
  metadata = data.frame(dataset_id = exif$dataset_id[1],
                        flight_speed_derived = flight_speed_derived
                        # Add more metadata variables here
  )

  return(metadata)

}
