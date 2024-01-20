# Functions to extract metadata from a drone imagery dataset, specifically using a data frame of
# EXIF data that has already been created by extracting EXIF data from all the images.

# Derek's metadata extraction code
extract_metadata_dy = function(exif_filepath) {

  # --- Prep ---
  # (These are steps that provide setup for extraction of multiple different metadata attributes,
  # but the actual extraction happens in the subsequent section(s).)

  # Read in the EXIF data from file for the first test dataset, as a data frame
  exif = read.csv(exif_filepath)

  # Get the dataset ID from the filename (knowing the naming convention is "exif_<dataset_id>.csv")
  dataset_id = stringr::str_extract(basename(exif_filepath), "(?<=exif_)(.*)(?=\\.csv)")
  # Alternatively, if we wanted to match the convention for the dataset_id, it would be "(([0-9]){8}-([0-9]){4})"

  # Convert the data frame into a geospatial 'sf' object (you could anternatively use a 'terra' 'vect'
  # object)
  exif = sf::st_as_sf(exif, crs = 4326, coords = c("GPSLongitude", "GPSLatitude"))

  # Convert the capture datetime to an R object. This uses the EXIF attribute 'DateTimeOriginal', but it
  # looks like there is another one called 'CreateDate' which has the same info. The lubridate parsing
  # function assumes the time zone is UTC, even when it's not (the EXIF attribute doesn't include TZ), but we will drop that later and
  # interpret this as local time.
  exif$capture_datetime = lubridate::ymd_hms(exif$DateTimeOriginal)

  # Arrange images by capture time (presumably they're already in capture order, but just to be sure).
  # First arrange by full file path (assuming that is the capture order), then by capture time. This
  # way, the capture time is used as top priority, with the file path used as a tiebreaker (e.g. if
  # there were two images taken in the same second). The file path where the drone saves the image
  # onto the SD card (at least for the DJI datasets tested) seems to be stored in EXIF attribute
  # 'ImageDescription'. We can't use the path alone because if the mission was split over two SD
  # cards, the file write path may have started over.
  # TODO: Deal with the case where a dataset was collected by two drones flying at once.
  exif = exif[order(exif$ImageDescription), ]
  exif = exif[order(exif$capture_datetime), ]

  # Plot the flight path as a visual check
  flightpath = exif |> dplyr::summarize(do_union = FALSE) |> sf::st_cast("LINESTRING")
  plot(flightpath)

  # --- Compute mean flight speed ---

  # Get distance from each image to the next, in meters.
  start_image = exif[1:(nrow(exif) - 1), ]
  end_image = exif[2:nrow(exif), ]
  distance = sf::st_distance(start_image, end_image, by_element = TRUE)

  # Get time from each image to the next, in seconds.
  interval = end_image$capture_datetime - start_image$capture_datetime

  # Compute speed in meters per second
  speed = as.numeric(distance) / as.numeric(interval)

  # Compute the median speed as the value to report, to avoid influence of outliers (such as battery swaps)
  speed = median(speed, na.rm = TRUE)


  # --- Return extracted/computed metadata as a data frame row ---
  metadata = data.frame(dataset_id = dataset_id,
                        flight_speed_derived = speed
                        # Add more metadata variables here
                        )

  return(metadata)

}