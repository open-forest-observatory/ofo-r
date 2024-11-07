# Prep the EXIF data for extraction of metadata attributes. Speficially, load the EXIF CSV file into
# a dataframe (or accept a dataframe directly), add a dataset_id attribute based on the filename, compute image capture datetime,
# order the images by capture time, and optionally plot the flight path for visual inspection,
# standardize column names across different drone models, and remove any rows with missing GPS data.
#' @export
prep_exif = function(exif_in, plot_flightpath = FALSE) {
  if ("data.frame" %in% class(exif_in)) {
    exif = exif_in
  } else {
    exif = read.csv(exif_filepath)
    exif = exif_in
  }

  # Standardize column names across different drone models
  candidate_pitch_cols = c("CameraPitch", "GimbalPitchDegree")

  exif = exif |>
    dplyr::mutate(CameraPitch = dplyr::coalesce(!!!dplyr::select(exif, dplyr::any_of(candidate_pitch_cols))))

  # Remove any rows with missing GPS data
  missing_gps_rows = is.na(exif$GPSLongitude) | is.na(exif$GPSLatitude)
  n_missing_gps_rows = sum(missing_gps_rows)
  if (n_missing_gps_rows > 0) {
    warning("Removing ", n_missing_gps_rows, " rows with missing GPS data from dataset", exif$dataset_id[1])
    exif = exif[!missing_gps_rows, ]
  }

  # Convert the data frame into a geospatial 'sf' object (you could alternatively use a
  # 'terra::vect' object)
  exif = sf::st_as_sf(exif, crs = 4326, coords = c("GPSLongitude", "GPSLatitude"))

  # Convert the capture datetime to an R 'lubridate' object. This uses the EXIF attribute
  # 'DateTimeOriginal', but it looks like there is another one called 'CreateDate' which has the
  # same info. The lubridate parsing function assumes the time zone is UTC when not specified (the
  # EXIF attribute doesn't include the TZ), but we will drop that later and interpret this as local
  # time.
  exif$capture_datetime = lubridate::ymd_hms(exif$DateTimeOriginal)

  # Arrange images by capture time (presumably they're already in capture order, but just to be
  # sure). First arrange by full file path (assuming that is the capture order), then by capture
  # time. This way, the capture time is used as top priority, with the file path used as a
  # tiebreaker (e.g. if there were two images taken in the same second). The file path where the
  # drone saves the image onto the SD card (at least for the DJI datasets tested) seems to be stored
  # in EXIF attribute 'ImageDescription'. We can't use the path alone because if the mission was
  # split over two SD cards, the file write path may have started over. TODO: Deal with the case
  # where a dataset was collected by two drones flying at once.
  exif = exif[order(exif$ImageDescription), ]
  exif = exif[order(exif$capture_datetime), ]

  if (plot_flightpath) {
    # Plot the flight path as a visual check
    flightpath = exif |>
      dplyr::summarize(do_union = FALSE) |>
      sf::st_cast("LINESTRING")
    plot(flightpath)
  }

  return(exif)
}

# Read in the EXIF data from a set of absolute image paths and drop the ThumbnailImage and
# PreviewImage attributes if they exist, and convert all cols to character
#' @export
read_exif_drop_thumbnails = function(image_paths) {
  exif = exifr::read_exif(image_paths)

  exif = exif |>
    # Remove thumbnail data
    dplyr::select(-dplyr::any_of(c("ThumbnailImage", "PreviewImage"))) |>
    # Convert all cols to character
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    # Make sure SourceFile is in R-friendly format
    dplyr::mutate(SourceFile = image_paths)
}
