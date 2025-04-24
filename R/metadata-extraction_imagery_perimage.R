# Image-level metadata extraction functions
# Originally written by Emily Marie Purvis, last updated March 27th 2024
# Since updated by others

# Image ID, from filename
#' @export
extract_image_id = function(exif) {
  image_id = tools::file_path_sans_ext(exif$FileName)

  return(image_id)
}

#' Extract local date and time of image collection
#'
#' Pulls the local date and time of image collection (Format: YYYYMMDD HHMMSS) to include in image-level metadata collation
#'
#' @param exif the exif metadata file
#'
#' @return local date and time of image collection
#'
#' @examples
#' extract_datatime_local(exif)
#'
#' @export
extract_datetime_local = function(exif, candidate_columns = c("capture_datetime", "GPSDateTime", "DateTimeOriginal")) {
  datetime_local = extract_candidate_columns(sf::st_drop_geometry(exif), candidate_columns)
  datetime_local = lubridate::ymd_hms(datetime_local)
  return(datetime_local)
}

#### lat and lon (Format: dd.dddddddd (EPSG:4326)) ####

#' Extract latitude and longitude of image collection location
#'
#' Pulls the latitude and longitude of image collection location (Format: dd.dddddddd) to include in image-level metadata collation
#'
#' @param exif the exif metadata file
#'
#' @return latitude and longitude of image collection location
#'
#' @examples
#' extract_lat_lon(exif)
#'
#' @export

extract_lon_lat = function(exif) {
  exif = sf::st_as_sf(exif, crs = 4326, coords = c("GPSLongitude", "GPSLatitude"))
  coords = sf::st_coordinates(exif)
  lon = coords[, 1] |> as.numeric()
  lat = coords[, 2] |> as.numeric()

  ret = list(lon = lon, lat = lat)

  return(ret)
}

#### rtk_fix (Format: True/False. Use EXIF RTKFlat) ####

#' Returns rtk_fix of image
#'
#' Pulls the rtk fix to include in image-level metadata collation. Return TRUE if there is a RtkFlag value of 50, and FALSE otherwise, i.e. other RtkFlag value, or none. IMPORTANT NOTE!!!!!! THIS FUNCTION ASSUMES DJI DRONES WERE USED. OTHER DRONE MAKES WILL REQUIRE AN UPDATED FUNCTION.
#'
#' @param exif the exif metadata file
#'
#' @return rtk fix of image, true/false
#'
#' @examples
#' extract_rtk_fix(exif)
#'
#' @export
extract_rtk_fix = function(exif) {
  if ("RtkFlag" %in% names(exif)) {
    rtk_flag = exif$RtkFlag
    # Since this may contain NA, replace them with zeros
    rtk_flag = tidyr::replace_na(rtk_flag, 0)
    # Compute whether it has the "fix" flag
    rtk_fix = rtk_flag == 50
    return(rtk_fix)
  } else {
    rtk_fix = rep(FALSE, nrow(exif))
    return(rtk_fix)
  }
}

#### accuracy (Units: m) ####

#' Returns accuracy of latitude and longitude. Standard deviation. Units: meters.
#'
#' DJI EXIF files have an RTK standard longitude deviation (RtkStdLon, the standard deviation (in meters) of the photo recording position in longitude direction) and an RTK standard latitude deviation (RtkStdLat, the standard deviation (in meters) of the photo recording position in latitude direction). This function pulls those three values to include in image-level metadata collation.
#'
#' @param exif the exif metadata file
#'
#' @return accuracy of latitude and longitude of each image. Standard deviation. Units: meters.
#'
#' @examples
#' extract_accuracy(exif)
#'
#' @export

extract_accuracy = function(exif) {
  exif = sf::st_drop_geometry(exif)
  # This field may not be actually present in the data but only introduced by
  # row_bind-ing across multiple platforms
  if ("GPSXYAccuracy" %in% colnames(exif) && !all(is.na(exif$GPSXYAccuracy))) {
    accuracy_x = exif$GPSXYAccuracy
    accuracy_y = exif$GPSXYAccuracy
  } else {
    # Try extracting the deviation from the RTK field
    # TODO should we instead prioritize the RTK value if both are present?
    exif["RtkStdLon"[!("RtkStdLon" %in% colnames(exif))]] = NA
    exif["RtkStdLat"[!("RtkStdLat" %in% colnames(exif))]] = NA
    accuracy_x = exif$RtkStdLon
    accuracy_y = exif$RtkStdLat
  }

  accuracy_x = round(accuracy_x, 4)
  accuracy_y = round(accuracy_y, 4)

  ret = list(
    accuracy_x = accuracy_x,
    accuracy_y = accuracy_y
  )

  return(ret)
}

extract_image_shape = function(exif, image_width_candidate_columns = c("ImageWidth"), image_height_candidate_columns = c("ImageHeight")) {
  image_width = extract_candidate_columns(exif, image_width_candidate_columns)
  image_height = extract_candidate_columns(exif, image_height_candidate_columns)

  ret_list = list(image_width = image_width, image_height = image_height)

  return(ret_list)
}

extract_file_format = function(exif) {
  # Convert into a character array
  file_format = extract_candidate_columns(exif, c("FileTypeExtension"))

  return(file_format)
}

extract_file_size = function(exif) {
  file_size_gb = exif$FileSize / 1000000000
  return(file_size_gb)
}

#### pitch_roll_yaw: camera_pitch (Units: deg, degrees up from nadir), camera_roll (Units: deg, degrees clockwise from up), camera_yaw (Units: deg, degrees right from true north) ####

#' Returns camera pitch, camera roll, and camera yaw. Units: degrees.
#'
#' Returns camera_pitch (Units: deg, degrees up from nadir), camera_roll (Units: deg, degrees clockwise from up), and camera_yaw (Units: deg, degrees right from true north) to include in image-level metadata collation.
#'
#' @param exif the exif metadata file
#'
#' @return camera pitch, camera roll, and camera yaw. Units: degrees.
#'
#' @examples
#' extract_pitch_roll_yaw(exif)
#'
#' @export

extract_pitch_roll_yaw = function(exif,
                                  platform_name,
                                  candidate_pitch_cols = c("GimbalPitchDegree", "Pitch"),
                                  candidate_roll_cols = c("GimbalRollDegree", "Roll"),
                                  candidate_yaw_cols = c("GimbalYawDegree", "Yaw")) {
  # Remove the geometry column since it can cause issues
  exif = sf::st_drop_geometry(exif)
  camera_pitch = extract_candidate_columns(exif, candidate_pitch_cols)
  camera_roll = extract_candidate_columns(exif, candidate_roll_cols)
  camera_yaw = extract_candidate_columns(exif, candidate_yaw_cols)

  # DJI platforms use an alternative pitch convention so we need shift it so it reflects degrees
  # up from nadir
  dji_platforms = c(
    "Phantom 4 Pro v2.0",
    "Phantom 4 Advanced",
    "Mavic 3 Multispectral",
    "Phantom 4 RTK",
    "Phantom 4 Standard",
    "Matrice 210 RTK",
    "Matrice 100",
    "Matrice 300"
  )

  camera_pitch = dplyr::case_when(
    platform_name %in% dji_platforms ~ camera_pitch + 90,
    # else
    TRUE ~ camera_pitch
  )

  camera_pitch = round(camera_pitch, 2)
  camera_roll = round(camera_roll, 2)
  camera_yaw = round(camera_yaw, 2)

  pitch_roll_yaw = list(
    camera_pitch = camera_pitch,
    camera_roll = camera_roll,
    camera_yaw = camera_yaw
  )

  return(pitch_roll_yaw)
}

#### exposure (Units: sec) ####

#' Returns exposure (units: seconds)
#'
#' Returns exposure time in seconds to include in image-level metadata collation.
#'
#' @param exif the exif metadata file
#'
#' @return exposure (units: seconds)
#'
#' @examples
#' extract_exposure(exif)
#'
#' @export
extract_exposure = function(exif) {
  exposure = exif$ExposureTime |> round(6)
  return(exposure)
}

#### aperture (Format: xxxxx) ####

#' Returns aperture (format: xxxxx)
#'
#' Returns aperture to include in image-level metadata collation.
#'
#' @param exif the exif metadata file
#'
#' @return aperture (format: xxxxx)
#'
#' @examples
#' extract_aperture(exif)
#'
#' @export
extract_aperture = function(exif) {
  aperture = (exif$Aperture)
  return(aperture)
}

#### iso ####

#' Returns ISO
#'
#' Returns ISO to include in image-level metadata collation.
#'
#' @param exif the exif metadata file
#'
#' @return ISO
#'
#' @examples
#' extract_iso(exif)
#'
#' @export
extract_iso = function(exif) {
  iso = exif$ISO
  return(iso)
}

#### white_balance (Format: auto/manual) ####

#' Returns white balance, either auto or manual
#'
#' Returns white balance (auto vs. manual) to include in image-level metadata collation.
#'
#' @param exif the exif metadata file
#'
#' @return white balance
#'
#' @examples
#' extract_white_balance(exif)
#'
#' @export
extract_white_balance = function(exif) {
  white_balance = dplyr::case_when(
    exif$WhiteBalance == 0 ~ "auto",
    exif$WhiteBalance == 1 ~ "manual"
  )

  return(white_balance)
}

#### altitude: returns altitude above sea level (asl) in meters ####

#' Returns altitude above sea level (asl) in meters
#'
#' Returns altitude above sea level (asl) in meters to include in image-level metadata collation.
#'
#' @param exif the exif metadata file
#'
#' @return altitude above sea level in meters
#'
#' @examples
#' extract_altitude_asl(exif)
#'
#' @export
extract_altitude_asl = function(exif, candidate_asl_columns = c("AbsoluteAltitude", "GPSAltitude")) {
  altitude_asl = extract_candidate_columns(sf::st_drop_geometry(exif), candidate_asl_columns)
  altitude_asl = round(altitude_asl)

  return(altitude_asl)
}

# TODO document
#' @export
extract_original_file_name = function(exif, candidate_file_name_columns = c("ImageDescription", "FileName")) {
  original_file_names = extract_candidate_columns(sf::st_drop_geometry(exif), candidate_file_name_columns)

  return(original_file_names)
}

#' Extract image-level metadata parameters from EXIF datafram
#'
#' @param exif_file the exif filepath (before being prepared to pass to the functions in the
#' wrapper)
#' @param input_type the type of input, either "dataframe" or "filepath" to a .csv file
#'
#' @return a data.frame of datetime_local, lat, lon, rtk_fix, accuracy_x, accuracy_y, camera_pitch, camera_roll, camera_yaw, exposure, aperture, iso, white_balance, and altitude_asl
#'
#' @examples
#' extract_metadata_emp(exif)
#'
#' @export
extract_imagery_perimage_metadata = function(exif, platform_name, plot_flightpath = FALSE) {

  # Note to devs: This is designed to be vectorized, where the user can supply either 1 value of
  # platform_name that will be used for all images, or a vector of platform names that is the same
  # length as the exif data (i.e. one platform name per image).

  # Check if the platform is supported
  supported_platforms = c(
    "Phantom 4 Pro v2.0",
    "Phantom 4 Advanced",
    "Mavic 3 Multispectral",
    "Phantom 4 RTK",
    "Phantom 4 Standard",
    "Matrice 210 RTK",
    "Matrice 100",
    "Matrice 300",
    "eBee X",
    "Evo II v2"
  )

  if (any(!(platform_name %in% supported_platforms))) {

    # find unsupported platforms by setdiff
    unsupported_platforms = setdiff(platform_name, supported_platforms)

    stop(paste("Platform(s) ", paste(unsupported_platforms, collapse = ", "), " not supported"))
  }

  # In the future, some of these parsing steps might be dependent on the platform but for now it's
  # not required. For example, a given attribute may require different default column names to
  # inspect dependent on what platform generated the metadata. If platform-specific logic needs to
  # be implemented, it could be done in a variety of ways. One option (David's current
  # recommendation) is to defer any platform-specific considerations to the individual functions.
  # For example, the `extract_image_id` function could be passed both the exif data and the platform
  # and then it would handle the logic of properly extracting the standardized data. Alternatively,
  # we could have a function that extracts a parameter for a group of platforms, such as one
  # function for extracting the image_id for all DJI platforms and another for all eBee platfroms.
  # Then, the appropriate parsing function would be called in this function, based on the platform.
  # The latter logic would need a little more thought to allow it to be vectorizable (or maybe we
  # will determine that vectorization is not important -- but in that case, will need to update the
  # calling scripts because they are set up in the vectorized way -- but would not be hard to change
  # that).
  image_id = extract_image_id(exif)
  original_file_name = extract_original_file_name(exif)
  datetime_local = extract_datetime_local(exif)
  lon_lat = extract_lon_lat(exif)
  rtk_fix = extract_rtk_fix(exif)
  accuracy = extract_accuracy(exif)
  pitch_roll_yaw = extract_pitch_roll_yaw(exif, platform_name)
  exposure = extract_exposure(exif)
  aperture = extract_aperture(exif)
  iso = extract_iso(exif)
  white_balance = extract_white_balance(exif)
  altitude_asl_drone = extract_altitude_asl(exif)
  image_shape = extract_image_shape(exif)
  file_format = extract_file_format(exif)
  file_size_gb = extract_file_size(exif)

  metadata = data.frame(
    image_id = image_id,
    original_file_name = original_file_name,
    datetime_local = datetime_local,
    lon_lat,
    rtk_fix = rtk_fix,
    accuracy,
    pitch_roll_yaw,
    exposure = exposure,
    aperture = aperture,
    iso = iso,
    white_balance = white_balance,
    altitude_asl_drone = altitude_asl_drone,
    image_shape,
    file_format = file_format,
    file_size_gb = file_size_gb
  )

  # Remove any rows with missing GPS data
  missing_gps_rows = is.na(metadata$lat) | is.na(metadata$lon)
  n_missing_gps_rows = sum(missing_gps_rows)
  if (n_missing_gps_rows > 0) {
    warning("Removing ", n_missing_gps_rows, " rows with missing GPS data from dataset", exif$sub_mission_id[1])

    # Keep only rows with GPS data
    metadata = metadata[!missing_gps_rows, ]
  }

  # Arrange images by capture time (presumably they're already in capture order, but just to be
  # sure). First arrange by full file path (assuming that is the capture order), then by capture
  # time. This way, the capture time is used as top priority, with the file path used as a
  # tiebreaker (e.g. if there were two images taken in the same second). We can't use the path alone
  # because if the mission was split over two SD cards, the file write path may have started over.
  # TODO: Deal with the case where a dataset was collected by two drones flying at once.
  metadata = metadata[order(metadata$original_file_name), ]
  metadata = metadata[order(metadata$datetime_local), ]

  # Plot the flight path as a visual check if requested
  if (plot_flightpath) {
    # TODO figure out if this is the best way to visualize
    # This has the issue of opening a bunch of windows
    x11()
    flightpath = sf::st_as_sf(metadata, crs = 4326, coords = c("lon", "lat"))

    flightpath = flightpath |>
      dplyr::summarize(do_union = FALSE) |>
      sf::st_cast("LINESTRING")
    plot(flightpath)
  }

  return(metadata)
}


extract_candidate_columns = function(dataframe, candidate_columns) {
  selected_columns = dplyr::select(dataframe, dplyr::any_of(candidate_columns))
  if (ncol(selected_columns) == 0) {
    print("Will fail to extract candidate columns")
    print(candidate_columns)
    print(names(dataframe))
  }
  extracted_column = dplyr::coalesce(!!!selected_columns)
  return(extracted_column)
}
