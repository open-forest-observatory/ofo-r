# Image-level metadata extraction functions
# Originally written by Emily Marie Purvis, last updated March 27th 2024
# Since updated by others

#### dataset ID ####

#' Extract dataset id
#'
#' Pulls the dataset id, to include in image-level metadata collation
#'
#' @param exif the exif metadata file
#'
#' @return dataset id
#'
#' @examples
#' extract_dataset_id(exif)
#'
#' @export
extract_dataset_id_perimage = function(exif) {
  dataset_id_image_level = exif$dataset_id

  return(dataset_id_image_level)
}

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
  datetime_local = extract_candidate_columns(st_drop_geometry(exif), candidate_columns)
  datetime_local = lubridate::ymd_hms(datetime_local)
  print(datetime_local)
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
    rtk_fix = exif$RtkFlag == 50
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
  if ("GPSXYAccuracy" %in% colnames(exif)) {
    accuracy_x = exif$GPSXYAccuracy
    accuracy_y = exif$GPSXYAccuracy
  } else {
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
                                  candidate_pitch_cols = c("GimbalPitchDegree", "Pitch"),
                                  candidate_roll_cols = c("GimbalRollDegree", "Roll"),
                                  candidate_yaw_cols = c("GimbalYawDegree", "Yaw")) {
  # Remove the geometry column since it can cause issues
  exif = sf::st_drop_geometry(exif)
  camera_pitch = extract_candidate_columns(exif, candidate_pitch_cols)
  camera_roll = extract_candidate_columns(exif, candidate_roll_cols)
  camera_yaw = extract_candidate_columns(exif, candidate_yaw_cols)

  # TODO should this be done in all cases?
  camera_pitch = camera_pitch + 90

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

#### received_image_path (Image path in as-received dataset, with the top level being the folder named with the dataset ID) ####

#' Returns image path
#'
#' Returns path in as-received dataset, with the top level being the folder named with the dataset ID, to include in image-level metadata collation.
#'
#' @param exif the exif metadata file
#'
#' @return received image path
#'
#' @examples
#' extract_received_image_path(exif)
#'
#' @export

extract_received_image_path = function(exif) {
  received_image_path = stringr::str_split_fixed(exif$SourceFile, stringr::fixed(exif$dataset_id), 2)

  received_image_path <- received_image_path[, 2]

  received_image_path <- paste0(exif$dataset_id, received_image_path)

  return(received_image_path)
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
#' @return a data.frame of dataset_id_image_level, datetime_local, lat, lon, rtk_fix, accuracy_x, accuracy_y, camera_pitch, camera_roll, camera_yaw, exposure, aperture, iso, white_balance, received_image_path, and altitude_asl
#'
#' @examples
#' extract_metadata_emp(exif)
#'
#' @export
extract_imagery_perimage_metadata_DJI = function(exif) {
  image_id = extract_image_id(exif)
  dataset_id_image_level = extract_dataset_id_perimage(exif)
  datetime_local = extract_datetime_local(exif)
  lon_lat = extract_lon_lat(exif)
  rtk_fix = extract_rtk_fix(exif)
  accuracy = extract_accuracy(exif)
  pitch_roll_yaw = extract_pitch_roll_yaw(exif)
  exposure = extract_exposure(exif)
  aperture = extract_aperture(exif)
  iso = extract_iso(exif)
  white_balance = extract_white_balance(exif)
  received_image_path = extract_received_image_path(exif)
  altitude_asl_drone = extract_altitude_asl(exif)

  metadata = data.frame(
    image_id = image_id,
    dataset_id_image_level = dataset_id_image_level,
    datetime_local = datetime_local,
    lon_lat,
    rtk_fix = rtk_fix,
    accuracy,
    pitch_roll_yaw,
    exposure = exposure,
    aperture = aperture,
    iso = iso,
    white_balance = white_balance,
    received_image_path = received_image_path,
    altitude_asl_drone = altitude_asl_drone
  )

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
