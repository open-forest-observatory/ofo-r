# This script defines image-level metadata extraction functions
# Written by Emily Marie Purvis, March 4th 2024

#### Step 1: Define image-level metadata extraction functions ####

#### dataset ID ####

dataset_id = exif$dataset_id

extract_dataset_id = function (exif) {

  dataset_id = exif$dataset_id

  return(dataset_id)

}

#### datatime_local (Format: YYYYMMDD HHMMSS (local time zone, 24 hr)) ####

exif$DateTimeOriginal_nocolon = stringr::str_replace_all(exif$DateTimeOriginal, ":", "")

datatime_local = (exif$DateTimeOriginal_nocolon)

extract_datatime_local = function (exif) {

  exif$DateTimeOriginal_nocolon = stringr::str_replace_all(exif$DateTimeOriginal, ":", "")

  datatime_local = (exif$DateTimeOriginal_nocolon)

  return(datatime_local)

}

#### lat and lon (Format: dd.dddddddd (EPSG:4326)) ####

exif_coordinates <- data.frame(exif$X, sf::st_coordinates(exif[,1], st_coordinates(exif[,2])))

lat = exif_coordinates$Y

lon = exif_coordinates$X

extract_lat_lon = function (exif) {

  exif_coordinates <- data.frame(exif$X, sf::st_coordinates(exif[,1], st_coordinates(exif[,2])))

  lat = exif_coordinates$Y

  lon = exif_coordinates$X

  lat_lon = data.frame (lat, lon)

  return(lat_lon)
}

#### rtk_fix (Format: True/False. Use EXIF RTKFlat) ####

rtk_fix = {
  if ("RtkFlag" %in% names(exif)) {rtk_fix = exif$RtkFlag == 50}
  else {rtk_fix = rep(FALSE, nrow(exif))}
}

extract_rtk_fix = function(exif) {
  if ("RtkFlag" %in% names(exif)) {
    rtk_fix = exif$RtkFlag == 50
    return(rtk_fix)
  }
  else {
    rtk_fix = rep(FALSE, nrow(exif))
    return(rtk_fix)
  }
}

#### accuracy_x (Units: m rmse) ####

accuracy_x = exif$RtkStdLon

accuracy_y = exif$RtkStdLat

accuracy_z = exif$RtkStdHgt


extract_accuracy = function (exif) {

  exif["RtkStdLon"[!("RtkStdLon" %in% colnames(exif))]] = NA

  accuracy_x = exif$RtkStdLon

  exif["RtkStdLat"[!("RtkStdLat" %in% colnames(exif))]] = NA

  accuracy_y = exif$RtkStdLat

  exif["RtkStdHgt"[!("RtkStdHgt" %in% colnames(exif))]] = NA

  accuracy_z = exif$RtkStdHgt

  accuracy = data.frame (accuracy_x, accuracy_y, accuracy_z)

  return(accuracy)
}

#### pitch_roll_yaw: camera_pitch (Units: deg, degrees up from nadir), camera_roll (Units: deg, degrees clockwise from up), camera_yaw (Units: deg, degrees right from true north) ####

camera_pitch = exif$GimbalPitchDegree + 90

camera_roll = exif$GimbalRollDegree

camera_yaw = exif$GimbalYawDegree

extract_pitch_roll_yaw = function(exif) {

  camera_pitch = exif$GimbalPitchDegree + 90

  camera_roll = exif$GimbalRollDegree

  camera_yaw = exif$GimbalYawDegree

  pitch_roll_yaw = data.frame (camera_pitch, camera_roll, camera_yaw)

  return(pitch_roll_yaw)
}

#### exposure (Units: sec) ####

exposure = exif$ExposureTime

extract_exposure = function (exif) {
  exposure = exif$ExposureTime
  return(exposure)
}

#### aperture (Format: xxxxx) ####

aperture = (exif$Aperture)

extract_aperture = function (exif) {
  aperture = (exif$Aperture)
  return(aperture)
}

#### iso ####

iso = exif$ISO

extract_iso = function(exif) {
  iso = exif$ISO
  return(iso)
}

#### white_balance (Format: auto/sunny/cloudy/(others?)) ####

white_balance = dplyr::case_when(
  exif$WhiteBalance == 0 ~ "auto",
  exif$WhiteBalance == 1 ~ "manual"
)

extract_white_balance = function(exif) {

  white_balance = dplyr::case_when(
    exif$WhiteBalance == 0 ~ "auto",
    exif$WhiteBalance == 1 ~ "manual"
  )

  return(white_balance)
}

#### received_image_path (Image path in as-received dataset, with the top level being the folder named with the dataset ID) ####

received_image_path = stringr::str_split_fixed(exif$SourceFile, fixed(dataset_id), 2)

received_image_path <- received_image_path[,2]

received_image_path <- with(exif, paste0(dataset_id, received_image_path))

extract_received_image_path = function(exif) {

  received_image_path = stringr::str_split_fixed(exif$SourceFile, fixed(dataset_id), 2)

  received_image_path <- received_image_path[,2]

  received_image_path <- with(exif, paste0(dataset_id, received_image_path))

  return(received_image_path)
}

#### altitude: returns altitude above sea level (asl) in meters ####

altitude_asl = exif$AbsoluteAltitude

extract_altitude = function(exif) {

  altitude_asl = exif$AbsoluteAltitude

  altitude = data.frame (altitude_asl)

  return(altitude)
}

#### Step 2: Create wrapper for metadata extraction functions. Preps the EXIF data for passing to the extraction functions, then calls all the individual extraction functions to extract the respective attributes. ####

extract_metadata_emp = function(exif_filepath) {

  exif = prep_exif(exif_filepath)

  dataset_id = extract_dataset_id (exif)
  datatime_local = extract_datatime_local(exif)
  lat_lon = extract_lat_lon(exif)
  rtk_fix = extract_rtk_fix(exif)
  accuracy = extract_accuracy(exif)
  pitch_roll_yaw = extract_pitch_roll_yaw(exif)
  exposure = extract_exposure(exif)
  aperture = extract_aperture (exif)
  iso = extract_iso(exif)
  white_balance = extract_white_balance(exif)
  received_image_path = extract_received_image_path(exif)
  altitude = extract_altitude(exif)

  metadata = data.frame(dataset_id = dataset_id,
                        datatime_local = datatime_local,
                        lat_lon,
                        rtk_fix = rtk_fix,
                        accuracy,
                        pitch_roll_yaw,
                        exposure = exposure,
                        aperture = aperture,
                        iso = iso,
                        white_balance = white_balance,
                        received_image_path = received_image_path,
                        altitude
  )

  return(metadata)

}
