# This script defines image-level metadata extraction functions
# Written by Emily Marie Purvis, March 4th 2024

#### Step 1: Define image-level metadata extraction functions ####

#### dataset ID ####

# BEGIN FUNCTION CODE

dataset_id = exif$dataset_id

# END FUNCTION CODE

# turn code into a function

extract_dataset_id = function (exif) {

  dataset_id = exif$dataset_id

  return(dataset_id)

}

#### datatime_local (Format: YYYYMMDD HHMMSS (local time zone, 24 hr)) ####

# BEGIN FUNCTION CODE

exif$DateTimeOriginal_nocolon = stringr::str_replace_all(exif$DateTimeOriginal, ":", "")

datatime_local = (exif$DateTimeOriginal_nocolon)

# END FUNCTION CODE

# turn code into a function

extract_datatime_local = function (exif) {

  exif$DateTimeOriginal_nocolon = stringr::str_replace_all(exif$DateTimeOriginal, ":", "")

  datatime_local = (exif$DateTimeOriginal_nocolon)

  return(datatime_local)

}

#### lat and lon (Format: dd.dddddddd (EPSG:4326)) ####

# BEGIN FUNCTION CODE

exif_coordinates <- data.frame(exif$X, sf::st_coordinates(exif[,1], st_coordinates(exif[,2])))

lat = exif_coordinates$Y

lon = exif_coordinates$X

# END FUNCTION CODE

# turn code into a function

extract_lat_lon = function (exif) {

  exif_coordinates <- data.frame(exif$X, sf::st_coordinates(exif[,1], st_coordinates(exif[,2])))

  lat = exif_coordinates$Y

  lon = exif_coordinates$X

  lat_lon = data.frame (lat, lon)

  return(lat_lon)
}

#### rtk_fix (Format: True/False. Use EXIF RTKFlat) ####

# goal: return TRUE if there is a RtkFlag value of 50, and FALSE otherwise (i.e. other RtkFlag value, or none). IMPORTANT NOTE!!!!!! THIS FUNCTION ASSUMES DJI DRONES WERE USED. OTHER DRONE MAKES WILL REQUIRE AN UPDATED FUNCTION.

# BEGIN FUNCTION CODE

rtk_fix = {
  if ("RtkFlag" %in% names(exif)) {rtk_fix = exif$RtkFlag == 50}
  else {rtk_fix = rep(FALSE, nrow(exif))}
}

# END FUNCTION CODE

# turn code into a function

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

# EXIF files have an RTK standard longitude deviation (RtkStdLon, the standard deviation (in meters) of the photo recording position in longitude direction), an RTK standard latitude deviation (RtkStdLat, the standard deviation (in meters) of the photo recording position in latitude direction), and an RTK standard altitude deviation (RtkStdHgt, the RTK positioning standard elevation deviation in meters).

# BEGIN FUNCTION CODE

accuracy_x = exif$RtkStdLon

accuracy_y = exif$RtkStdLat

accuracy_z = exif$RtkStdHgt

# END FUNCTION CODE

# turn code into a function

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

# BEGIN FUNCTION CODE

camera_pitch = exif$GimbalPitchDegree + 90

camera_roll = exif$GimbalRollDegree

camera_yaw = exif$GimbalYawDegree

# END FUNCTION CODE

# turn code into a function

extract_pitch_roll_yaw = function(exif) {

  camera_pitch = exif$GimbalPitchDegree + 90

  camera_roll = exif$GimbalRollDegree

  camera_yaw = exif$GimbalYawDegree

  pitch_roll_yaw = data.frame (camera_pitch, camera_roll, camera_yaw)

  return(pitch_roll_yaw)
}

#### exposure (Units: sec) ####

# BEGIN FUNCTION CODE

exposure = exif$ExposureTime

# END FUNCTION CODE

# turn code into a function

extract_exposure = function (exif) {
  exposure = exif$ExposureTime
  return(exposure)
}

#### aperture (Format: xxxxx) ####

# BEGIN FUNCTION CODE

aperture = (exif$Aperture)

# END FUNCTION CODE

# turn code into a function

extract_aperture = function (exif) {
  aperture = (exif$Aperture)
  return(aperture)
}

#### iso ####

# BEGIN FUNCTION CODE

iso = exif$ISO

# END FUNCTION CODE

# turn code into a function

extract_iso = function(exif) {
  iso = exif$ISO
  return(iso)
}

#### white_balance (Format: auto/sunny/cloudy/(others?)) ####

# Some example exifs have all photos with white balance 0, others have all photos with white balance 1

# BEGIN FUNCTION CODE

white_balance = dplyr::case_when(
  exif$WhiteBalance == 0 ~ "auto",
  exif$WhiteBalance == 1 ~ "manual"
)

# END FUNCTION CODE

# turn code into a function

extract_white_balance = function(exif) {

  white_balance = dplyr::case_when(
    exif$WhiteBalance == 0 ~ "auto",
    exif$WhiteBalance == 1 ~ "manual"
  )

  return(white_balance)
}

#### received_image_path (Image path in as-received dataset, with the top level being the folder named with the dataset ID) ####

# BEGIN FUNCTION CODE

received_image_path = stringr::str_split_fixed(exif$SourceFile, fixed(dataset_id), 2)

received_image_path <- received_image_path[,2]

received_image_path <- with(exif, paste0(dataset_id, received_image_path))

# END FUNCTION CODE

# turn code into a function

extract_received_image_path = function(exif) {

  received_image_path = stringr::str_split_fixed(exif$SourceFile, fixed(dataset_id), 2)

  received_image_path <- received_image_path[,2]

  received_image_path <- with(exif, paste0(dataset_id, received_image_path))

  return(received_image_path)
}

#### altitude: returns altitude above sea level (asl) in meters ####

# BEGIN FUNCTION CODE

altitude_asl = exif$AbsoluteAltitude

# END FUNCTION CODE

# turn code into a function

extract_altitude = function(exif) {

  altitude_asl = exif$AbsoluteAltitude

  altitude = data.frame (altitude_asl)

  return(altitude)
}

#### Step 2: Create wrapper for metadata extraction functions. Preps the EXIF data for passing to the extraction functions, then calls all the individual extraction functions to extract the respective attributes. ####

extract_metadata_emp = function(exif_filepath) {

  # Prep the EXIF data for extraction of metadata attributes
  exif = prep_exif(exif_filepath)

  # Extract/compute metadata attributes
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

  # Return extracted/computed metadata as a data frame row
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
