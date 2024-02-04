# This script has two purposes: 1. Demonstrate how to run functions to extract metadata from imagery
# dataset EXIF data, and 2. Demonstrate how to write a new metadata extraction function.

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
# datadir = readLines(file.path("sandbox", "data-dirs", "derek-metadata-laptop.txt"))

datadir = readLines(file.path("sandbox", "data-dirs", "emp-metadata-laptop.txt"))


# --- 1. Workflow for running metadata extraction ---

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
exif_file = exif_files[1]

# Run for that one EXIF file.
extract_metadata_dy(exif_file, plot_flightpath = TRUE)
# ^ If you want to inspect the definition of this function, it is at the bottom of
# 'R/imagery-metadata-extraction_dy.R'.

# Run extraction on all EXIF files
metadata = purrr::map_dfr(exif_files, extract_metadata_dy, plot_flightpath = TRUE)
metadata

# Write results to file (creating directory if it doesn't exist)
out_dir = file.path(datadir, "extracted-metadata", "dataset-level-tabular")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
# write.csv(metadata, file.path(out_dir, "dataset-metadata_dy.csv"), row.names = FALSE)

write.csv(metadata, file.path(out_dir, "dataset-metadata_emp.csv"), row.names = FALSE)


# --- 2. Example of how to use this sandbox script to write a metadata extraction function ---

# Select an EXIF file to test on, and prep the EXIF data by loading it as a geospatial data frame
# using the 'prep_exif' function. The 'prep_exif' function is defined in
# 'R/imagery-metadata-extraction_general.R'            # nolint
exif_file = exif_files[1]
exif = prep_exif(exif_file)
# Note that the 'prep_exif' function returns the EXIF data as a geospatial data frame (an 'sf'
# object) with point geometry. So any geospatial operations you attempt on it should use functions
# from the 'sf' package. If you are more familiar with 'terra' objects and would rather work with
# them, let Derek know and we can create an option to return 'terra::vect' objects.

# Between the BEGIN and END comments below, write code to extract the metadata attribute you're
# working on. When you're done, you can wrap it in a function definition, taking only one parameter,
# 'exif'. Here is an example of developing code to extract the number of images in an imagery
# dataset.

# BEGIN FUNCTION CODE

# Get the number of images in the dataset by counting the rows of the EXIF data frame
image_count = nrow(exif)

# END FUNCTION CODE


# Now here is an example of turning that code into a function

extract_image_count = function(exif) {

  # Get the number of images in the dataset by counting the rows of the EXIF data frame
  image_count = nrow(exif)

  return(image_count)

}


# Now you can test the function on the EXIF data

image_count = extract_image_count(exif)
image_count

# Once it is working right, you can move this function to your
# 'R/imagery-metadata-extraction_<initials>.R' file and then add a call to this function from within
# your 'extract_metadata_<initials>' function. Once it is in there, then you can run the top part of
# this script again, and when it extracts the metadata for each EXIF dataset, your additional
# metadata should be included.





#### Image-level metadata CSV file (one file per dataset) ####

# Output file: imagery-metadata-dev/extracted-metadata/image-level-metadata/image-metadata_<dataset_id>.csv

exif_file = exif_files[1]
exif = prep_exif(exif_file)

#### First need to make sure each exif file has the right columns for the following functions. If column(s) are missing, need to add columns of blanks or NAs ####

#### dataset ID ####

# BEGIN FUNCTION CODE

dataset_id = exif$dataset_id

# END FUNCTION CODE

# turn code into a function

extract_dataset_id = function (exif) {

  dataset_id = exif$dataset_id

  return(dataset_id)

}

# test function on exif data

dataset_id = extract_dataset_id (exif)

dataset_id

#### datatime_local (Format: YYYYMMDD HHMMSS (local time zone, 24 hr)) ####

# BEGIN FUNCTION CODE

exif$DateTimeOriginal_nocolon <- sub(":", "", exif$DateTimeOriginal)
exif$DateTimeOriginal_nocolon <- sub(":", "", exif$DateTimeOriginal_nocolon)
exif$DateTimeOriginal_nocolon <- sub(":", "", exif$DateTimeOriginal_nocolon)
exif$DateTimeOriginal_nocolon <- sub(":", "", exif$DateTimeOriginal_nocolon)

datatime_local = (exif$DateTimeOriginal_nocolon)

# END FUNCTION CODE

# turn code into a function

extract_datatime_local = function (exif) {

  exif$DateTimeOriginal_nocolon <- sub(":", "", exif$DateTimeOriginal)
  exif$DateTimeOriginal_nocolon <- sub(":", "", exif$DateTimeOriginal_nocolon)
  exif$DateTimeOriginal_nocolon <- sub(":", "", exif$DateTimeOriginal_nocolon)
  exif$DateTimeOriginal_nocolon <- sub(":", "", exif$DateTimeOriginal_nocolon)

  datatime_local = (exif$DateTimeOriginal_nocolon)

  return(datatime_local)

}

# test function on exif data

datatime_local = extract_datatime_local(exif)

datatime_local

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

# test function on exif data

lat_lon = extract_lat_lon(exif)
lat_lon

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

# test function on exif data

rtk_fix = extract_rtk_fix(exif)
rtk_fix

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

# test function on exif data

accuracy = extract_accuracy(exif)
accuracy

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

# test function on exif data

pitch_roll_yaw = extract_pitch_roll_yaw(exif)
pitch_roll_yaw

#### exposure (Units: sec) ####

# BEGIN FUNCTION CODE

exposure = exif$ExposureTime

# END FUNCTION CODE

# turn code into a function

extract_exposure = function (exif) {
  exposure = exif$ExposureTime
  return(exposure)
}

# test function on exif data

exposure = extract_exposure(exif)
exposure

#### aperture (Format: xxxxx) ####

# BEGIN FUNCTION CODE

aperture = (exif$Aperture)

# END FUNCTION CODE

# turn code into a function

extract_aperture = function (exif) {
  aperture = (exif$Aperture)
  return(aperture)
}

# test function on exif data

aperture = extract_aperture (exif)
aperture

#### iso ####

# BEGIN FUNCTION CODE

iso = exif$ISO

# END FUNCTION CODE

# turn code into a function

extract_iso = function(exif) {
  iso = exif$ISO
  return(iso)
}

# test function on exif data

iso = extract_iso(exif)
iso

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

# test function on exif data

white_balance = extract_white_balance(exif)
white_balance

#### received_image_path (Image path in as-received dataset, with the top level being the folder named with the dataset ID) ####

# BEGIN FUNCTION CODE

received_image_path = stringr::str_split_fixed(exif$SourceFile, "/", 5)

received_image_path <- received_image_path[,5]

# END FUNCTION CODE

# turn code into a function

extract_received_image_path = function(exif) {

  received_image_path = stringr::str_split_fixed(exif$SourceFile, "/", 5)

  received_image_path <- received_image_path[,5]

  return(received_image_path)
}

# test function on exif data

received_image_path = extract_received_image_path(exif)
received_image_path

#### altitude: returns altitude above ground level (agl) and above sea level (asl) in meters ####

# BEGIN FUNCTION CODE

altitude_agl = exif$RelativeAltitude

altitude_asl = exif$AbsoluteAltitude

# END FUNCTION CODE

# turn code into a function

extract_altitude = function(exif) {

  altitude_agl = exif$RelativeAltitude

  altitude_asl = exif$AbsoluteAltitude

  altitude = data.frame (altitude_agl, altitude_asl)

  return(altitude)
}

# test function on exif data

altitude = extract_altitude(exif)
altitude

#### standardized_image_path (Image path in standardized dataset) ####

# Cannot complete until we also include the step of reorganizing the image files into a standardized naming/folder structure

# BEGIN FUNCTION CODE

# END FUNCTION CODE

# turn code into a function

# test function on exif data

#### Create wrapper for metadata extraction functions. Preps the EXIF data for passing to the extraction functions, then calls all the individual extraction functions to extract the respective attributes. ####

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

#### Workflow for running metadata extraction ####

exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

out_dir = file.path(datadir, "extracted-metadata", "image-level-metadata")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Extract metadata for one EXIF file at a time and save to csv

exif_file = exif_files[1]

metadata1 <- extract_metadata_emp(exif_file)

write.csv(metadata1, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\image-level-metadata\\image-metadata_20220630-0041.csv"), row.names = FALSE)


exif_file = exif_files[2]

metadata2 <- extract_metadata_emp(exif_file)

write.csv(metadata2, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\image-level-metadata\\image-metadata_20220730-0079.csv"), row.names = FALSE)


exif_file = exif_files[3]

metadata3 <- extract_metadata_emp(exif_file)

write.csv(metadata3, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\image-level-metadata\\image-metadata_20230528-0008.csv"), row.names = FALSE)


exif_file = exif_files[4]

metadata4 <- extract_metadata_emp(exif_file)

write.csv(metadata4, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\image-level-metadata\\image-metadata_20230528-0009.csv"), row.names = FALSE)


exif_file = exif_files[5]

metadata5 <- extract_metadata_emp(exif_file)

write.csv(metadata5, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\image-level-metadata\\image-metadata_20230706-0152.csv"), row.names = FALSE)


exif_file = exif_files[6]

metadata6 <- extract_metadata_emp(exif_file)

write.csv(metadata6, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\image-level-metadata\\image-metadata_20230706-0153.csv"), row.names = FALSE)
