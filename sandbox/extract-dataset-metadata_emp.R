# This script defines dataset-level metadata extraction functions

# --- Setup ---

# load packages

library(stringr)
library(tidyverse)
library(sf)
library(devtools)

# Set working directory

setwd("C:\\Users\\emily\\Desktop\\FOCAL\\ofo-r")

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
# datadir = readLines(file.path("sandbox", "data-dirs", "derek-metadata-laptop.txt"))

datadir = readLines(file.path("sandbox", "data-dirs", "emp-metadata-laptop.txt"))


# --- 1. Workflow for running metadata extraction ---

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Select an EXIF file to test on, and prep the EXIF data by loading it as a geospatial data frame

exif_file = exif_files[1]
exif = prep_exif(exif_file)

#### Dataset-level metadata CSV file (one file per dataset) ####

# Output file (all datasets in same file, but each developer can save to a separate file to avoid overwriting): imagery-metadata-dev/extracted-metadata/dataset-level-tabular/dataset-metadata_<yourinitials>.csv

#### dataset_id ####

# OFO dataset ID

# BEGIN FUNCTION CODE

dataset_id = exif$dataset_id[1]

# END FUNCTION CODE

# turn code into a function

extract_dataset_id = function (exif) {

  dataset_id = exif$dataset_id[1]

  return(dataset_id)

}

# test function on exif data

dataset_id = extract_dataset_id(exif)

dataset_id

#### image_count ####

# Number of images in set

# BEGIN FUNCTION CODE

image_count = nrow(exif)

# END FUNCTION CODE

# turn code into a function

extract_image_count = function (exif) {

  image_count = nrow(exif)

  return(image_count)

}

# test function on exif data

image_count = extract_image_count(exif)

image_count

#### file_size ####

# Total file size of all images in set. Units: GB.

# BEGIN FUNCTION CODE

file_size = sum(exif$FileSize)

# END FUNCTION CODE

# turn code into a function

extract_file_size = function (exif) {

  file_size = sum(exif$FileSize) / 1000000000

  return(file_size)

}

# test function on exif data

file_size = extract_file_size(exif)

file_size

#### percent_images_rtk ####

# Percent of images with an RTK fix. Use the EXIF RTKFlag. Will need to look up what the different codes mean.

# BEGIN FUNCTION CODE

exif$rtk_fix = extract_rtk_fix(exif)

percent_images_rtk = (sum(exif$rtk_fix == TRUE) / nrow(exif)) * 100

# END FUNCTION CODE

# turn code into a function

extract_percent_images_rtk = function (exif) {

  exif$rtk_fix = extract_rtk_fix(exif)

  percent_images_rtk = (sum(exif$rtk_fix == TRUE) / nrow(exif)) * 100

  return(percent_images_rtk)

}

# test function on exif data

percent_images_rtk = extract_percent_images_rtk(exif)

percent_images_rtk


#### white_balance_mode_derived ####

# The most common white balance setting across all images in the project. Options: auto and manual

# BEGIN FUNCTION CODE

exif$white_balance = extract_white_balance(exif)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

white_balance_mode_derived = Mode(exif$white_balance)

# END FUNCTION CODE

# turn code into a function

extract_white_balance_mode_derived = function (exif) {

  exif$white_balance = extract_white_balance(exif)

  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  white_balance_mode_derived = Mode(exif$white_balance)

  return(white_balance_mode_derived)

}

# test function on exif data

white_balance_mode_derived = extract_white_balance_mode_derived(exif)

white_balance_mode_derived

#### white_balance_mode_prop_derived ####

# The proportion of images with a white balance setting that matches the modal white balance setting in the dataset

# BEGIN FUNCTION CODE

exif$white_balance = extract_white_balance(exif)

white_balance_mode_derived = extract_white_balance_mode_derived(exif)

white_balance_mode_prop_derived = (sum(exif$white_balance == white_balance_mode_derived) / nrow(exif))

# END FUNCTION CODE

# turn code into a function

extract_white_balance_mode_prop_derived = function (exif) {

  exif$white_balance = extract_white_balance(exif)

  white_balance_mode_derived = extract_white_balance_mode_derived(exif)

  white_balance_mode_prop_derived = (sum(exif$white_balance == white_balance_mode_derived) / nrow(exif))

  return(white_balance_mode_prop_derived)

}

# test function on exif data

white_balance_mode_prop_derived = extract_white_balance_mode_prop_derived(exif)

white_balance_mode_prop_derived

#### exposure_median_derived ####

# The median exposure time across all images in the dataset. Units: sec

# BEGIN FUNCTION CODE

exposure_median_derived = median(exif$ExposureTime)

# END FUNCTION CODE

# turn code into a function

extract_exposure_median_derived = function (exif) {

  exposure_median_derived = median(exif$ExposureTime)

  return(exposure_median_derived)

}

# test function on exif data

exposure_median_derived = extract_exposure_median_derived(exif)

exposure_median_derived

#### exposure_stdev_derived ####

# The standard deviation of exposure time across all images in the dataset. Units: sec

# BEGIN FUNCTION CODE

exposure_stdev_derived = sd(exif$ExposureTime)

# END FUNCTION CODE

# turn code into a function

extract_exposure_stdev_derived = function (exif) {

  exposure_stdev_derived = sd(exif$ExposureTime)

  return(exposure_stdev_derived)

}

# test function on exif data

exposure_stdev_derived = extract_exposure_stdev_derived(exif)

exposure_stdev_derived

#### image_density ####

# Computed based on image count and footprint. Units: img/ha

# BEGIN FUNCTION CODE

image_density = (nrow(exif))/(units::set_units(sf::st_area(create_mission_polygon(exif, image_merge_distance)), "hectare"))

# END FUNCTION CODE

# turn code into a function

extract_image_density = function (exif, image_merge_distance) {

  image_density = (nrow(exif))/(units::set_units(sf::st_area(create_mission_polygon(exif, image_merge_distance)), "hectare"))

  return(image_density)

}

# test function on exif data

image_density = extract_image_density(exif, image_merge_distance = 50)

image_density

#### area_ha ####

# Area of mission footprint. Units: ha.

# BEGIN FUNCTION CODE

area_ha = units::set_units(sf::st_area(create_mission_polygon(exif, image_merge_distance)), "hectare")

# END FUNCTION CODE

# turn code into a function

extract_area_ha = function (exif, image_merge_distance) {

  area_ha = units::set_units(sf::st_area(create_mission_polygon(exif, image_merge_distance)), "hectare")

  return(area_ha)

}

# test function on exif data

area_ha = extract_area_ha(exif, image_merge_distance = 50)

area_ha

#### Create wrapper for metadata extraction functions. Preps the EXIF data for passing to the extraction functions, then calls all the individual extraction functions to extract the respective attributes. ####

extract_dataset_metadata_emp = function(exif_filepath, image_merge_distance) {

  # Prep the EXIF data for extraction of metadata attributes
  exif = prep_exif(exif_filepath)

  # Extract/compute metadata attributes
  dataset_id = extract_dataset_id(exif)
  image_count = extract_image_count(exif)
  file_size = extract_file_size(exif)
  percent_images_rtk = extract_percent_images_rtk(exif)
  white_balance_mode_derived = extract_white_balance_mode_derived(exif)
  white_balance_mode_prop_derived = extract_white_balance_mode_prop_derived(exif)
  exposure_median_derived = extract_exposure_median_derived(exif)
  exposure_stdev_derived = extract_exposure_stdev_derived(exif)
  image_density = extract_image_density(exif, image_merge_distance)
  area_ha = extract_area_ha(exif, image_merge_distance = 50)

  # Return extracted/computed metadata as a data frame row
  metadata = data.frame(dataset_id = dataset_id,
                        image_count = image_count,
                        file_size = file_size,
                        percent_images_rtk = percent_images_rtk,
                        white_balance_mode_derived = white_balance_mode_derived,
                        white_balance_mode_prop_derived = white_balance_mode_prop_derived,
                        exposure_median_derived = exposure_median_derived,
                        exposure_stdev_derived = exposure_stdev_derived,
                        image_density = image_density,
                        area_ha = area_ha
  )

  return(metadata)

}

#### Workflow for running metadata extraction ####

exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

out_dir = file.path(datadir, "extracted-metadata", "image-level-metadata")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Extract metadata for one EXIF file at a time and save to csv

exif_file = exif_files[1]

metadata1 <- extract_dataset_metadata_emp(exif_file, image_merge_distance = 50)

write.csv(metadata1, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev/extracted-metadata/dataset-level-tabular/dataset-metadata-emp_20220630-0041.csv"), row.names = FALSE)


exif_file = exif_files[2]

metadata2 <- extract_dataset_metadata_emp(exif_file, image_merge_distance = 50)

write.csv(metadata2, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev/extracted-metadata/dataset-level-tabular/dataset-metadata-emp_20220730-0079.csv"), row.names = FALSE)


exif_file = exif_files[3]

metadata3 <- extract_dataset_metadata_emp(exif_file, image_merge_distance = 50)

write.csv(metadata3, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev/extracted-metadata/dataset-level-tabular/dataset-metadata-emp_20230528-0008.csv"), row.names = FALSE)


exif_file = exif_files[4]

metadata4 <- extract_dataset_metadata_emp(exif_file, image_merge_distance = 50)

write.csv(metadata4, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev/extracted-metadata/dataset-level-tabular/dataset-metadata-emp_20230528-0009.csv"), row.names = FALSE)


exif_file = exif_files[5]

metadata5 <- extract_dataset_metadata_emp(exif_file, image_merge_distance = 50)

write.csv(metadata5, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev/extracted-metadata/dataset-level-tabular/dataset-metadata-emp_20230706-0152.csv"), row.names = FALSE)


exif_file = exif_files[6]

metadata6 <- extract_dataset_metadata_emp(exif_file, image_merge_distance = 50)

write.csv(metadata6, file.path("C:\\Users\\emily\\Box\\imagery-metadata-dev/extracted-metadata/dataset-level-tabular/dataset-metadata-emp_20230706-0153.csv"), row.names = FALSE)
