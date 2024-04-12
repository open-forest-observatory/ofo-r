# This script defines dataset-level metadata extraction functions

# --- Setup ---

# load packages

library(stringr)
library(tidyverse)
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

# The most common white balance setting across all images in the project. Options: sunny, cloudy, auto

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

image_density =

# END FUNCTION CODE

# turn code into a function

extract_image_density = function (exif) {

  image_density =

  return(image_density)

}

# test function on exif data

image_density = extract_image_density(exif)

image_density

#### area_ha ####

# Area of mission footprint. Units: ha.

# BEGIN FUNCTION CODE

area_ha =

# END FUNCTION CODE

# turn code into a function

extract_area_ha = function (exif) {

  area_ha =

  return(area_ha)

}

# test function on exif data

area_ha = extract_area_ha(exif)

area_ha
