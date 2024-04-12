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


