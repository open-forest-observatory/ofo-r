# Purpose: For each sub-mission, pull the relevant Baserow metadata. For each mission, merge the
# pulled sub-mission data into a mission-level record by concatenating the multiple different values
# where they differ. Save out the mission-level and sub-mission-level data. If the dataset
# association records indicate that a mission is on part of a two-part grid, set the mission-level
# mission type from "normal" to "grid".

# TODO: Currently, we are ignoring cases where there are two baserow records for a mission, but the
# mission could not be split out into sub-missions that we were confident corresponded to the two
# baserow records. The occurrence of this is recorded in the crosswalks in 1c_exif-for-sorting. When
# this occurred, all missions were crosswalked to the first Baserow record. We could improve this
# script's workflow to take the non-directly-assignable records into account and report both entries
# for any attributes that differ between the two records for an unsplittable dataset.

library(tidyverse)

devtools::load_all()

IMAGERY_PROJECT_NAME = "2019-focal"

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
FOLDER_BASEROW_CROSSWALK_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting/"


# Derived constants
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))



# Pull in the baserow (human-entered) metadata
baserow = read_csv(file.path(BASEROW_DATA_PATH, "export - datasets-imagery.csv"))
dataset_associations = read.csv(file.path(BASEROW_DATA_PATH, "export - dataset-associations - Grid.csv"))


# Load the crosswalk linking folder names to baserow rows. If there is more than one sub-mission for
# a mission, they may or may not have different entries in Baserow, and their manually extracted
# exif may or may not have differences.
crosswalk = read_csv(crosswalk_filepath)

# We need to create a set of attributes at the mission level and the sub-mission level. At the
# mission level, we can generate the *derived* attributes from the exif data, but the *manually
# provided* attributes from Baserow like base station location and drone model will need to be
# pulled from both matching baserow rows and concatenated