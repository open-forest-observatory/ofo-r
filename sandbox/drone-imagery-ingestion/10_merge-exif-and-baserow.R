# Purpose: At the mission level and the sub-mission level, merge the human-provided Baserow metadata
# and the EXIF metadata extracted from the drone imagery.

library(tidyverse)

IMAGERY_PROJECT_NAME = "2019-focal"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

# Derived constants
baserow_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
baserow_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
exif_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
exif_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_", IMAGERY_PROJECT_NAME, ".csv"))

# Load the metadata