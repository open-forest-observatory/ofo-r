# Purpose: At the mission level and the sub-mission level (separately), merge the human-provided Baserow metadata
# and the EXIF metadata extracted from the drone imagery.

library(tidyverse)

IMAGERY_PROJECT_NAME = "2020-dispersal"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

# Derived constants
baserow_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
baserow_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
exif_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
exif_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
full_metadata_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-full-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
full_metadata_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-full-metadata_", IMAGERY_PROJECT_NAME, ".csv"))

# Load the metadata
baserow_sub_mission = read_csv(baserow_sub_mission_filepath)
baserow_mission = read_csv(baserow_mission_filepath)
exif_sub_mission = read_csv(exif_sub_mission_filepath)
exif_mission = read_csv(exif_mission_filepath)


# Combine at the mission level
metadata_mission = left_join(exif_mission, baserow_mission, by = c("dataset_id" = "dataset_id"))
metadata_sub_mission = left_join(exif_sub_mission, baserow_sub_mission, by = c("dataset_id" = "dataset_id"))


# Write the result
write_csv(metadata_mission, full_metadata_mission_filepath)
write_csv(metadata_sub_mission, full_metadata_sub_mission_filepath)
