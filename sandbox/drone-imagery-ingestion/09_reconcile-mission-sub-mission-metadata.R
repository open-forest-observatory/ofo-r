# Purpose: Thin the image-level metadata to only include images that were retained by *both* the
# mission level and sub-mission level processing. Write this back to the same metadata input files

library(tidyverse)
library(sf)

IMAGERY_PROJECT_NAME = "2019-focal"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"


## Derived constants
mission_metadata_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))
sub_mission_metadata_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))


## Workflow

# Read in the metadata files
mission_metadata_perimage = read_csv(mission_metadata_perimage_filepath)
sub_mission_metadata_perimage = read_csv(sub_mission_metadata_perimage_filepath)

# Thin to only include the image_ids that are in the other dataset
mission_metadata_perimage = mission_metadata_perimage |>
  filter(image_id %in% sub_mission_metadata_perimage$image_id)
sub_mission_metadata_perimage = sub_mission_metadata_perimage |>
  filter(image_id %in% mission_metadata_perimage$image_id)

write_csv(mission_metadata_perimage, mission_metadata_perimage_filepath)
write_csv(sub_mission_metadata_perimage, sub_mission_metadata_perimage_filepath)
