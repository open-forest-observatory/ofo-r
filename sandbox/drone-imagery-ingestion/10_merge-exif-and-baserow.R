# Purpose: At the mission level and the sub-mission level (separately), merge the human-provided Baserow metadata
# and the summarized (mission or sub-mission level) EXIF metadata extracted from the drone imagery.

library(tidyverse)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = readr::read_lines(IMAGERY_PROJECT_NAME_FILE)

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

# Derived constants
# In
baserow_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
baserow_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
exif_metadata_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
exif_metadata_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))

# Out
full_metadata_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-full-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
full_metadata_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-full-metadata_", IMAGERY_PROJECT_NAME, ".csv"))

# Load the metadata
baserow_sub_mission = read_csv(baserow_sub_mission_filepath)
baserow_mission = read_csv(baserow_mission_filepath)
exif_metadata_mission = read_csv(exif_metadata_mission_filepath)
exif_metadata_sub_mission = read_csv(exif_metadata_sub_mission_filepath)

# dataset_id is confusing so it is just dropped in general
# In the context of a mission, the sub_mission_id has different meanings in baserow and the exif.
# In baserow, it means the list of sub-missions included in the mission that a given image is part of.
# In the exif it is just the sub-mission that a given image is a part of.
baserow_mission = baserow_mission |>
  rename(sub_mission_ids = sub_mission_id) |>
  select(-dataset_id)

baserow_sub_mission = subset(baserow_sub_mission, select = -dataset_id)

# Generate image-level metadata
metadata_mission = right_join(
  baserow_mission,
  exif_metadata_mission,
  by = c("mission_id" = "mission_id"),
)
metadata_sub_mission = right_join(
  baserow_sub_mission,
  exif_metadata_sub_mission,
  by = c("sub_mission_id" = "sub_mission_id"),
)

# Write the result
write_csv(metadata_mission, full_metadata_mission_filepath)
write_csv(metadata_sub_mission, full_metadata_sub_mission_filepath)
