# Purpose: For each sub-mission, pull the relevant Baserow metadata. For each mission, merge the
# pulled sub-mission data into a mission-level record by concatenating the multiple different values
# where they differ. Save out the mission-level and sub-mission-level data. If the dataset
# association records indicate that a mission is one part of a two-part grid, change the
# mission-level mission type from "normal" to "grid".

# TODO: Currently, we are ignoring cases where there are two baserow records for a mission, but the
# mission could not be split out into sub-missions that we were confident corresponded to the two
# baserow records. The occurrence of this is recorded in the crosswalks in 1c_exif-for-sorting. When
# this occurred, all missions were crosswalked to the first Baserow record. We could improve this
# script's workflow to take the non-directly-assignable records into account and report both entries
# for any attributes that differ between the two records for an unsplittable dataset.

library(tidyverse)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = readr::read_lines(IMAGERY_PROJECT_NAME_FILE)

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
FOLDER_BASEROW_CROSSWALK_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting/"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

# Derived constants
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))
metadata_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))


# Pull in the baserow (human-entered) metadata
baserow_datasets = read_csv(file.path(BASEROW_DATA_PATH, "export - datasets-imagery.csv"))
baserow_projects = read_csv(file.path(BASEROW_DATA_PATH, "export - acquisition-projects.csv"))
baserow_dataset_associations = read.csv(file.path(BASEROW_DATA_PATH, "export - dataset-associations - Grid.csv"))

# Fix the formatting of the Baserow dataset_id, and remove the internal baserow 'id' column
baserow_datasets = baserow_datasets |>
  mutate(dataset_id = str_pad(dataset_id, 6, pad = "0", side = "left")) |>
  select(-id)

# Merge some project-level data into the dataset-level data:
# - contributor_names
# - contact_info
# - license
# - objectives
baserow_projects = baserow_projects |>
  select(project_id, contributor_names, contact_info, license, objectives)
baserow_datasets = baserow_datasets |>
  left_join(baserow_projects, by = c("project_id" = "project_id"))

# Apply any dataset-level contributor overrides
baserow_datasets = baserow_datasets |>
  mutate(contributor_names = ifelse(!is.na(contributor_names_override), contributor_names_override, contributor_names),
         contact_info = ifelse(!is.na(contact_info_override), contact_info_override, contact_info)) |>
  select(-contributor_names_override, -contact_info_override)


# Load the crosswalk linking sub-dataset folder names to baserow rows. If there is more than one sub-mission for
# a mission, they may or may not have different entries in Baserow, and their manually extracted
# exif may or may not have differences.
crosswalk = read_csv(crosswalk_filepath)

# We need to create a set of attributes at the mission level and the sub-mission level. At the
# mission level, we can generate the *derived* attributes from the exif data, but the *manually
# provided* attributes from Baserow like base station location and drone model will need to be
# pulled from both matching baserow rows and concatenated

# Start with the sub-mission-level Baserow attributes
sub_mission_baserow = left_join(crosswalk, baserow_datasets, by = c("dataset_id_baserow" = "dataset_id")) |>
  select(-dataset_id_baserow) |>
  rename(sub_mission_id = dataset_id_imagefolder) |>
  # Get the mission ID as the first part of the sub-mission ID
  mutate(mission_id = str_sub(sub_mission_id, 1, 6)) |>
  # Create an alias for the sub_mission_id as dataset_id, since other metadata files at the
  # sub-mission level use this column name
  mutate(dataset_id = sub_mission_id)

# Next, the mission-level Baserow attributes, including concatenating the sub-mission-level
# attributes if they differ

concat_unique = function(x) {
  x = x[!is.na(x)]
  unq = unique(x)
  if (length(unq) > 1) {
    return(paste(unq, collapse = ", "))
  } else if (length(unq) == 0) {
    return(NA)
  } else {
    return(unq)
  }
}

mission_baserow = sub_mission_baserow |>
  # At the mission level, the dataset_id is the mission_id
  mutate(dataset_id = mission_id) |>
  mutate(across(everything(), as.character)) |>
  group_by(mission_id) |>
  summarize(across(everything(), concat_unique)) |>
  ungroup()

# Determine if the combined mission comprises a grid, by looking up all the dataset_ids (these are
# mission IDs, not sub-mission IDs) from the dataset_associations table that have an association
# type of "multi-orientation"

grid_missions = baserow_dataset_associations |>
  filter(assoc_type == "multi-orientation") |>
  pull(dataset_ids) |>
  str_split(pattern = ",") |>
  unlist() |>
  unique() |>
  str_pad(6, pad = "0", side = "left")

# We have the Baserow dataset IDs for the missions. Look up the sub-mission IDs (folder names) for
# them.
grid_mission_ids = crosswalk |>
  filter(dataset_id_baserow %in% grid_missions) |>
  pull(dataset_id_imagefolder) |>
  # The folder names are the sub-mission IDs, so extract the mission ID from them
  str_sub(1, 6) |>
  unique()

# Identify which mission-level baserow records match grid missions and set the flight pattern to
# "grid"
mission_baserow[mission_baserow$mission_id %in% grid_mission_ids, "flight_pattern"] = "grid"

# Save out the metadata
write_csv(sub_mission_baserow, metadata_sub_mission_filepath)
write_csv(mission_baserow, metadata_mission_filepath)
