# Purpose: At the mission level and the sub-mission level (separately), merge the human-provided Baserow metadata
# and the summarized (mission or sub-mission level) EXIF metadata extracted from the drone imagery.

library(tidyverse)
library(sf)
library(furrr)
library(ofo)

# In
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")
CONTRIBUTED_METADATA_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/1_contributed-metadata-per-mission/"
CONTRIBUTED_METADATA_SUB_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/2_contributed-metadata-per-sub-mission/"
DERIVED_METADATA_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/6_derived-metadata-per-mission"
DERIVED_METADATA_SUB_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/7_derived-metadata-per-sub-mission"

# Out
FULL_METADATA_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/3_final/1_full-metadata-per-mission/"
FULL_METADATA_SUB_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/3_final/2_full-metadata-per-sub-mission/"

## Workflow

# Determine which missions to process
missions_to_process = read_csv(MISSIONS_TO_PROCESS_LIST_PATH) |>
  pull(mission_id)

# Create the output folders
create_dir(FULL_METADATA_MISSION_PATH)
create_dir(FULL_METADATA_SUB_MISSION_PATH)


merge_derived_and_contributed_metadata = function(mission_foc) {

  # Derived filepaths
  baserow_mission_filepath = file.path(CONTRIBUTED_METADATA_MISSION_PATH, paste0(mission_foc, ".csv"))
  exif_metadata_mission_filepath = file.path(DERIVED_METADATA_MISSION_PATH, paste0(mission_foc, ".gpkg"))

  # Load the mission-level metadata
  baserow_mission = read_csv(baserow_mission_filepath)
  exif_metadata_mission = st_read(exif_metadata_mission_filepath)

  # dataset_id is confusing so it is just dropped in general
  # In the context of a mission, the sub_mission_id has different meanings in baserow and the exif.
  # In baserow, it means the list of sub-missions included in the mission that a given image is part of.
  # In the exif it is just the sub-mission that a given image is a part of.
  baserow_mission = baserow_mission |>
    # Rename the sub_mission_id to be more descriptive (its a comma-separated list of sub-mission IDs)
    rename(sub_mission_ids = sub_mission_id)

  # Bind together the derived and contributed mission-level metadata
  full_metadata_mission = bind_cols(
    exif_metadata_mission |>
      select(-mission_id), # remove redundant col
    baserow_mission) |>
    # Put all the derived columns (which end in _derived) at the end
    select(!ends_with("_derived"), everything())

  # Write it
  full_metadata_mission_filepath = file.path(FULL_METADATA_MISSION_PATH, paste0(mission_foc, ".gpkg"))
  st_write(full_metadata_mission, full_metadata_mission_filepath, delete_dsn = TRUE)

  # Get the sub-missions that make up the mission
  sub_mission_files = list.files(
    path = file.path(CONTRIBUTED_METADATA_SUB_MISSION_PATH),
    pattern = paste0(mission_foc, "-[0-9]{2}"),
    full.names = TRUE
  )
  sub_mission_ids = sub_mission_files |>
    basename() |>
    str_extract(paste0(mission_foc, "-[0-9]{2}"))

  for (sub_mission_id_foc in sub_mission_ids) {
    # Load the sub-mission metadata
    baserow_sub_mission = read_csv(file.path(CONTRIBUTED_METADATA_SUB_MISSION_PATH, paste0(sub_mission_id_foc, ".csv")))
    exif_metadata_sub_mission = st_read(file.path(DERIVED_METADATA_SUB_MISSION_PATH, paste0(sub_mission_id_foc, ".gpkg")))

    # Bind together the derived and contributed sub-mission-level metadata
    full_metadata_sub_mission = bind_cols(
      exif_metadata_sub_mission |>
        select(-sub_mission_id, -mission_id),
      baserow_sub_mission) |>
      # Put all the derived columns (which end in _derived) at the end
      select(!ends_with("_derived"), everything())

    # Write it
    full_metadata_sub_mission_filepath = file.path(FULL_METADATA_SUB_MISSION_PATH, paste0(sub_mission_id_foc, ".gpkg"))
    st_write(full_metadata_sub_mission, full_metadata_sub_mission_filepath, delete_dsn = TRUE)
  }
}


# Parallelize across all selected missions
future::plan(multisession)
future_walk(
  missions_to_process,
  merge_derived_and_contributed_metadata,
  .progress = TRUE
)
