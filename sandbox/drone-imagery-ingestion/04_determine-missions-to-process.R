# Purpose: Get the mission IDs of the missions to process, using one of several options, and write a
# CSV of the mission IDs to be referenced by the next scripts in the pipeline.

library(tidyverse)

## Constants

# # One way to specify which missions is all that make up a project, as specified here:
# IMAGERY_PROJECT_NAME = "2024-ofo2"

# Another way is to read the project name from the text file
# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)


# # Optionally can specify a subset of missions
IMAGERY_PROJECT_SUBSET_MISSIONS = NULL
# IMAGERY_PROJECT_SUBSET_MISSIONS = c(000643:000900) |> str_pad(6, pad = "0", side = "left")

# In
MISSION_ID_CROSSWALK_DIR = "/ofo-share/drone-imagery-organization/metadata/1_reconciling-contributions/3_contributed-to-sorted-id-crosswalk/"

# Out
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")

## Workflow

# Read the crosswalk file that contains the mission IDs for the project
crosswalk_file = file.path(MISSION_ID_CROSSWALK_DIR, paste0(IMAGERY_PROJECT_NAME, ".csv"))
crosswalk = read_csv(crosswalk_file)

# Extract the mission IDs from the crosswalk
missions_to_process = crosswalk$mission_id |> unique()

# If a subset of missions is specified, filter the missions to process
if (!is.null(IMAGERY_PROJECT_SUBSET_MISSIONS) && length(IMAGERY_PROJECT_SUBSET_MISSIONS) > 0) {
  missions_to_process = missions_to_process[missions_to_process %in% IMAGERY_PROJECT_SUBSET_MISSIONS]
}

# Format as data frame
missions_to_process_df = data.frame(mission_id = missions_to_process)

# Write
write_csv(missions_to_process_df, MISSIONS_TO_PROCESS_LIST_PATH)
