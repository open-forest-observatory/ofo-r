# Purpose: Obtain the list of all files in the Data Store. Save to a CSV file.

# This script requries that you have irods installed and configured for anonymous access to CyVerse
# following: https://learning.cyverse.org/ds/icommands/. This is already set up by default on
# internal OFO dev instances.

library(stringr)
library(tidyverse)

PUBLISHED_DATA_RECORDS_DIR = "/ofo-share/drone-imagery-organization/8_published-data-records"

base_path = "/iplant/home/shared/ofo/public/missions/"
call = paste0("iquest --no-page '%s/%s' \"select COLL_NAME, DATA_NAME where COLL_NAME like '", base_path, "%/processed_%/full' and DATA_NAME = 'chm-mesh.tif'\"")
output = system(call, intern = TRUE)

rel_paths = str_replace(output, base_path, "")
file_parts = str_split(rel_paths, "/")
mission_ids = map_chr(file_parts, 1)
processed_folder = map_chr(file_parts, 2) |> unlist()
processed_parts = str_split(processed_folder, "-")
processing_run_id = map_chr(processed_parts, 2) |> unlist()

# Complie relevant attributes into a dataframe
chm_files = tibble(
  mission_id = mission_ids,
  processing_run_id = processing_run_id,
  chm_id = paste0(mission_id, "-", processing_run_id),
  filepath_abs = absolute_paths,
  filepath_rel = rel_paths
)

write_csv(chm_files, file.path(PUBLISHED_DATA_RECORDS_DIR, "chm_mesh_files.csv"))
