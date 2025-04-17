# Purpose: Obtain the list of processed mesh-based CHM files from the Data Store, including the
# mission ID, processing run ID, and the full path to the file on the data store. Save to a CSV
# file.

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





############### Working but unneceessarily complex alternative

# # This script requries that you have rclone set up and configured with a SFTP remote named 'cvftp'.
# # It will need to be configured with your CyVerse username and password. !!! Make sure rclone is set
# # up with an encrypted config file since it will contain your password and be stored on Jetstream
# # !!!.  To allow rclone to decrypt your config file, you can set the environment variable
# # RCLONE_CONFIG_PASS, which you can do from R as demonstrated below. Just make sure you do not save
# # your password in the R script.

# library(stringr)
# library(tidyverse)
# library(processx)

# PUBLISHED_DATA_RECORDS_DIR = "/ofo-share/drone-imagery-organization/8_published-data-records"

# Sys.setenv(RCLONE_CONFIG_PASS = "mypass")

# base_path = "shared/ofo/public/missions"
# base_with_remote = paste0("cvftp:", base_path)

# # Run the system command and store output, but also display it for tracking progress since it takes
# # so long to run. From
# # https://stackoverflow.com/questions/62902042/simultaneously-save-and-print-r-system-call-output
# proc <- processx::process$new("rclone", args = c("ls", base_with_remote, "--include", "**/chm-mesh.tif", "--transfers", "64"), stdout = "|")
# output <- character(0)
# while (proc$is_alive()) {
#   Sys.sleep(5)
#   thisout <- proc$read_output_lines()
#   if (length(thisout)) {
#     output = c(output, thisout)
#     thisout_msg = paste0(thisout, collapse = "\n")
#     message(thisout_msg)
#   }
# }
# # If you start a process incorrectly and need to kill it: proc$kill()

# # Parse the rclone output into the relevant attributes
# output = str_trim(output)
# parts = map(output, str_split_1, " ")
# filepaths = map(parts, 2)
# filepaths = unlist(filepaths)
# absolute_paths = file.path(base_path, filepaths)
# file_parts = str_split(filepaths, "/")
# mission_ids = map_chr(file_parts, 1)
# processed_folder = map_chr(file_parts, 2) |> unlist()
# processed_parts = str_split(processed_folder, "-")
# processing_run_id = map_chr(processed_parts, 2) |> unlist()

# # Complie relevant attributes into a dataframe
# chm_files = tibble(
#   mission_id = mission_ids,
#   processing_run_id = processing_run_id,
#   chm_id = paste0(mission_id, "-", processing_run_id),
#   filepath_abs = absolute_paths,
#   filepath_rel = filepaths
# )

# write_csv(chm_files, file.path(PUBLISHED_DATA_RECORDS_DIR, "chm_mesh_files.csv"))


############### Tested but rejected alternatives (incomplete and broken)

# library(rirods)
# library(webdav)

# Sys.setenv(RCLONE_CONFIG = "/ofo-share/utils/rclone-cyverse-anon.conf")

# request = webdav_create_request("https://data.cyverse.org/dav-anon/", "djyoung", "mypassword")



# # Get the list of CHM files from CyVerse WebDAV
# all_files = webdav_copy_file("https://data.cyverse.org/dav-anon/",
#                               from_path = "/iplant/projects/ofo/public/missions/000001/footprint/footprint.gpkg",
#                               to_path = "/ofo-share/temp",
#                               username = "",
#                               password = "")

# all_files = webdav_list_files(base_url = "https://data.cyverse.org/dav-anon/",
#                               folder_path = "/iplant/projects/ofo/public/missions/000001/footprint",
#                               depth = 5,
#                               username = "",
#                               password = "")


# create_irods("http://data.cyverse.org:1247/", overwrite = TRUE)
# iauth("djyoung", "mypassword")
# df
# pattern = file.path(PHOTOGRAMMETRY_PUBLISH_PATH, "*", "full/chm-mesh.tif$")
# chm_files = list.files(PHOTOGRAMMETRY_PUBLISH_PATH, pattern = "full//chm-mesh.tif$", full.names = TRUE, recursive = TRUE)



# Sys.setenv(RCLONE_CONFIG_PASS = "mypass")
# system('rclone ls cvftp:shared/ofo/public/metadata')

# base_path = "shared/ofo/public/missions"
# base_with_remote = paste0("cvftp:", base_path)

# system_call = paste0("rclone ls cvftp:", base_path, " --include '**/chm-mesh.tif' --transfers 64")
# system_call_no_rclone = paste0("ls ", base_path, " --include '**/chm-mesh.tif'")

# a = system(system_call, intern = TRUE)


# system_call = paste0("rclone ls cvftp:", base_path, " --include '**/chm-mesh.tif' --transfers 64")
# system_call_no_rclone = paste0("ls ", base_path, " --include '**/chm-mesh.tif'")

# a = system(system_call, intern = TRUE)

# processx::run("rclone", args = c("ls", base_with_remote, "--include", "**/chm-mesh.tif", "--transfers", "64"), stdout = "|")
