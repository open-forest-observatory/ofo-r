# Purpose: For all the images in the sorted missions folder (including all subdirectories), set the
# "Orientation" flag to `1`. Note that you must have exiftool installed first, such as by `sudo apt
# install libimage-exiftool-perl`.

library(furrr)

SORTED_MISSIONS_FOLDER = "/ofo-share/drone-imagery-organization/3_sorted-mission"

# Get each mission folder (for parallelizing)
folders = list.dirs(SORTED_MISSIONS_FOLDER, recursive = FALSE)

# Construct exiftool commands for the folder
cmds = paste0("exiftool -n -r -fast4 -overwrite_original -Orientation=1 ", folders)

# Run the commands in parallel
future::plan(future::multisession, workers = future::availableCores() * 1.9)
res = future_map(cmds, system, .progress = TRUE)

folders = "/ofo-share/drone-imagery-organization/2_sorted-sub-mission/000643-01"
cmds = paste0("exiftool -n -r -fast4 -overwrite_original -Orientation=1 ", folders)

system(cmds)
