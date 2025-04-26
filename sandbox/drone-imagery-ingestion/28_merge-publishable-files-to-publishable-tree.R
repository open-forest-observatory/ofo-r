# Purpose: Take the imagery, footprint, and photogrammetry products for each mission and hardlink
# them all into a single file tree that is ready to be published (transferred to cyverse).

library(purrr)

## Constants

# File paths

PUBLISHABLE_MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-processed/01/mission-footprints-publish"
# PUBLISHABLE_SUB_MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-processed/01/sub-mission-footprints-publish"
PUBLISHABLE_MISSION_POINTS_PATH = "/ofo-share/drone-imagery-processed/01/mission-points-publish"
PUBLISHABLE_IMAGES_PATH = "/ofo-share/drone-imagery-organization/7_to-publish"
PHOTOGRAMMETRY_PUBLISH_PATH = "/ofo-share/drone-imagery-processed/01/photogrammetry-publish"
ITD_PUBLISH_PATH = "/ofo-share/drone-imagery-processed/01/tree-detection-publish"

PUBLISHABLE_DATA_TREE = "/ofo-share/drone-data-publish/01/"


# Processing constants



## Functions


## Workflow

# MISSION FOOTPRINTS

# Hardlink all files from the publishable mission footprints dir to the unified publishable tree
# First make the directories
infiles = list.files(PUBLISHABLE_MISSION_FOOTPRINTS_PATH, full.names = FALSE, recursive = TRUE)
infiles_full = file.path(PUBLISHABLE_MISSION_FOOTPRINTS_PATH, infiles)
outfiles = file.path(PUBLISHABLE_DATA_TREE, infiles)
outdirs = unique(dirname(outfiles))

walk(outdirs, dir.create, recursive = TRUE)

file.link(infiles_full, outfiles)

# # SUB-MISSION FOOTPRINTS

# # Hardlink all files from the publishable sub-mission footprints dir to the unified publishable tree
# # First make the directories
# infiles = list.files(PUBLISHABLE_SUB_MISSION_FOOTPRINTS_PATH, full.names = FALSE, recursive = TRUE)
# infiles_full = file.path(PUBLISHABLE_SUB_MISSION_FOOTPRINTS_PATH, infiles)
# outfiles = file.path(PUBLISHABLE_DATA_TREE, infiles)
# outdirs = unique(dirname(outfiles))
# walk(outdirs, dir.create, recursive = TRUE)
# file.link(infiles_full, outfiles)

# MISSION POINTS

# Hardlink all files from the publishable mission points dir to the unified publishable tree
# First make the directories
infiles = list.files(PUBLISHABLE_MISSION_POINTS_PATH, full.names = FALSE, recursive = TRUE)
infiles_full = file.path(PUBLISHABLE_MISSION_POINTS_PATH, infiles)
outfiles = file.path(PUBLISHABLE_DATA_TREE, infiles)
outdirs = unique(dirname(outfiles))
walk(outdirs, dir.create, recursive = TRUE)
file.link(infiles_full, outfiles)


# IMAGES

# Hardlink all files from the publishable images dir to the unified publishable tree
# First make the directories
infiles = list.files(PUBLISHABLE_IMAGES_PATH, full.names = FALSE, recursive = TRUE)
infiles_full = file.path(PUBLISHABLE_IMAGES_PATH, infiles)
outfiles = file.path(PUBLISHABLE_DATA_TREE, infiles)
outdirs = unique(dirname(outfiles))

walk(outdirs, dir.create, recursive = TRUE)

file.link(infiles_full, outfiles)


# PHOTOGRAMMETRY

# Hardlink all files from the publishable photogrammetry dir to the unified publishable tree
# First make the directories
infiles = list.files(PHOTOGRAMMETRY_PUBLISH_PATH, full.names = FALSE, recursive = TRUE)
infiles_full = file.path(PHOTOGRAMMETRY_PUBLISH_PATH, infiles)
outfiles = file.path(PUBLISHABLE_DATA_TREE, infiles)
outdirs = unique(dirname(outfiles))

walk(outdirs, dir.create, recursive = TRUE)

file.link(infiles_full, outfiles)


# ITD

# Hardlink all files from the publishable ITD dir to the unified publishable tree
# First make the directories
infiles = list.files(ITD_PUBLISH_PATH, full.names = FALSE, recursive = TRUE)
infiles_full = file.path(ITD_PUBLISH_PATH, infiles)
outfiles = file.path(PUBLISHABLE_DATA_TREE, infiles)
outdirs = unique(dirname(outfiles))

walk(outdirs, dir.create, recursive = TRUE)

file.link(infiles_full, outfiles)
