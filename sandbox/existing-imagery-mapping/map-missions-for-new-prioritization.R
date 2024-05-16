# Purpose: Create a polygon shapeile for all missions, attributed with flight altitude (need to
# compute this from cameras XML and DTM) and pitch

library(dplyr)

PROCESSED_IMAGERY_DIR = "/ofo-share/drone-imagery-processed/01/metashape-outputs/"
RAW_IMAGES_DIR = "/ofo-share/drone-imagery-organization/2z_sorted-notcleaned-combined/"


devtools::load_all()

# Processing

# Get the list of datasets
files = list.files(PROCESSED_IMAGERY_DIR, recursive = FALSE, full.names = FALSE)
files
