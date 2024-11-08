# Purpose: Take the photogrammetry products and compute the "deliverable" versions of the outputs by
# postprocessing. Perform at the mission level.

library(tidyverse)
library(sf)

METASHAPE_OUTPUTS_PATH = "/ofo-share/drone-imagery-processed/01/metashape-outputs/"
PHOTOGRAMMETRY_PUBLISH_PATH = "/ofo-share/drone-imagery-processed/01/photogrammetry-publish/"

## Functions



## Workflow

# Get all the mission names