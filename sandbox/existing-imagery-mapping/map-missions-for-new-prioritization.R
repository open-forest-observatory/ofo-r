# Purpose: Create a polygon shapeile for all missions, attributed with flight altitude (need to
# compute this from cameras XML and DTM) and pitch

library(tidyverse)
library(xml2)
library(terra)
library(sf)

PROCESSED_IMAGERY_DIR = "/ofo-share/drone-imagery-processed/01/metashape-outputs/"
RAW_IMAGES_DIR = "/ofo-share/drone-imagery-organization/2z_sorted-notcleaned-combined/"
EXIF_DIR = "/ofo-share/drone-imagery-organization/ancillary/extracted-exif-2z/"

# devtools::load_all()
devtools::document()
devtools::install()
library(ofo)

# Processing

# Get the list of datasets
files = list.files(PROCESSED_IMAGERY_DIR, pattern = "report.pdf$", recursive = FALSE, full.names = FALSE)
files
dataset_runs = str_sub(files, 1, 20) |> unique()

dataset_run = dataset_runs[1]

get_poly_alt_pitch = function(dataset_run, processed_imagery_dir, exif_dir) {

  dataset = str_sub(dataset_run, 1, 6)

  alt = get_mission_agl(dataset_run, processed_imagery_dir)

  # Get the exif for this mission
  exif_file = file.path(exif_dir, paste0("exif_", dataset, ".csv"))
  exif = prep_exif(exif_file)

  # Create the polygon
  poly = create_mission_polygon(exif, image_merge_distance = 50) |> st_as_sf()

  # Get gimbal pitch
  pitch = extract_camera_pitch(exif)
  poly$camera_pitch = pitch
  poly = bind_cols(poly, alt)

  return(poly)

}

# polys_list = list()
# for (i in 1:length(dataset_runs)) {
#   cat("Running dataset ", i, " of ", length(dataset_runs), "\n")
#   polys_list[i] = get_poly_alt_pitch(dataset_runs[i], PROCESSED_IMAGERY_DIR, EXIF_DIR)

# }



future::plan("multisession")
polys_list = furrr::future_map(dataset_runs, get_poly_alt_pitch, processed_imagery_dir = PROCESSED_IMAGERY_DIR, exif_dir = EXIF_DIR)

polys = bind_rows(polys_list)

# Check why getting som NA AGL CVs and camera_pitches