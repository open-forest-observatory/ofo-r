# Purpose: For all CHMs within a specified mission ID range, detect treetops and save results to a
# file.

library(tidyverse)
library(sf)
library(lidR)
library(terra)
library(nngeo)
library(smoothr)
library(furrr)

## Constants

TREE_DETECTION_PUBLISH_PATH = "/ofo-share/drone-imagery-processed/01/tree-detection-publish"

PUBLISHED_DATA_RECORDS_DIR = "/ofo-share/drone-imagery-organization/8_published-data-records"
# ^ this is to know which CHMs have been processed and where they are on CyVerse

CYVERSE_BASE_PATH = "shared/ofo/public/missions"

CYVERSE_BASE_URL = "https://data.cyverse.org/dav-anon/iplant/projects/ofo/public/missions/"

MISSIONS_TO_PROCESS = c(1:452, 643:811, 874, 875, 932:1313) # This is everything except NRS

ITD_PARAMETERIZATION_ID = 1


## Derived constants

# Pad mission IDs with zeros to 6 digits
missions_to_process = str_pad(MISSIONS_TO_PROCESS, width = 6, pad = "0")

# Pad ITD ID with zeros to 4 digits
itd_parameterization_id = str_pad(ITD_PARAMETERIZATION_ID, width = 4, pad = "0")


## Functions

# Function to create a variable radius window function for LMF
make_win_fun <- function(a, b, c, diam_min, diam_max) {
  win_fun <- function(x) {
    win <- a + b*x + c*x^2
    win[win < diam_min] = diam_min
    win[win > diam_max] = diam_max
    return(win)
  }
  return(win_fun)
}

# Function to resample and smooth a CHM
resample_and_smooth_chm = function(chm, res, smooth_width) {
  chm_resamp <- terra::project(chm, terra::crs(chm), res = res, method = "bilinear")
  chm_smooth <- terra::focal(chm_resamp, w = matrix(1, smooth_width, smooth_width), mean, na.rm = TRUE)
  return(chm_smooth)
}

# Function to predict trees from a prepped (resampled and smoothed) CHM
predict_trees_from_chm <- function(chm, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) {
  win_fun <- make_win_fun(lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max)
  ttops <- lidR::locate_trees(chm, algorithm = lmf(ws = win_fun, shape = "circular", hmin = 5), )
  return(ttops)
}



#### Workflow

# Load the list of missions on CyVerse (i.e., all that have a CHM produced for them)
chm_files <- read_csv(file.path(PUBLISHED_DATA_RECORDS_DIR, "chm_mesh_files.csv"), col_types = "cccccc")

# Define the parameters for the lmf algorithm (later could be pulled in dynamically)
chm_res = 0.25
chm_smooth_width = 7
lmf_a = 0
lmf_b = 0.11
lmf_c = 0
lmf_diam_min = 0.5
lmf_diam_max = 100

# lmf_a = -1
# lmf_b = 0.1093

chm_files_to_process = chm_files |> filter(mission_id %in% missions_to_process)

detect_ttops_and_crowns = function(chm_file_foc) {

  # Create the output directory
  out_folder = file.path(TREE_DETECTION_PUBLISH_PATH,
                          chm_file_foc$mission_id,
                          paste0("processed-", chm_file_foc$processing_run_id),
                          paste0("itd-", itd_parameterization_id))
  if (!dir.exists(out_folder)) dir.create(out_folder, recursive = TRUE)

  # Download and load the CHM
  url = paste0(CYVERSE_BASE_URL, chm_file_foc$filepath_rel)
  tempfile = tempfile("chm", fileext = ".tif")
  x = download.file(url, tempfile, method = "wget")
  chm = terra::rast(tempfile)


  # Prep the CHM
  chm_smooth = resample_and_smooth_chm(chm, chm_res, chm_smooth_width)

  # Detect trees
  ttops = predict_trees_from_chm(chm_smooth, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max)

  # # Write both to inspect
  # tempfolder = "/ofo-share/scratch-derek/temp"
  # if (!dir.exists(tempfolder)) dir.create(tempfolder, recursive = TRUE)
  # st_write(ttops, file.path(tempfolder, "ttops_smooth3_disc.gpkg"), delete_dsn = TRUE)
  # writeRaster(chm_smooth, file.path(tempfolder, "chm_smooth.tif"), overwrite = TRUE)

  # Extract tree height from the non-smoothed CHM
  ttops$Z = extract(chm, ttops)[,2]

  # For removing edge trees:
  # Get the bounds of the CHM
  chm_non_na = chm
  chm_non_na[!is.na(chm)] = 1
  chm_poly = as.polygons(chm_non_na, values = FALSE)

  # Buffer in by 10 m
  chm_poly_buff = buffer(chm_poly, width = -10)

  # Delineate crowns: silva
  crowns_silva = lidR::silva2016(chm_smooth, ttops, max_cr_factor = 0.24, exclusion = 0.1)()
  crowns_silva <- as.polygons(crowns_silva)
  crowns_silva <- st_as_sf(crowns_silva)
  crowns_silva <- st_cast(crowns_silva, "MULTIPOLYGON")
  crowns_silva <- st_cast(crowns_silva, "POLYGON")
  crowns_silva <- st_remove_holes(crowns_silva)
  crowns_silva <- st_make_valid(crowns_silva)
  crowns_silva <- smooth(crowns_silva, method = "ksmooth", smoothness = 3)
  crowns_silva <- st_simplify(crowns_silva, preserveTopology = TRUE, dTolerance = 0.1)

  crowns_watershed = lidR::watershed(chm_smooth, th_tree = 2, tol = 0, ext = 1)()
  crowns_watershed <- as.polygons(crowns_watershed)
  crowns_watershed <- st_as_sf(crowns_watershed)
  crowns_watershed <- st_cast(crowns_watershed, "MULTIPOLYGON")
  crowns_watershed <- st_cast(crowns_watershed, "POLYGON")
  crowns_watershed <- st_remove_holes(crowns_watershed)
  crowns_watershed <- st_make_valid(crowns_watershed)
  crowns_watershed <- smooth(crowns_watershed, method = "ksmooth", smoothness = 3)
  crowns_watershed <- st_simplify(crowns_watershed, preserveTopology = TRUE, dTolerance = 0.1)

  # Crop the ttops
  ttops = st_intersection(ttops, chm_poly_buff |> st_as_sf())

  # Assign crowns the treetop height and remove those that have no treetops in them
  crowns_silva = st_join(crowns_silva, ttops)
  crowns_silva = crowns_silva[, -1]
  crowns_silva = crowns_silva[!is.na(crowns_silva$Z),]

  crowns_watershed = st_join(crowns_watershed, ttops)
  crowns_watershed = crowns_watershed[, -1]
  crowns_watershed = crowns_watershed[!is.na(crowns_watershed$Z),]

  # Write predicted treetops and crowns
  st_write(ttops, file.path(out_folder, "treetops.gpkg"), delete_dsn = TRUE)
  st_write(crowns_watershed, file.path(out_folder, "crowns_watershed.gpkg"), delete_dsn = TRUE)
  st_write(crowns_silva, file.path(out_folder, "crowns_silva.gpkg"), delete_dsn = TRUE)

  gc()
  file.remove(tempfile)
}

# Turn chm_files_to_process into a list
chm_files_list = split(chm_files_to_process, seq_len(nrow(chm_files_to_process)))

plan(multicore)

# Process each CHM
future_walk(chm_files_list, detect_ttops_and_crowns, .progress = TRUE)
