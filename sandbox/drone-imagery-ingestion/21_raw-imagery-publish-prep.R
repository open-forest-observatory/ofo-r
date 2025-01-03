# Purpose: Take the raw imagery folders and compute the "deliverable" versions of the outputs by
# postprocessing. Perform at the mission level.

# TODO: This relies on an attribute of the image-level metadata called "received_image_path" that
# really represents the path of the image in the raw, organized imagery folder (i.e., likely
# different than as it was "received"). Upstream, we should change the name of this attribute (or
# add a new one that we use here instead) that is more descriptive of this.

library(tidyverse)
library(sf)
library(magick)
library(furrr)
library(ofo)

## Constants

# File paths

RAW_IMAGES_PATH = "/ofo-share/drone-imagery-organization/6_combined-across-projects"
RAW_IMAGES_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/all-sub-mission-points-w-metadata.gpkg"
MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/all-mission-polygons-w-metadata.gpkg"
PUBLISHABLE_IMAGES_PATH = "/ofo-share/drone-imagery-organization/7_to-publish"
IN_PROCESS_PATH = "/ofo-share/tmp/raw-imagery-publish-prep-progress-tracking/"

# Processing constants
N_EXAMPLE_IMAGES = 4
THUMBNAIL_SIZE = "512"
SKIP_EXISTING = TRUE # Skip processing for missions that already have all outputs


## Functions

# Function to do all the imagery prep for a given mission, with pre-subsetted metadata and footprint
imagery_publish_prep_mission = function(mission_id_foc, mission_images_metadata, mission_footprint) {

  cat("Processing mission", mission_id_foc, "\n")

  # Skip if the mission already has all outputs, asuming that if the zip file exists, the entire
  # mission was processed to completion
  zip_outpath = file.path(PUBLISHABLE_IMAGES_PATH, mission_id_foc, "images", "images.zip")

  if (SKIP_EXISTING && file.exists(zip_outpath)) {
    cat("Already exists. Skipping.\n")
    return()
  }

  # Save a file that indicates the mission is being processed
  processing_file = file.path(IN_PROCESS_PATH, paste0(mission_id_foc, ".csv"))
  fake_df = data.frame(a = 1, b = 1)
  write.csv(fake_df, processing_file)

  # Project mission image locs and footprint to the local UTM zone
  mission_images_metadata = transform_to_local_utm(mission_images_metadata)
  mission_footprint = transform_to_local_utm(mission_footprint)

  # In case one was in a different UTM zone than the other, reproject to the same zone
  mission_footprint = st_transform(mission_footprint, st_crs(mission_images_metadata))

  # Get the footprint area
  mission_footprint_area = mission_footprint |> st_area()

  # Buffer in the polygon by 100 m
  mission_footprint_buffered = mission_footprint |> st_buffer(-100)

  # Get the new area
  mission_footprint_buffered_area = mission_footprint_buffered |> st_area()

  # If the new area is < 40% of the original, use a smaller buffer, first trying 50 m, then 25 m,
  # then 0
  if (as.vector(mission_footprint_buffered_area / mission_footprint_area) < 0.4) {
    mission_footprint_buffered = mission_footprint |> st_buffer(-50)
    mission_footprint_buffered_area = mission_footprint_buffered |> st_area()
  }

  if (as.vector(mission_footprint_buffered_area / mission_footprint_area) < 0.4) {
    mission_footprint_buffered = mission_footprint |> st_buffer(-25)
    mission_footprint_buffered_area = mission_footprint_buffered |> st_area()
  }

  if (as.vector(mission_footprint_buffered_area / mission_footprint_area) < 0.4) {
    mission_footprint_buffered = mission_footprint
  }

  # Get the image locations within the buffered footprint
  mission_images_interior = mission_images_metadata |> st_intersection(mission_footprint_buffered)

  # Group points into 4 clusters
  coords = st_coordinates(mission_images_interior)
  clusts = kmeans(coords, centers = N_EXAMPLE_IMAGES)
  mission_images_interior$cluster = clusts$cluster

  # Get the cluster centroids, then the image point nearest each, to be the 4 representative images
  # from the mission
  selected_images = data.frame()
  for (i in 1:N_EXAMPLE_IMAGES) {
    cluster_pts = mission_images_interior |> filter(cluster == i)
    cluster_centroid = cluster_pts |> st_union() |> st_centroid()
    nearest_idx = st_nearest_feature(cluster_centroid, cluster_pts)
    nearest_pt = cluster_pts[nearest_idx, ]
    selected_images = rbind(selected_images, nearest_pt)
  }

  # Hardlink the selected images to the publishable folder
  inpaths = file.path(RAW_IMAGES_PATH, mission_id_foc, selected_images$received_image_path)
  extensions = tools::file_ext(inpaths)
  outpaths = file.path(PUBLISHABLE_IMAGES_PATH, mission_id_foc, "images", "examples", "fullsize", paste0("example_", 1:N_EXAMPLE_IMAGES, ".", extensions))

  outdirs = unique(dirname(outpaths))
  walk(outdirs, dir.create, recursive = TRUE)

  # Remove the file if it already exists
  exists = file.exists(outpaths)
  outpaths_remove = outpaths[exists]
  file.remove(outpaths_remove)

  file.link(inpaths, outpaths)

  # Create thumbnails of the images using magick package and save to the publishable folder
  for (i in 1:N_EXAMPLE_IMAGES) {
    img = image_read(inpaths[i])
    dim_string = paste0(THUMBNAIL_SIZE, "x", THUMBNAIL_SIZE)
    img = image_resize(img, dim_string)

    thumb_outpath = file.path(PUBLISHABLE_IMAGES_PATH, mission_id_foc, "images", "examples", "thumbnails", paste0("example_", i, ".", extensions[i]))

    outdirs = unique(dirname(thumb_outpath))
    walk(outdirs, dir.create, recursive = TRUE)

    image_write(img, thumb_outpath)
  }

  # Zip the entirety of the raw images folder and save to the publishable folder
  # Save to a tempfile while creating the zip, then move to the final location, because if the
  # process is terminated we don't want to leave a partial temp file in the file tree
  inpath = file.path(RAW_IMAGES_PATH, mission_id_foc)

  tempfile = paste0("/ofo-share/tmp/ofor_tempzip/tempzip_", mission_id_foc, ".zip")
  # Delete if exists
  if (file.exists(tempfile)) {
    file.remove(tempfile)
  }
  # Create dir
  dir.create(dirname(tempfile), recursive = TRUE)


  system(paste("zip -r -0", shQuote(tempfile), shQuote(inpath)), ignore.stdout = TRUE)
  file.rename(tempfile, zip_outpath)

  # Remove the file indicating the mission is being processed
  file.remove(processing_file)

  gc()

}

## Workflow

future::plan("multisession")
magick:::magick_threads(1)

# Read in the raw image locations and the mission footptings (both with metadata)
raw_images_metadata = st_read(RAW_IMAGES_METADATA_PATH)
mission_footprints = st_read(MISSION_FOOTPRINTS_PATH)

# Get all the mission IDs from the sub-mission
mission_ids = raw_images_metadata$dataset_id_image_level
mission_ids = unlist(lapply(mission_ids, function(x) {
  substring(x, 1, 6)
}))
raw_images_metadata$mission_id = mission_ids

unique_mission_ids = raw_images_metadata$mission_id |> unique()

# Split the raw images metadata and mission footprints by mission ID
mission_ids_to_run = unique_mission_ids
mission_images_metadata_list = list()
mission_footprints_list = list()
for (mission_id_foc in mission_ids_to_run) {

  # Get the mission metadata (for image locations)
  mission_images_metadata_list[[mission_id_foc]] = raw_images_metadata |> filter(mission_id == mission_id_foc)

  # Get the mission footprint
  mission_footprints_list[[mission_id_foc]] = mission_footprints |> filter(dataset_id == mission_id_foc)

}

# # Run the imagery prep for each mission
# pwalk(
#  list(
#    mission_id_foc = mission_ids_to_run,
#    mission_images_metadata = mission_images_metadata_list,
#    mission_footprint = mission_footprints_list
#  ),
#  imagery_publish_prep_mission
# )

input_list = list(
  mission_id_foc = mission_ids_to_run,
  mission_images_metadata = mission_images_metadata_list,
  mission_footprint = mission_footprints_list
)
future_pwalk(
  input_list,
  imagery_publish_prep_mission,
  .options = furrr_options(seed = TRUE,
                           scheduling = Inf))