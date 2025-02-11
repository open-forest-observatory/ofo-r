# Purpose: Read a CSV file containing the EXIF data for all images in a dataset (multiple missions),
# *at the mission level* (i.e., combining both dates of a two-date mission; both orientations of a
# two-part grid mission) and extract/process into human-readable metadata at the image level and
# sub-mission level using the metadata extraction functions of the ofo package.

# TODO: This script is very similar to script 07_extract-exif-metadata-sub-mission.R, but it
# operates at the mission level instead of the sub-mission level. Consider refactoring into (more)
# shared functions to reduce repetition.

library(tidyverse)
library(sf)

# devtools::document(); devtools::install()
library(ofo)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
FOLDER_BASEROW_CROSSWALK_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting/"
EXIF_PATH = "/ofo-share/drone-imagery-organization/3b_exif-unprocessed/"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

## Derived constants
exif_filepath = file.path(EXIF_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv"))
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))

# This per-image metadata was saved out in the last step
metadata_perimage_input_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))
# Data for the subset of images retained in both the mission polygons and sub-mission polygons are saved out here
metadata_perimage_subset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("exif-metadata_perimage_subset_", IMAGERY_PROJECT_NAME, ".csv"))

# Output files per mission or sub-mission
metadata_per_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
mission_polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))

metadata_per_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
sub_mission_polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))

## Functions
compute_polygons_and_images_retained = function(image_metadata, column_to_split_on) {
  # Split the metadata by the values in the requested column
  split_metadata = split(image_metadata, image_metadata[[column_to_split_on]])
  # Those unique values are used as the dataset_id for logging purposes
  dataset_ids = names(split_metadata)

  # Extract the polygons for each chunk of metadata
  polygons_and_inds = furrr::future_map2(
    split_metadata,
    dataset_ids,
    extract_mission_polygon,
    image_merge_distance = 50,
    identify_images_in_polygon = TRUE
  )
  polygons = dplyr::bind_rows(purrr::map(polygons_and_inds, "polygon"))
  intersection_idxs = unlist(purrr::map(polygons_and_inds, "intersection_idxs"))
  # Re-assemble the image metadata in the same order as the intersection IDs
  unsplit_metadata = dplyr::bind_rows(split_metadata)
  retained_image_IDs = unsplit_metadata[intersection_idxs, ]$image_id

  return(list(polygons = polygons, retained_image_IDs = retained_image_IDs))
}

compute_and_save_summary_statistics = function(
    image_metadata,
    polygons_perdataset,
    column_to_split_on,
    metadata_perdataset_filepath,
    polygons_filepath) {
  print("Started computing dataset-level summary statistics")
  future::plan("multisession")
  # Run dataset-level metadata extraction across each subset
  metadata_chunks_per_dataset = split(image_metadata, image_metadata[[column_to_split_on]])

  # Take only the polygons that have a non-zero number of images retained
  polygons_perdataset = polygons_perdataset[names(metadata_chunks_per_dataset)]

  # Extract the "dataset_id" from each metadata chunk
  dataset_ids = names(metadata_chunks_per_dataset)

  # Extract the summary statistics from each metadata chunk
  summaries_perdataset = furrr::future_pmap(
    list(metadata_chunks_per_dataset, polygons_perdataset, dataset_ids),
    extract_imagery_dataset_metadata,
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )
  print("Finished computing dataset-level summary statistics")
  # Extract the elements of the summary statistics
  summaries_perdataset = dplyr::bind_rows(summaries_perdataset)

  # Write out the results
  ## The per-dataset summary statistics
  # readr::write_csv(summaries_perdataset, metadata_perdataset_filepath)
  # Apply several steps to transform the polygons into the appropriate format. The polygons begin
  # as a named list, with the names corresponding to the dataset ID ("column_to_split_on"). The
  # goal is to convert it to a sf object with each row representing a different dataset ID and the
  # associated multi-polygon bounds.
  # Convert into a dataframe
  polygons_perdataset_df = do.call(rbind, polygons_perdataset)
  # And then a tibble
  polygons_perdataset_tbl = dplyr::as_tibble(polygons_perdataset_df)
  # The tibble no longer has the dataset IDs, so add those back
  polygons_perdataset_tbl[column_to_split_on] = row.names(polygons_perdataset_df)
  # Rename the unnamed column to geometry
  polygons_perdataset_tbl = dplyr::rename(polygons_perdataset_tbl, "geometry" = "V1")
  # Convert to a sf object and then write
  polygons_perdataset_sf = sf::st_sfc(polygons_perdataset_tbl)
  sf::st_write(polygons_perdataset_sf, polygons_filepath, delete_dsn = TRUE)
}

## Workflow
# Read in image-level metadata that was parsed in step 07
image_metadata = read_csv(metadata_perimage_input_filepath)

mission_res = compute_polygons_and_images_retained(
  image_metadata = image_metadata, column_to_split_on = "mission_id"
)
sub_mission_res = compute_polygons_and_images_retained(
  image_metadata = image_metadata, column_to_split_on = "sub_mission_id"
)

# Compute the images that were retained in both the mission polygons and the sub-mission polygons
images_retained_in_both = intersect(
  mission_res$retained_image_IDs, sub_mission_res$retained_image_IDs
)

# TODO consider reporting how many were droppped per dataset
# Filter the image metadata to only include data for those images
image_metadata = image_metadata |> filter(image_id %in% images_retained_in_both)

# Write out the the filtered subset
write_csv(image_metadata, metadata_perimage_subset_filepath)

# Create the output folder
create_dir(EXTRACTED_METADATA_PATH)
# Compute summary statistics and save out results for mission-level data
compute_and_save_summary_statistics(
  image_metadata = image_metadata,
  column_to_split_on = "mission_id",
  polygons_perdataset = mission_res$polygons,
  metadata_perdataset_filepath = metadata_per_mission_filepath,
  polygons_filepath = mission_polygons_filepath
)
# Compute summary statistics and save out results for sub-mission-level data
compute_and_save_summary_statistics(
  image_metadata = image_metadata,
  column_to_split_on = "sub_mission_id",
  polygons_perdataset = sub_mission_res$polygons,
  metadata_perdataset_filepath = metadata_per_sub_mission_filepath,
  polygons_filepath = sub_mission_polygons_filepath
)
