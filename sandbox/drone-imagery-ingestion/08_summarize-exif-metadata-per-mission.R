# Purpose: Read a CSV file containing the EXIF data for all images in a dataset (multiple missions),
# *at the mission level* (i.e., combining both dates of a two-date mission; both orientations of a
# two-part grid mission) and extract/process into human-readable metadata at the image level and
# sub-mission level using the metadata extraction functions of the ofo package.

library(tidyverse)
library(sf)

# devtools::document()
# devtools::install()
library(ofo)

IMAGE_MERGE_DISTANCE = 50


# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)

# For NRS datasets, use a larger merge distance because some datasets used very low overlap
if (IMAGERY_PROJECT_NAME %in% c("2020-ucnrs", "2023-ucnrs", "2024-ucnrs")) {
  IMAGE_MERGE_DISTANCE = 100
}

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

## Derived constants

# This per-image metadata was saved out in the last step
metadata_perimage_input_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))

# Output files per mission or sub-mission
metadata_per_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
mission_polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))

metadata_per_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
sub_mission_polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))

# Output data for the subset of images retained in both the mission polygons and sub-mission polygons are saved out here
metadata_perimage_subset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("exif-metadata_perimage_subset_", IMAGERY_PROJECT_NAME, ".csv"))


## Functions
compute_polygons_and_images_retained = function(image_metadata, column_to_split_on, image_merge_distance) {
  # Split the metadata by the values in the requested column
  split_metadata = split(image_metadata, image_metadata[[column_to_split_on]])
  # Those unique values are used as the dataset_id for logging purposes
  dataset_ids = names(split_metadata)

  # Extract the polygons for each chunk (mission or sub-mission) of metadata
  polygons_and_inds = furrr::future_map2(
    split_metadata,
    dataset_ids,
    extract_mission_polygon,
    image_merge_distance = image_merge_distance,
    identify_images_in_polygon = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )

  # get count of retained images per dataset
  intersection_image_ids = purrr::map(polygons_and_inds, "intersection_image_ids")
  image_counts = purrr::map(intersection_image_ids, length)

  # get the ones that had < 10 images retained
  too_few_images_bool = image_counts < 10

  if (any(too_few_images_bool)) {
    too_few_images_names = names(which(too_few_images_bool))
    too_fes_images_names_string = paste(too_few_images_names, collapse = ", ")
    warning(
      paste0("The following missions/sub-missions had fewer than 10 images retained in the computed polygons and were dropped entirely: ", too_fes_images_names_string)
    )
  }

  # Drop the polygons (and image IDs) that had too few images retained
  polygons_and_inds = polygons_and_inds[!too_few_images_bool]

  polygons_sfc = purrr::map(polygons_and_inds, "polygon")
  polygons_sf = purrr::map(polygons_sfc, st_as_sf)
  intersection_image_ids = purrr::map(polygons_and_inds, "intersection_image_ids")

  intersection_image_ids = unlist(intersection_image_ids, use.names = FALSE)

  return(list(polygons = polygons_sf, retained_image_IDs = intersection_image_ids))
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
  # Add the identifying column
  summaries_perdataset[column_to_split_on] = dataset_ids
  # Write out
  readr::write_csv(summaries_perdataset, metadata_perdataset_filepath)

  # Transform the polygons into the appropriate format. The polygons begin as a named list, with the
  # names corresponding to the dataset ID ("column_to_split_on"). The goal is to convert it to a sf
  # object with each row representing a different dataset ID and the associated multi-polygon
  # bounds.

  polygons_perdataset_sf = dplyr::bind_rows(polygons_perdataset)
  polygons_perdataset_sf[, column_to_split_on] = names(polygons_perdataset)

  # TODO: ^ the geometry col is named 'x', may need to rename 'geometry', or may not be necessary
  # since it seems to save correctly

  # TODO: ^ in the future we might want to ensure that all the polygons have the same CRS but for
  # now they should all be EPSG::4326, and the bind_rows step should error out if they don't

  sf::st_write(polygons_perdataset_sf, polygons_filepath, delete_dsn = TRUE)
}

## Workflow
# Read in image-level metadata that was parsed in step 07
image_metadata = read_csv(metadata_perimage_input_filepath)

mission_res = compute_polygons_and_images_retained(
  image_metadata = image_metadata,
  column_to_split_on = "mission_id",
  image_merge_distance = IMAGE_MERGE_DISTANCE
)
sub_mission_res = compute_polygons_and_images_retained(
  image_metadata = image_metadata,
  column_to_split_on = "sub_mission_id",
  image_merge_distance = IMAGE_MERGE_DISTANCE
)

# Compute the images that were retained in both the mission polygons and the sub-mission polygons
images_retained_in_both = intersect(
  mission_res$retained_image_IDs, sub_mission_res$retained_image_IDs
)

# Filter the image metadata to only include data for those images
# TODO consider reporting how many were droppped per dataset
image_metadata = image_metadata |> filter(image_id %in% images_retained_in_both)

# Write out the the filtered subset
write_csv(image_metadata, metadata_perimage_subset_filepath)

# Re-compute the polygons now that extraneous images have been filtered out
mission_res = compute_polygons_and_images_retained(
  image_metadata = image_metadata,
  column_to_split_on = "mission_id",
  image_merge_distance = IMAGE_MERGE_DISTANCE
)
sub_mission_res = compute_polygons_and_images_retained(
  image_metadata = image_metadata,
  column_to_split_on = "sub_mission_id",
  image_merge_distance = IMAGE_MERGE_DISTANCE
)

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
