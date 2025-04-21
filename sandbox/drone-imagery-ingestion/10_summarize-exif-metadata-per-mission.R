# Purpose: Read a CSV file containing the EXIF data for all images in a mission,
# *at the mission level* (i.e., combining both dates of a two-date mission; both orientations of a
# two-part grid mission), and extract/process into human-readable metadata at the mission level and
# sub-mission level using the metadata summarization functions of the ofo package.

library(tidyverse)
library(sf)
library(furrr)

# devtools::document()
# devtools::install()
library(ofo)

IMAGE_MERGE_DISTANCE = 50

# If processing NRS datasets, use a larger merge distance because some datasets used very low overlap
# IMAGE_MERGE_DISTANCE = 100

# In
MISSIONS_TO_PROCESS_LIST_PATH = file.path("sandbox", "drone-imagery-ingestion", "missions-to-process.csv")
PARSED_EXIF_METADATA_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/4_parsed-exif"

# Out
PARSED_EXIF_FOR_RETAINED_IMAGES_PATH = "/ofo-share/drone-imagery-organization/metadata/3_final/3_parsed-exif-per-image"
DERIVED_METADATA_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/6_derived-metadata-per-mission"
DERIVED_METADATA_SUB_MISSION_PATH = "/ofo-share/drone-imagery-organization/metadata/2_intermediate/7_derived-metadata-per-sub-mission"


## Functions
compute_polygons_and_images_retained = function(image_metadata, column_to_split_on, image_merge_distance) {
  # Split the metadata by the values in the requested column. Note that for the mission level, this
  # approach is overkill because it will only be passed one mission of metadata at a time. It is a
  # vestige of a former way it was being applied. But it is still useful for the sub-mission level.
  split_metadata = split(image_metadata, image_metadata[[column_to_split_on]])
  # Those unique values are used as the dataset_id for logging purposes
  dataset_ids = names(split_metadata)

  # Extract the polygons for each chunk (mission or sub-mission) of metadata
  polygons_and_inds = purrr::map2(
    split_metadata,
    dataset_ids,
    extract_mission_polygon,
    image_merge_distance = image_merge_distance,
    identify_images_in_polygon = TRUE
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


## Workflow

# Determine which missions to process
missions_to_process = read_csv(MISSIONS_TO_PROCESS_LIST_PATH) |>
  pull(mission_id)

# Create the output folder
create_dir(DERIVED_METADATA_MISSION_PATH)
create_dir(DERIVED_METADATA_SUB_MISSION_PATH)
create_dir(PARSED_EXIF_FOR_RETAINED_IMAGES_PATH)


summarize_exif = function(mission_id_foc) {
  # Read in image-level metadata that was parsed in previous script
  metadata_perimage_input_filepath = file.path(PARSED_EXIF_METADATA_PATH, paste0(mission_id_foc, ".csv"))
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

  # Make the images geospatial and write
  metadata_perimage_subset_filepath = file.path(PARSED_EXIF_FOR_RETAINED_IMAGES_PATH, paste0(mission_id_foc, ".gpkg"))
  image_metadata_sf = image_metadata |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  st_write(image_metadata_sf, metadata_perimage_subset_filepath, delete_dsn = TRUE)

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


  # Extract the summary statistics for the mission
  summary_mission = extract_imagery_dataset_metadata(
    metadata = image_metadata,
    # only one set of polygons is expected, since only passed one mission of data, so take the first
    # index
    mission_polygon = mission_res$polygons[[1]],
    dataset_id = mission_id_foc
  )

  # Attribute the polygon with the metadata
  mission_poly = mission_res$polygons[[1]]
  mission_poly_attributed = bind_cols(mission_poly, summary_mission)

  # Give it a mission ID column
  mission_poly_attributed$mission_id = mission_id_foc

  # Write
  metadata_per_mission_filepath = file.path(DERIVED_METADATA_MISSION_PATH, paste0(mission_id_foc, ".gpkg"))
  st_write(
    mission_poly_attributed,
    metadata_per_mission_filepath,
    delete_dsn = TRUE
  )

  # Now repeat for each sub-mission within the mission
  sub_mission_ids = unique(image_metadata$sub_mission_id)

  for (sub_mission_id_foc in sub_mission_ids) {
    # Pull the metadata for the imges from this sub-mission
    image_metadata_sub_mission = image_metadata |> filter(sub_mission_id == sub_mission_id_foc)

    # Pull the polygon for this sub-mission
    polygon_sub_mission_foc = sub_mission_res$polygons[[sub_mission_id_foc]]

    # Extract the summary statistics for this sub-mission
    summary_sub_mission = extract_imagery_dataset_metadata(
      metadata = image_metadata_sub_mission,
      mission_polygon = polygon_sub_mission_foc,
      dataset_id = sub_mission_id_foc
    )

    # Attribute the polygon with the metadata
    sub_mission_poly_attributed = bind_cols(polygon_sub_mission_foc, summary_sub_mission)

    # Give it a mission ID and sub-mission ID column
    sub_mission_poly_attributed$mission_id = mission_id_foc
    sub_mission_poly_attributed$sub_mission_id = sub_mission_id_foc

    metadata_per_sub_mission_filepath = file.path(DERIVED_METADATA_SUB_MISSION_PATH, paste0(sub_mission_id_foc, ".gpkg"))

    # Write
    st_write(
      sub_mission_poly_attributed,
      metadata_per_sub_mission_filepath,
      delete_dsn = TRUE
    )
  }
}

# Run for each mission_id
future::plan(multisession)
future_walk(
  missions_to_process,
  summarize_exif,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE)
)
