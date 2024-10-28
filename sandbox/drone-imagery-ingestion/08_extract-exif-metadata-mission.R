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

IMAGERY_PROJECT_NAME = "2020-ucnrs"

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
FOLDER_BASEROW_CROSSWALK_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting/"
EXIF_PATH = "/ofo-share/drone-imagery-organization/3b_exif-unprocessed/"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"


## Derived constants
exif_filepath = file.path(EXIF_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv"))
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))

metadata_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_perdataset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))

# The already exported sub-mission image-level metadata from previous workflow step (to select only
# those images that were retained as a part of a sub-mission)
metadata_perimage_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))

## Workflow

# Read in the EXIF
exif = prep_exif(exif_filepath, plot_flightpath = FALSE)

# Format columns
exif = exif |>
  mutate(mission_id = str_pad(mission_id, 6, pad = "0", side = "left"))

# Assign the "dataset_id" parameter that is used in the metadata extraction functions. This is done
# here as opposed to in the metadata extraction to keep those functions flexible as to how a dataset
# is defined (e.g. a "mission" or a "sub-mission"). Here we are defining a dataset as a
# "sub-mission".
exif$dataset_id = exif$mission_id

# Extract image-level metadata, which can occur across all missions at once becuase there are no
# hierarchical dependencies on mission-level data
metadata_perimage = extract_imagery_perimage_metadata(exif,
                                                      input_type = "dataframe")

# Assign image_id to the exif dataframe so it can be used in the dataset-level metadata extraction
exif$image_id = metadata_perimage$image_id

# Filter the exif dataframe and extracted per-image metadata to include only images that were
# retained in the previous workflow step of sub-mission-level metadata extraction
metadata_perimage_sub_mission = read_csv(metadata_perimage_sub_mission_filepath)
image_ids_retained = metadata_perimage_sub_mission$image_id
exif = exif |>
  filter(image_id %in% image_ids_retained)
metadata_perimage = metadata_perimage |>
  filter(image_id %in% image_ids_retained)


# For sub-mission-level metadata, run sub-mission by sub-mission
missions = unique(exif$mission_id)

# For parallelizing, make a list of subsets of the exif dataframe, one for each sub-mission
exif_list <- lapply(missions, function(mission) {
  exif_foc <- exif |>
    filter(mission_id == mission)
  return(exif_foc)
})

# Run dataset-level metadata extraction across each subset

future::plan("multisession")
res = furrr::future_map(exif_list,
                        extract_imagery_dataset_metadata,
                        input_type = "dataframe",
                        plot_flightpath = FALSE,
                        crop_to_contiguous = TRUE,
                        min_contig_area = 10000,
                        .options = furrr_options(seed = TRUE))

metadata_list = map(res, ~.x$dataset_metadata)
polygon_list = map(res, ~.x$mission_polygon)
images_retained_list = map(res, ~.x$images_retained)

metadata_perdataset = bind_rows(metadata_list)
polygon_perdataset = bind_rows(polygon_list)
images_retained = unlist(images_retained_list)

# Filter the extracted metadata to only include images that were retained in the dataset-level
# metadata extraction based on intersection with the mission polygon
metadata_perimage = metadata_perimage |>
  filter(image_id %in% images_retained)

# Save the metadata

folders = c(metadata_perimage_filepath, metadata_perdataset_filepath, polygons_filepath)
folders = dirname(folders)
purrr::walk(folders,
            create_dir)

write_csv(metadata_perimage, metadata_perimage_filepath)
write_csv(metadata_perdataset, metadata_perdataset_filepath)
st_write(polygon_perdataset, polygons_filepath, delete_dsn = TRUE)
