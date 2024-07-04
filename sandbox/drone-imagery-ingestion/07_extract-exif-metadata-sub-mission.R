# Purpose: Read a CSV file containing the EXIF data for all images in a dataset (multiple missions),
# *at the sub-mission level* (i.e., a single date if a two-date mission; a single orientation of
# a two-part grid mission) and extract/process into human-readable metadata at the image level and
# sub-mission level using the metadata extraction functions of the ofo package.

library(tidyverse)
library(sf)

#devtools::document(); devtools::install()
library(ofo)

IMAGERY_PROJECT_NAME = "2019-focal"

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
FOLDER_BASEROW_CROSSWALK_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting/"
EXIF_PATH = "/ofo-share/drone-imagery-organization/3b_exif-unprocessed/"

EXTRACTED_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"


# Derived constants
exif_filepath = file.path(EXIF_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv"))
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))

metadata_perimage_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perimage_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_perdataset_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-exif-metadata_perdataset_", IMAGERY_PROJECT_NAME, ".csv"))
polygons_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-polygons_", IMAGERY_PROJECT_NAME, ".gpkg"))

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
exif$dataset_id = exif$submission_id

# Extract image-level metadata, which can occur across all missions at once becuase there are no
# hierarchical dependencies on mission-level data
metadata_perimage = extract_imagery_perimage_metadata(exif,
                                                      input_type = "dataframe")

# Assign image_id to the exif dataframe so it can be used in the dataset-level metadata extraction
exif$image_id = metadata_perimage$image_id

# For sub-mission-level metadata, run sub-mission by sub-mission
submissions = unique(exif$submission_id)

# For parallelizing, make a list of subsets of the exif dataframe, one for each sub-mission
exif_list <- lapply(submissions, function(submission) {
  exif_foc <- exif |>
    filter(submission_id == submission)
  return(exif_foc)
})

# Run dataset-level metadata extraction across each subset

future::plan("multisession")
res = furrr::future_map(exif_list,
                        extract_imagery_dataset_metadata,
                        input_type = "dataframe",
                        plot_flightpath = FALSE,
                        crop_to_contiguous = TRUE)

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
