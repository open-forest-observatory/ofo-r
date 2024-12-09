# Purpose: Read a CSV file containing the EXIF data for all images in a dataset (multiple missions),
# *at the sub-mission level* (i.e., a single date if a two-date mission; a single orientation of
# a two-part grid mission) and extract/process into human-readable metadata at the image level and
# sub-mission level using the metadata extraction functions of the ofo package.

library(tidyverse)
library(sf)
library(ggplot2)
library(concaveman)

# devtools::document()
# devtools::install("/ofo-share/repos-david/ofo-r", quick = TRUE)
library(ofo)

IMAGERY_PROJECT_NAME = "2020-ucnrs"

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
metadata_sub_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("sub-mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))
metadata_mission_filepath = file.path(EXTRACTED_METADATA_PATH, paste0("mission-baserow-metadata_", IMAGERY_PROJECT_NAME, ".csv"))

## Workflow

# Read in the EXIF
exif = read.csv(exif_filepath)
# Read in the baserow metadata
baserow_sub_mission_metadata = read.csv(metadata_sub_mission_filepath)

# Assign the "dataset_id" parameter that is used in the metadata extraction functions. This is done
# here as opposed to in the metadata extraction to keep those functions flexible as to how a dataset
# is defined (e.g. a "mission" or a "sub-mission"). Here we are defining a dataset as a
# "sub-mission".
exif$dataset_id = exif$submission_id
# Pad the mission ID
exif = exif |> mutate(mission_id = str_pad(mission_id, 6, pad = "0", side = "left"))

# Compute the unique sub mission IDs
submissions = unique(exif$submission_id)

# For parallelizing, make a list of subsets of the exif dataframe, one for each sub-mission
exif_list <- lapply(submissions, function(submission) {
  exif_foc <- exif |>
    filter(submission_id == submission)
  return(list(submission = submission, exif_foc = exif_foc))
})

# Compute the corresponding
exif_list <- lapply(exif_list, function(submission_exif) {
  sub_mission_id = submission_exif[[1]]
  sub_mission_exif = submission_exif[[2]]

  baserow_for_sub_mission <- baserow_sub_mission_metadata[
    baserow_sub_mission_metadata$sub_mission_id == sub_mission_id,
  ]
  if (nrow(baserow_for_sub_mission) != 1) {
    # TODO make this an actual error
    print("Error: there was not one corresponding baserow entry")
    print(baserow_for_sub_mission)
  }
  return(
    list(
      aircraft_model_name = baserow_for_sub_mission$aircraft_model_name,
      sub_mission_exif = sub_mission_exif,
      sub_mission_id = sub_mission_id
    )
  )
})

extract_perimage_metadata = function(submission_and_exif) {
  aircraft_model_name = submission_and_exif$aircraft_model_name
  exif = submission_and_exif$sub_mission_exif

  if (aircraft_model_name %in% c(
    "Phantom 4 Pro v2.0",
    "Phantom 4 Advanced",
    "Mavic 3 Multispectral",
    "Phantom 4 RTK",
    "Phantom 4 Standard",
    "Matrice 210 RTK",
    "Matrice 100",
    "Matrice 300",
    "eBee X" # As far as I can tell, there is no issue with this using the DJI parser
  )) {
    return(extract_imagery_perimage_metadata_generic(exif))
  } else {
    stop(paste0("Aircraft model name \'", aircraft_model_name, "\' not supported"))
    return(NULL)
  }
}


metadata_per_sub_dataset = furrr::future_map(exif_list, extract_perimage_metadata)
metadata_perimage = bind_rows(metadata_per_sub_dataset)
print("Finished processing per-sub-mission datasets")

res = furrr::future_map(metadata_per_sub_dataset, extract_imagery_dataset_metadata)
print("Finished processing dataset-level metadata")

metadata_list = map(res, ~ .x$dataset_metadata)
polygon_list = map(res, ~ .x$mission_polygon)
images_retained_list = map(res, ~ .x$images_retained)

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

purrr::walk(
  folders,
  create_dir
)

write_csv(metadata_perimage, metadata_perimage_filepath)
write_csv(metadata_perdataset, metadata_perdataset_filepath)
st_write(polygon_perdataset, polygons_filepath, delete_dsn = TRUE)
