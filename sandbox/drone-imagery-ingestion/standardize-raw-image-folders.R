# Purpose: Read in the manually cleaned drone images in mission folders and write them back out in a
# standardized folder structure with standaridzed file names

library(tidyverse)
library(exifr)
library(furrr)

IMAGERY_INPUT_PATH = "/ofo-share/drone-imagery-organization/1_manually-cleaned/2023-ny-ofo/"
IMAGERY_OUTPUT_PATH = "/ofo-share/drone-imagery-organization/2_standardized-preclean/2023-ny-ofo/"
BASEROW_DATA_PATH = "/ofo-share/scratch-derek/standardize-image-folders_data/baserow/"



## END CONSTANTS

## Prep baserow

# Load baserow records
baserow = read_csv(file.path(BASEROW_DATA_PATH, "export - datasets-imagery.csv"))
dataset_associations = read.table(file.path(BASEROW_DATA_PATH, "export - dataset-associations.csv"), sep = "\t", header = TRUE)
if ("notes" %in% colnames(dataset_associations)) stop("You need to export the dataset associations table without comments because they screw up the cell delimitations for some reason")

split_to_list = function(x) {
  a = str_split(x, ",")
  b = map(a, list)
  return(b)
}

# Merge the dataset assoc ID into the main baserow records
assoc = dataset_associations |>
  # Separate the multiple dataset IDs (currently in a column separated by commas) into a list column
  mutate(dataset_ids = split_to_list(dataset_ids))

# TODO: Make this more efficient
baserow$association_id = NA
for (i in 1:nrow(baserow)) {

  row = baserow[i, ]

  # Is this row associated with other rows
  for (j in 1:nrow(assoc)) {

    assoc_foc = assoc[j, ]
    assoc_id = assoc_foc$assoc_id
    assoc_dataset_ids = assoc_foc$dataset_ids[[1]][[1]]

    if (row$dataset_id %in% assoc_dataset_ids) {

      # If this row already had an association ID, then update all the associated rows to have the new
      # association ID
      if (!is.na(row$association_id)) {
        existing_assoc_id = row$association_id
        baserow[which(baserow$association_id == existing_assoc_id), "association_id"] = assoc_id
      }

      baserow[i, "association_id"] = assoc_id
    }
  }
}


# Create a column for the canonical dataset ID (from dataset_id_old if populated, otherwise from
# dataset_id)
b = baserow |>
  # Remove a database row ID (not an actual dataset ID)
  select(-id) |>
  mutate(across(contains("dataset_id"), as.character)) |>
  mutate(uses_old_id = !is.na(dataset_id_old)) |>
  # Remove the first part of the ID (the date, separated from the actual ID by a "-")
  mutate(dataset_id_old_trimmed = str_split(dataset_id_old, "-", simplify = TRUE)[, 2]) |>
  # Also save the date out of the old dataset ID
  mutate(date_old = str_split(dataset_id_old, "-", simplify = TRUE)[, 1]) |>
  mutate(date_old = paste(str_sub(date_old, 1, 4), str_sub(date_old, 5, 6), str_sub(date_old, 7, 8), sep = "-")) |>
  mutate(date_old = as.Date(date_old)) |>
  mutate(dataset_id_canon = ifelse(uses_old_id, dataset_id_old_trimmed, dataset_id)) |>
  mutate(dataset_date_canon = ifelse(uses_old_id, date_old, date) |> as.Date()) |>
  # Preserve baserow_dataset_id for linking tables
  rename(baserow_dataset_id = dataset_id) |>
  # Remove intermediate cols 
  select(-dataset_id_old, -dataset_id_old_trimmed, -uses_old_id, -date_old, -date) |>
  rename(dataset_id = dataset_id_canon,
         date = dataset_date_canon) |>
  mutate(dataset_id = str_pad(dataset_id, 6, pad = "0"))

# There may be baserow records that are identical except for the date or base station location
# (because a data curator duplicated the entry to accommodate the multiple dates). Assign the
# associated records a group ID that is unique for each group.
b2 = b |>
  group_by(contributor_dataset_name, submission_id, project_id, aircraft_model_id, sensor_id, flight_planner_id, flight_pattern, overlap_front_nominal,
           overlap_side_nominal, camera_pitch_nominal, terrain_follow, altitude_agl_nominal) |>
  mutate(group_id = cur_group_id(),
         n_in_group = n()) |>
  mutate(group_id = ifelse(n_in_group > 1, group_id, NA))

# If an image folder needs to be split to two associated folders (e.g. because two dates or
# aircrafts), then assign the associated records a group ID that is unique for each group.
# Ultimately, we need to create dataset IDs that are the same but have a sub-ID to differentiate. We
# will determine this by going through the image files in each folder and checking the date of each
# image.

# First goal is to create a data frame of: dataset_id, date, aircraft_id, [list col of all image
# filepaths]. We will do this by getting this info for each image file in every folder, then grouping.

dataset_folder = "20230527-0005/"


get_image_data = function(dataset_folder) {

  # Get the full path to the dataset folder
  base_folder = basename(dataset_folder)

  # Get list of image files
  image_filepaths = list.files(dataset_folder, full.names = TRUE, recursive = TRUE, pattern = "(.jpg$)|(.jpeg$)|(.JPG$)|(.JPEG$)")

  # Get exif data
  exif = read_exif(image_filepaths)

  # Compile relevant per-image info (including potential ways to distinguish two drones) into data
  # frame
  image_data_onefolder = data.frame(folder_in = base_folder,
                          image_path = image_filepaths,
                          date = exif$DateTimeOriginal,
                          model = exif$Model,
                          software = exif$Software,
                          serialnumber = exif$SerialNumber,
                          dewarpdata = exif$DewarpData)

  return(image_data_onefolder)

}


# All folders
folders = list.dirs(IMAGERY_INPUT_PATH, full.names = TRUE, recursive = FALSE)

plan = future::plan(multicore)
l = future_map(folders, get_image_data)





##### TODO: modify to process data frame of image data

focal_dataset = file.path(IMAGERY_INPUT_PATH, dataset_folder)
base_folder = basename(focal_dataset)

# Get list of image files
image_files = list.files(focal_dataset, full.names = TRUE, recursive = TRUE, pattern = "(.jpg$)|(.jpeg$)|(.JPG$)|(.JPEG$)")

# Get exif data
exif = read_exif(images)

# Compile relevant per-image info (including potential ways to distinguish two drones) into data
# frame
image_data_onefolder = data.frame(folder_in = base_folder,
                        image_path = image_files,
                        date = exif$DateTimeOriginal,
                        model = exif$Model,
                        software = exif$Software,
                        serialnumber = exif$SerialNumber,
                        dewarpdata = exif$DewarpData)
















#################################################

# Get the unique dates that have at least 25 images (assumed minimum for photogrammetry)
image_dates_unique = image_data |>
  count(date) |>
  filter(n >= 25) |>
  pull(date)

# Get the baserow record(s) for this dataset
baserow_foc = baserow |>
  filter(dataset_id == dataset_id)





# Get the dataset ID from the folder name
base_folder = basename(focal_dataset)
dataset_id = str_sub(base_folder, -4, -1)



date = str_split(datetime, " ", simplify = TRUE)[, 1] |>
         str_replace_all(fixed(":"), "-") |>
         as.Date()
         