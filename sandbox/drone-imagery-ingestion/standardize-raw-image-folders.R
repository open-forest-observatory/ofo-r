# Purpose: Read in the manually cleaned drone images in mission folders and write them back out in a
# standardized folder structure with standaridzed file names

library(tidyverse)
library(exifr)
library(furrr)

IMAGERY_INPUT_PATH = "/ofo-share/drone-imagery-organization/1_manually-cleaned/2022-early-regen/"
IMAGERY_OUTPUT_PATH = "/ofo-share/drone-imagery-organization/2_standardized-preclean/2022-early-regen/"
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

get_image_data = function(dataset_folder) {

  # Get the full path to the dataset folder
  base_folder = basename(dataset_folder)

  # Get list of image files
  image_filepaths = list.files(dataset_folder, full.names = TRUE, recursive = TRUE, pattern = "(.jpg$)|(.jpeg$)|(.JPG$)|(.JPEG$)")

  # Get exif data
  exif = read_exif(image_filepaths)

  # Compile relevant per-image info (including potential ways to distinguish two drones) into data
  # frame

  # Check if nay of the relevant columns are null
  date_null = is.null(exif$DateTimeOriginal)
  model_null = is.null(exif$Model)
  serialnumber_null = is.null(exif$SerialNumber)
  if(date_null || model_null || serialnumber_null) {
    warning("Null values (likely complete image corruption) found in ", base_folder, ". Skipping.")
    return(NULL)
  }

  image_data_onefolder = data.frame(folder_in = base_folder,
                           image_path = image_filepaths,
                           date = exif$DateTimeOriginal,
                           model = exif$Model,
                           serialnumber = exif$SerialNumber)

  return(image_data_onefolder)

}


# All folders
folders = list.dirs(IMAGERY_INPUT_PATH, full.names = TRUE, recursive = FALSE)

plan = future::plan(multicore)
l = future_map(folders, get_image_data)

image_data = bind_rows(l)
write_csv(image_data, file.path(BASEROW_DATA_PATH, "image_data_2022-early-regen.csv"))


## Process the image-level data into dataset-level data to determine how to split and group the image files in their ultimate standardized folders

image_data = read_csv(file.path(BASEROW_DATA_PATH, "image_data_2022-early-regen.csv"))

# Remove images without a date (likely corrupted)
image_data = image_data |>
  filter(!is.na(date))

# Standardize date and get dataset ID from folder name, accommodating that it may be in the format
# {date}-{4-digit ID} or simply {4- to 6-digit ID}
image_data = image_data |>
  mutate(date = str_split(date, " ", simplify = TRUE)[, 1] |>
           str_replace_all(fixed(":"), "-") |>
           as.Date()) |>
  mutate(folder_in = str_replace_all(folder_in, fixed("/"), "")) |>
  separate_wider_delim(delim = "_and_", names = c("folder_in", "folder_in_2", "folder_in_3")) |>
  mutate(dataset_id_old_format = grepl("[0-9]{8}-[0-9]{4}", folder_in)) |>
  mutate(dataset_id = ifelse(dataset_id_old_format, str_sub(folder_in, -4, -1), folder_in) |>
           str_pad(6, "left", pad = "0")) |>
  # Images without a date are corrupted
  filter(!is.na(date))

datasets = image_data |>
  group_by(dataset_id, serialnumber, date) |>
  summarize(n_images = n()) |>
  # Datasets with < 30 images are likely not useful
  filter(n_images > 30)

# When does the same dataset ID show up for more than one date or serial number?
multiples = datasets |>
  group_by(dataset_id) |>
  summarize(n_dates = n_distinct(date),
            n_serialnumbers = n_distinct(serialnumber)) |>
  filter(n_dates > 1 | n_serialnumbers > 1)

## TODO: If the same dataset shows up in multiple dates or drone serial numbers, deal with that situation


# TODO: If image folder separated by _and_, check what baserow columns are different between the
# two. If it's just date, split on date into those two columns. If it's something else like base
# loc, then don't split, assign to the first ID, and add a record saying that the image dataset
# includes ultiple values of {all columns that have multiple values}