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
if ("notes" %in% colnames(dataset_associations)) stop("You need to export the dataset associations table without comments because they screw up the cell delimitations for some reason.")

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

# There may be baserow records that are identical except for the date or base station location or
# aircraft (because a data curator duplicated the entry to accommodate the multiple dates). Assign
# the associated records a group ID that is unique for each group.
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
  separate_wider_delim(delim = "_and_", cols = "folder_in", names = c("folder_in", "folder_in_2", "folder_in_3"), too_few = "align_start") |>
  mutate(dataset_id_old_format = grepl("[0-9]{8}-[0-9]{4}", folder_in)) |>
  mutate(dataset_id = ifelse(dataset_id_old_format, str_sub(folder_in, -4, -1), folder_in) |>
           str_pad(6, "left", pad = "0")) |>  mutate(dataset_id_old_format = grepl("[0-9]{8}-[0-9]{4}", folder_in)) |>
  mutate(dataset_id_old_format_2 = grepl("[0-9]{8}-[0-9]{4}", folder_in_2)) |>
  mutate(dataset_id_2 = ifelse(dataset_id_old_format_2, str_sub(folder_in_2, -4, -1), folder_in_2) |>
           str_pad(6, "left", pad = "0")) |>
  mutate(dataset_id_old_format_3 = grepl("[0-9]{8}-[0-9]{4}", folder_in_3)) |>
  mutate(dataset_id_3 = ifelse(dataset_id_old_format_3, str_sub(folder_in_3, -4, -1), folder_in_3) |>
           str_pad(6, "left", pad = "0")) |>
  select(-dataset_id_old_format, -dataset_id_old_format_2, -dataset_id_old_format_3) |>
  # Images without a date are corrupted
  filter(!is.na(date))

datasets = image_data |>
  group_by(dataset_id, serialnumber, date) |>
  summarize(n_images = n()) |>
  # Datasets with < 30 images are likely not useful
  filter(n_images > 30) |>
  ungroup()

# When does the same dataset ID show up for more than one date or serial number?
combos_by_exif = datasets |>
  group_by(dataset_id) |>
  summarize(n_dates = n_distinct(date),
            n_serialnumbers = n_distinct(serialnumber)) |>
  filter(n_dates > 1 | n_serialnumbers > 1)

## If the same dataset ID contains multiple dates or drone serial numbers, deal with splitting it
datasets_not_separable = data.frame()
for (i in 1:nrow(combos_by_exif)) {

  combo_by_exif = combos_by_exif[i, ]
  dataset_id_foc = combo_by_exif$dataset_id
  exif_data = image_data |>
    filter(dataset_id == dataset_id_foc)

  exif_summ = exif_data |>
    group_by(dataset_id, dataset_id_2, dataset_id_3) |>
    summarize(n_images = n())

  ids_by_foldername = exif_summ |>
    select(dataset_id, dataset_id_2, dataset_id_3) |>
    unlist() |> na.omit() |> unique()

  exif_summ2 = exif_data |> # TODO: <- rename
    # The first of the folder name dataset IDs is the one that was assigned to the dataset_id column
    # for the image-level data, so filter on that
    filter(dataset_id == dataset_id_foc) |>
    group_by(date, serialnumber) |>
    summarize(n_images = n()) |>
    arrange(date, -n_images)

  # Get the baserow records for the dataset IDs making up the combo according to the image folder name
  baserow_records = b2 |>
    filter(dataset_id %in% ids_by_foldername)

  # According to baserow, what are the unique combinations of date, aircraft_model_id, base_lat,
  # flight_pattern, overlap_front_nominal, overlap_side_nominal, altitude_agl_nominal, and
  # terrain_follow? This will tell us if date alone can be used to split the image folders.
  baserow_summ = baserow_records |>
    ungroup() |>
    select(date, aircraft_model_id, base_lat, base_lon, base_alt, flight_pattern, overlap_front_nominal,
           overlap_side_nominal, altitude_agl_nominal, terrain_follow, contributor_dataset_name) |>
    distinct()

  # If multiple baserow records, see which fields differ between them, so that we can make sure all
  # differences are explained (separable) by date, and if now so we can record what else they
  # differed by
  cols_diff_idx = which(apply(baserow_summ, 2, function(a) length(unique(a)) > 1))
  cols_diff_names = names(baserow_summ)[cols_diff_idx]
  # Remove boilerplate columns
  cols_diff_names = setdiff(cols_diff_names, c("baserow_dataset_id", "Created on", "dataset_id"))

  baserow_unique_record_count = nrow(baserow_summ)
  baserow_unique_date_count = n_distinct(baserow_summ$date)

  if (baserow_unique_record_count > baserow_unique_date_count) {
    warning("The baserow records for the dataset IDs (from folder name) ", paste(ids_by_foldername, collapse = ", "), " differ by more than date; impossible to split the composite photo folder to each baserow record. Naming by the first dataset ID.")

    # TOOD: here, could check if a dataset is a composite of > 3 baserow records, can at least one
    # of them be cleanly split?

    dataset_not_separable = data.frame(dataset_id = ids_by_foldername[1],
                                        dataset_id_2 = ids_by_foldername[2],
                                        dataset_id_3 = ids_by_foldername[3],
                                        differ_by = paste(cols_diff_names, collapse = ", "),
                                        why_not_splittable = "EXIF date does not explain other EXIF variation")
    datasets_not_separable = bind_rows(datasets_not_separable, dataset_not_separable)
    # ^ This will be used to add a record to the baserow records that says that the image dataset
    # contains additional values for one or more columns but could not be separated automatically

    # Assign the first dataset ID and split by date or serialnumber if possible
    # TODO: turn this and the 3 similar blocks below into a function
    for (j in 1:nrow(exif_summ2)) {
      exif_summ_foc = exif_summ2[j, ]
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$serialnumber == exif_summ_foc$serialnumber,
                  "dataset_id_out"] = dataset_id_foc
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$serialnumber == exif_summ_foc$serialnumber,
                  "subdataset_out"] = j
    }

    next()
  }

  # Date is sufficient to identify the unique baserow records for this combo image folder. Now need
  # to make sure that image files themselves have the same number of unique combinations of date and
  # drone model so that they can be split correctly by date.

  exif_unique_record_count = nrow(exif_summ2)
  exif_unique_date_count = n_distinct(exif_summ2$date)

  # Make sure that everything else that varies is perfectly coinciding with date differences
  if (exif_unique_record_count > exif_unique_date_count) {

    # If there are multiple baserow records, record the fact that we can't distinguish which to
    # associate with which date / serialnumber
    if (baserow_unique_record_count > 1) {
      warning("The imagery exif records for the dataset IDs ", paste(ids_by_foldername, collapse = ", "), " differ by more than date; impossible to split the composite photo folder to each baserow record. Naming by the first dataset ID.")
      dataset_not_separable = data.frame(dataset_id = ids_by_foldername[1],
                                        dataset_id_2 = ids_by_foldername[2],
                                        dataset_id_3 = ids_by_foldername[3],
                                        differ_by = paste(cols_diff_names, collapse = ", "))

      datasets_not_separable = bind_rows(datasets_not_separable, dataset_not_separable)
    }

    # Assign the first ID and split into subdatasets by date and serialnumber
    for (j in 1:nrow(exif_summ2)) {
      exif_summ_foc = exif_summ2[j, ]
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$serialnumber == exif_summ_foc$serialnumber,
                  "dataset_id_out"] = dataset_id_foc
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$serialnumber == exif_summ_foc$serialnumber,
                  "subdataset_out"] = j
    }

    next()
  }

  # Make sure that exif info agrees with baserow on the number of dates
  if (exif_unique_record_count != baserow_unique_record_count) {
    warning("For combo dataset IDs ", paste(ids_by_foldername, collapse = ", "), ", there are ", exif_unique_record_count, " unique exif dates and ", baserow_unique_record_count, " unique baserow dates. Impossible to split the composite photo folder to each baserow record. Naming by the first dataset ID.")
    dataset_not_separable = data.frame(dataset_id = ids_by_foldername[1],
                                      dataset_id_2 = ids_by_foldername[2],
                                      dataset_id_3 = ids_by_foldername[3],
                                      differ_by = paste(cols_diff_names, collapse = ", "),
                                      why_not_splittable = "Uniqe EXIF does not match unique baserow")
    datasets_not_separable = bind_rows(datasets_not_separable, dataset_not_separable)

    # Assign the first ID and split into subdatasets by date and serialnumber
    for (j in 1:nrow(exif_summ2)) {
      exif_summ_foc = exif_summ2[j, ]
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$serialnumber == exif_summ_foc$serialnumber,
                  "dataset_id_out"] = dataset_id_foc
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$serialnumber == exif_summ_foc$serialnumber,
                  "subdataset_out"] = j
    }
    next()
  }

  # If we got here, we can match the images to the baserow records 1:1 by date, so we can split the
  # image foler into two folders with different dataset IDs according to the date. We prepare for
  # this by first adding a column to the exif table that is the image's destination dataset ID

  for (j in 1:nrow(exif_summ2)) {

    date_foc = exif_summ2$date[j]

    # Which baserow dataset ID does this date corespond to? Note, this does not refer to the
    # baserow column "baserow_dataset_id" (which is just an auto-number of rows by Postgres), but
    # to the "dataset_id" number according to the Baserow table, which is the OFO dataset ID.
    baserow_dataset_id = baserow_records |>
      filter(date == date_foc) |>
      pull(dataset_id)

    # In the full image exif dataframe, assign the baserow dataset ID to the image
    image_data[image_data$dataset_id == dataset_id_foc & image_data$date == date_foc, "dataset_id_out"] = baserow_dataset_id

  }
}


# Any remaining combo by folder name ("_and_") cannot be split out to unique baserow records, so assign them the first dataset ID and
# record a record that they contain additional values for one or more columns. May not need the
# "datasets_not_separable" dataframe created above if it can be created here.

composites_not_split = image_data |>
  filter(is.na(dataset_id_out) & (!is.na(dataset_id_2) | !is.na(dataset_id_3))) |>
  group_by(dataset_id, dataset_id_2, dataset_id_3, date, serialnumber) |>
  summarize(n_images = n())

# For each one, determine what's different between the baserow records and save that info
for (i in seq_len(nrow(composites_not_split))) {

  # TODO: This block is very similar to the one above, so turn it into a function
  composite_not_split = composites_not_split[i, ]

  dataset_id_foc = composite_not_split$dataset_id
  exif_data = image_data |>
    filter(dataset_id == dataset_id_foc)

  exif_summ = exif_data |>
    group_by(dataset_id, dataset_id_2, dataset_id_3) |>
    summarize(n_images = n())

  ids_by_foldername = exif_summ |>
    select(dataset_id, dataset_id_2, dataset_id_3) |>
    unlist() |> na.omit() |> unique()

  # Get the baserow records for the dataset IDs making up the combo according to the image folder name
  baserow_records = b2 |>
    filter(dataset_id %in% ids_by_foldername)

  # According to baserow, what are the unique combinations of date, aircraft_model_id, base_lat,
  # flight_pattern, overlap_front_nominal, overlap_side_nominal, altitude_agl_nominal, and
  # terrain_follow? This will tell us if date alone can be used to split the image folders.
  baserow_summ = baserow_records |>
    ungroup() |>
    select(date, aircraft_model_id, base_lat, base_lon, base_alt, flight_pattern, overlap_front_nominal,
           overlap_side_nominal, altitude_agl_nominal, terrain_follow, contributor_dataset_name) |>
    distinct()

  # If multiple baserow records, see which fields differ between them, so we can record what they
  # differed by
  cols_diff_idx = which(apply(baserow_summ, 2, function(a) length(unique(a)) > 1))
  cols_diff_names = names(baserow_summ)[cols_diff_idx]
  # Remove boilerplate columns
  cols_diff_names = setdiff(cols_diff_names, c("baserow_dataset_id", "Created on", "dataset_id"))

  warning("For combo dataset IDs ", paste(ids_by_foldername, collapse = ", "), ", there is only one date (and drone serialnumber), so it is impossible to split the composite photo folder to each baserow record. Naming by the first dataset ID.")
  dataset_not_separable = data.frame(dataset_id = ids_by_foldername[1],
                                     dataset_id_2 = ids_by_foldername[2],
                                     dataset_id_3 = ids_by_foldername[3],
                                     differ_by = paste(cols_diff_names, collapse = ", "),
                                     why_not_splittable = "Only one date")
  datasets_not_separable = bind_rows(datasets_not_separable, dataset_not_separable)

  # Save the main dataset ID as the dataset ID out
  image_data[image_data$dataset_id == dataset_id_foc, "dataset_id_out"] = dataset_id_foc

}


# Summarize image_data to inspect
inspect = image_data |>
  group_by(dataset_id, dataset_id_2, dataset_id_3, date, serialnumber, dataset_id_out, subdataset_out) |>
  summarize(n_images = n())
inspect
View(inspect)

datasets_not_separable


## Using baserow records, determine what folders of images should be associated (merged to one).
# They are grouped by association_id when they were associated in baserow as being contiguous or two
# orientations of a grid, so put these together under one dataset ID.
# They are grouped by group_id when they differ by date or base station location or aircraft (and
# have the same contributor dataset ID, altitude, and other important mission parameters), so also
# put these together under one dataset ID.
# We will need to create a new table that links the new dataset ID (with subdataset ID) to the old
# dataset ID.

# Create one overarching group_id_full that unifies all records that share a group_id OR
# association_id



# For each group_id_full in baserow, and every subdataset ID, assign the right dataset_id and a
# unique subdataset_id_full to the image_data table, probalby by merging in a subset of the baserow
# table b2 into the image_data table and calling a cur_group_id() on the merged table.
