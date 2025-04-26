# Purpose: Read the EXIF data previously extracted from drone images, combined with baserow
# metadata, and determine how to split the images into a standrdized folder structure (e.g., no more
# than one date per folder, but with multiple dates associated via sub-IDs). Save the data needed to
# execute the sorting to a CSV.

library(tidyverse)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
EXIF_INPUT_PATH = "/ofo-share/drone-imagery-organization/metadata/1_reconciling-contributions/1_raw-exif/"

# Out
CROSSWALK_OUTPUT_PATH = "/ofo-share/drone-imagery-organization/metadata/1_reconciling-contributions/3_contributed-to-sorted-id-crosswalk/"
SORTED_IMAGERY_OUT_FOLDER = "/ofo-share/drone-imagery-organization/2_sorted"
IMAGE_EXIF_W_SORTING_PLAN_FOLDER = "/ofo-share/drone-imagery-organization/metadata/1_reconciling-contributions/2_exif-w-sorting-plan/"

# What is the padding width for the dataset ID in the folder names of the imagery folders to be ingested? (New format) This is used to
# force the Baserow dataset ID column to conform to the image folder names, so this should reflect
# the padding used when naming the image folders in the 1_manually-cleaned folder.
FOLDER_DATASET_ID_PADDING = 6

## END CONSTANTS

## Set up derived constants
exif_input_path = file.path(EXIF_INPUT_PATH, paste0(IMAGERY_PROJECT_NAME, ".csv"))
crosswalk_output_path = file.path(CROSSWALK_OUTPUT_PATH, paste0(IMAGERY_PROJECT_NAME, ".csv"))

if (!dir.exists(CROSSWALK_OUTPUT_PATH)) {
  dir.create(CROSSWALK_OUTPUT_PATH, recursive = TRUE)
}

## Prep baserow metadata

# Load baserow records
baserow = read_csv(file.path(BASEROW_DATA_PATH, "export - datasets-imagery.csv"))
dataset_associations = read.csv(file.path(BASEROW_DATA_PATH, "export - dataset-associations - Grid.csv"))
# ^ Needed to use the base R read.csv because read_csv was removing the comma from column values
# when the values were numeric.
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

# Drop associations of rgb to multispectral, since we do not want to combine these now (though the
# user still could)
assoc = assoc |>
  filter(assoc_type != "ms-and-rgb")


# TODO: Make this more efficient. Though it is not that slow.
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


# Create a column for the canonical dataset ID (and get the date out of the "old ID" that was used
# for some datasets initially, since at that time were were not recording the date as its own field,
# it was just embedded in the dataset ID)
b = baserow |>
  # Remove a database row ID (not an actual dataset ID)
  select(-id) |>
  mutate(across(contains("dataset_id"), as.character)) |>
  mutate(folder_uses_old_id = !is.na(dataset_id_old)) |>
  # Remove the first part of the ID (the date, separated from the actual ID by a "-")
  mutate(folder_name = ifelse(folder_uses_old_id,
                                    dataset_id_old,
                                    dataset_id |> str_pad(FOLDER_DATASET_ID_PADDING, "left", "0"))) |>
  mutate(dataset_id_old_trimmed = str_split(dataset_id_old, "-", simplify = TRUE)[, 2]) |>
  # Also save the date out of the old dataset ID
  mutate(date_old = str_split(dataset_id_old, "-", simplify = TRUE)[, 1]) |>
  mutate(date_old = paste(str_sub(date_old, 1, 4), str_sub(date_old, 5, 6), str_sub(date_old, 7, 8), sep = "-")) |>
  mutate(date_old = as.Date(date_old)) |>
  mutate(dataset_date_canon = ifelse(folder_uses_old_id, date_old, date) |> as.Date()) |>
  # Preserve baserow_dataset_id for linking tables
  rename(baserow_dataset_id = dataset_id) |>
  # Remove intermediate cols 
  select(-dataset_id_old, -dataset_id_old_trimmed, -folder_uses_old_id, -date_old, -date) |>
  rename(dataset_id = baserow_dataset_id,
         date = dataset_date_canon) |>
  mutate(dataset_id = str_pad(dataset_id, 6, pad = "0"))

# There may be baserow records that are identical except for the date or base station location or
# aircraft (because a data curator duplicated the entry to accommodate the multiple dates, base
# locations, or aircrafts). Assign the associated records a group ID that is unique for each group.
b2 = b |>
  group_by(contributor_dataset_name, submission_id, project_id, aircraft_model_id, sensor_id, flight_planner_id, flight_pattern, overlap_front_nominal,
           overlap_side_nominal, camera_pitch_nominal, terrain_follow, altitude_agl_nominal) |>
  mutate(group_id = cur_group_id(),
         n_in_group = n()) |>
  mutate(group_id = ifelse(n_in_group > 1, group_id, NA))


## Process the image-level data into dataset-level data to determine how to split and group the image files in their ultimate standardized folders

# We need read.csv instead of read_csv because the latter was removing the decimal portion of the
# GPSTimeStamp tag, which is needed to determine whether we need to apply an EXIF fix to timestamps
# formatted that way (because it will cause an error in Metashape)
image_data = read.csv(file.path(exif_input_path))

# Remove images without a date (likely corrupted)
image_data = image_data |>
  filter(!is.na(DateTimeOriginal))

# Standardize date and separate the folder name into its components (if it is a composite dataset
# with names separated by "_and_" because the imagery in the folder corresponds to multiple baserow
# records and the imagery folder could not be split into multiple folders with unique dataset IDs
# based on the information available to the curator)
image_data = image_data |>
  mutate(date = str_split(DateTimeOriginal, " ", simplify = TRUE)[, 1] |>
           str_replace_all(fixed(":"), "-") |>
           as.Date()) |>
  mutate(folder_in = str_replace_all(folder_in, fixed("/"), "")) |>
  separate_wider_delim(delim = "_and_", cols = "folder_in", names = c("folder_in", "folder_in_2", "folder_in_3"), too_few = "align_start") |>
  filter(!is.na(date)) |>
  mutate(across(c(folder_in, folder_in_2, folder_in_3), ~ str_pad(.x, width = FOLDER_DATASET_ID_PADDING, side = "left", pad = "0" )))


# Bring in the dataset ID (as listed in Baserow) corresponding to each image folder name (this is
# needed to accommodate folder names with the older format ({date}-{id}), to accommodate those names
# created before we were using the canonical baserow ID to name the folders). If there were multiple
# dataset IDs, then bring in all of them (up to 3)

# First remove multispectral folders from Baserow and the exif
multispec_folder_names = b |>
  filter(modality != "rgb") |>
  pull(folder_name)
# We are assuming that the contributor would not have combined multispectral and RGB images in the
# same folder, so we would never have a folder named something like "1011 and 1012" that would
# contain both RGB and multispectral images, corresponding to two separate baserow records. Since we
# don't need to deal with this case, we just need to check whether the first folder name is RGB or
# multispec.
image_data = image_data |>
  filter(!(folder_in %in% multispec_folder_names))
b = b |>
  filter(modality == "rgb")

crosswalk = b |>
  select(dataset_id_baserow = dataset_id, folder_name)

image_data = image_data |>
  left_join(crosswalk, by = c("folder_in" = "folder_name")) |>
  rename(dataset_id = dataset_id_baserow)
image_data = image_data |>
  left_join(crosswalk, by = c("folder_in_2" = "folder_name")) |>
  rename(dataset_id_2 = dataset_id_baserow)
image_data = image_data |>
  left_join(crosswalk, by = c("folder_in_3" = "folder_name")) |>
  rename(dataset_id_3 = dataset_id_baserow)

datasets = image_data |>
  group_by(dataset_id, dataset_id_2, dataset_id_3, folder_in, SerialNumber, date) |>
  summarize(n_images = n()) |>
  # Datasets with < 30 images are likely not useful
  filter(n_images > 30) |>
  ungroup()

# Check for any folders unmatched to dataset IDs (in either the primary dataset ID, or the secondary
# or tertiary, if the dataset was composed of multiple IDs (i.e. multiple baserow records, i.e. the
# folder name had "_and_" in it to designate that the images corresponded to multiple Baserow
# records and could not be split out based on the information available to the curator))

if (any(is.na(datasets$dataset_id))) {
  folders_no_id = datasets |>
    filter(is.na(dataset_id)) |>
    select(folder_in) |>
    pull()
  warning("Image folders ", paste(folders_no_id, collapse = ", ") , " could not be matched to a dataset ID. Make sure these is a corresponding record in Baserow. Ignoring them.")
  datasets = datasets |>
    filter(!is.na(dataset_id))
  image_data = image_data |>
    filter(!is.na(dataset_id))
}

a = image_data |>
  group_by(dataset_id_2, folder_in_2) |>
  summarize(n_images = n()) |>
  filter(!is.na(folder_in_2))

if (any(is.na(a$dataset_id_2))) {
  folders_no_id = a |>
    filter(is.na(dataset_id_2)) |>
    select(folder_in_2) |>
    pull()
  warning("Secondary (of composite) image folder names ", paste(folders_no_id, collapse = ", ") , " could not be matched to a dataset ID. Make sure these is a corresponding record in Baserow. Pretending the main folder is not part of a composite.")
}

a = image_data |>
  group_by(dataset_id_3, folder_in_3) |>
  summarize(n_images = n()) |>
  filter(!is.na(folder_in_3))

if (any(is.na(a$dataset_id_3))) {
  folders_no_id = a |>
    filter(is.na(dataset_id_3)) |>
    select(folder_in_3) |>
    pull()
  warning("Tertiary (of composite) image folder names ", paste(folders_no_id, collapse = ", ") , " could not be matched to a dataset ID. Make sure these is a corresponding record in Baserow. Pretending the main folder is not part of a composite.")
}

# Split out datasets that have more than one date or drone serial number.
# When does the same dataset ID (image folder) contain more than one date or serial number?
combos_by_exif = datasets |>
  group_by(dataset_id) |>
  summarize(n_dates = n_distinct(date),
            n_serialnumbers = n_distinct(SerialNumber)) |>
  filter(n_dates > 1 | n_serialnumbers > 1)

## If the same dataset ID contains multiple dates or drone serial numbers, deal with splitting it
datasets_not_separable = data.frame()
image_data$dataset_id_out = NA
image_data$subdataset_out = NA
for (i in seq_len(nrow(combos_by_exif))) {

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
    group_by(date, SerialNumber) |>
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
  # differences are explained (separable) by date, and if so we can record what else they differed
  # by
  cols_diff_idx = which(apply(baserow_summ, 2, function(a) length(unique(a)) > 1))
  cols_diff_names = names(baserow_summ)[cols_diff_idx]
  # Remove boilerplate columns
  cols_diff_names = setdiff(cols_diff_names, c("baserow_dataset_id", "Created on", "dataset_id"))

  baserow_unique_record_count = nrow(baserow_summ)
  baserow_unique_date_count = n_distinct(baserow_summ$date)

  if (baserow_unique_record_count > baserow_unique_date_count) {
    warning("The baserow records for the dataset IDs ", paste(ids_by_foldername, collapse = ", "), " differ by more than date; impossible to split the composite photo folder to each baserow record. Naming by the first dataset ID.")

    # TOOD: here, could check if a dataset is a composite of > 3 baserow records, can at least one
    # of them be cleanly split?

    dataset_not_separable = data.frame(dataset_id = ids_by_foldername[1],
                                        dataset_id_2 = ids_by_foldername[2],
                                        dataset_id_3 = ids_by_foldername[3],
                                        differ_by = paste(cols_diff_names, collapse = ", "),
                                        why_not_separable = "EXIF date does not explain other EXIF variation")
    datasets_not_separable = bind_rows(datasets_not_separable, dataset_not_separable)
    # ^ This will be used to add a record to the baserow records that says that the image dataset
    # contains additional values for one or more columns but could not be separated automatically

    # Assign the first dataset ID and split by date or serialnumber if possible
    # TODO: turn this and the 3 similar blocks below into a function
    for (j in 1:nrow(exif_summ2)) {
      exif_summ_foc = exif_summ2[j, ]
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$SerialNumber == exif_summ_foc$SerialNumber,
                  "dataset_id_out"] = dataset_id_foc
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$SerialNumber == exif_summ_foc$SerialNumber,
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
                    image_data$SerialNumber == exif_summ_foc$SerialNumber,
                  "dataset_id_out"] = dataset_id_foc
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$SerialNumber == exif_summ_foc$SerialNumber,
                  "subdataset_out"] = j
    }

    next()
  }

  # Make sure that exif info agrees with baserow on the number of dates
  if (exif_unique_record_count != baserow_unique_record_count) {
    warning("For combo dataset IDs ", paste(ids_by_foldername, collapse = ", "), ", there are ", exif_unique_record_count, " unique exif dates/serialnumbers and ", baserow_unique_record_count, " unique baserow dates/drone models. Impossible to split the composite photo folder to each baserow record. Naming by the first dataset ID and splitting into subdatasets by date/serialnumber.")
    dataset_not_separable = data.frame(dataset_id = ids_by_foldername[1],
                                      dataset_id_2 = ids_by_foldername[2],
                                      dataset_id_3 = ids_by_foldername[3],
                                      differ_by = paste(cols_diff_names, collapse = ", "),
                                      why_not_separable = "Uniqe EXIF does not match unique baserow")
    datasets_not_separable = bind_rows(datasets_not_separable, dataset_not_separable)

    # Assign the first ID and split into subdatasets by date and serialnumber
    for (j in 1:nrow(exif_summ2)) {
      exif_summ_foc = exif_summ2[j, ]
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$SerialNumber == exif_summ_foc$SerialNumber,
                  "dataset_id_out"] = dataset_id_foc
      image_data[image_data$dataset_id == dataset_id_foc &
                    image_data$date == exif_summ_foc$date &
                    image_data$SerialNumber == exif_summ_foc$SerialNumber,
                  "subdataset_out"] = j
    }
    next()
  }

  # If we got here, we can match the images to the baserow records 1:1 by date, so we can split the
  # image foler into two folders with different dataset IDs according to the date. We prepare for
  # this by first adding a column to the exif table that is the image's destination dataset ID

  for (j in 1:nrow(exif_summ2)) {

    date_foc = exif_summ2$date[j]

    # Which baserow dataset ID does this date corespond to?
    baserow_dataset_id = baserow_records |>
      filter(date == date_foc) |>
      pull(dataset_id)

    if (length(baserow_dataset_id) == 0) stop("Mismatch in dates between Baserow metadata and image EXIF data for dataset ID ", dataset_id_foc, ", which is from image folder.")

    # In the full image exif dataframe, assign the baserow dataset ID to the image
    image_data[image_data$dataset_id == dataset_id_foc & image_data$date == date_foc, "dataset_id_out"] = baserow_dataset_id

  }
}


# Any remaining combo by folder name ("_and_") cannot be split out to unique baserow records, so
# assign them the first dataset ID and record a record that they contain additional values for one
# or more columns that cannot be looked up from Baserow. May not need the "datasets_not_separable"
# dataframe created above if it can be created here.

composites_not_split = image_data |>
  filter(is.na(dataset_id_out) & (!is.na(dataset_id_2) | !is.na(dataset_id_3))) |>
  group_by(dataset_id, dataset_id_2, dataset_id_3, date, SerialNumber) |>
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
                                     why_not_separable = "Only one date")
  datasets_not_separable = bind_rows(datasets_not_separable, dataset_not_separable)

  # Save the main dataset ID as the dataset ID out
  image_data[image_data$dataset_id == dataset_id_foc, "dataset_id_out"] = dataset_id_foc

}

# If dataset_id_out was not assigned (because it was not a composite dataset), then assign the,
# original dataset_id
image_data = image_data |>
  mutate(dataset_id_out = ifelse(is.na(dataset_id_out), dataset_id, dataset_id_out),
         subdataset_out = ifelse(is.na(subdataset_out), 1, subdataset_out))


# Summarize image_data to inspect
inspect = image_data |>
  group_by(dataset_id, dataset_id_2, dataset_id_3, date, SerialNumber, dataset_id_out, subdataset_out) |>
  summarize(n_images = n())
inspect

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
# association_id. A given observation may have both, linking it to some records via group_id and
# others via association_id. This whole cascade of linked records should be assigned the same
# group_id_full. Approach: create an incrementing group_id_full. Loop through each baserow record.
# When you reach one that has not been assigned a group_id_full, first check if any other records of
# the same association_id or group_id have been assigned a group_id_full. If so, assign the same
# group_id full. If not, assign a new group_id_full and assign it to all records with the same
# group_id or association_id.

b3 = b2

# Prep: where group ID is missing, assign an incrementing group ID that's not used
max_group_id = max(b3$group_id, na.rm = TRUE)
max_association_id = max(b3$association_id, na.rm = TRUE)
b3 = b3 |>
  ungroup() |>
  mutate(group_id = ifelse(is.na(group_id), max_group_id + row_number(), group_id),
         association_id = ifelse(is.na(association_id), max_association_id + row_number(), association_id))


current_group_id_full = 1
b3$group_id_full = NA
for (i in seq_len(nrow(b3))) {

  b3_row = b3[i, ]

  if (!is.na(b3_row$group_id_full)) next()

  group_id_foc = b3_row$group_id
  association_id_foc = b3_row$association_id

  # Find any records with the same group_id or association_id that have already been assigned a
  # group_id_full and assign the same group_id_full to this record
  existing_paired_records = b3 |>
    filter(group_id == group_id_foc | association_id == association_id_foc) |>
    filter(!is.na(group_id_full))

  if (nrow(existing_paired_records) > 0) {

    existing_group_id_full = existing_paired_records$group_id_full |> unique()
    if (length(existing_group_id_full) > 1) stop("Multiple group_id_fulls found for the same group_id or association_id.")

    b3[i, "group_id_full"] = existing_group_id_full
    group_id_to_assign = existing_group_id_full
  } else {
    group_id_to_assign = current_group_id_full
    current_group_id_full = current_group_id_full + 1
  }

  b3[which(b3$group_id == group_id_foc | b3$association_id == association_id_foc), "group_id_full"] = group_id_to_assign
}

inspect = b3 |>
  ungroup() |>
  select(contributor_dataset_name, association_id, group_id, group_id_full)

# For each group_id_full in baserow, and every subdataset ID in image_data, assign the right
# dataset_id (the first one that occurs for the group) and a unique subdataset_id_full to the
# image_data table, probably by merging in a subset of the baserow table b3 into the image_data
# table and calling a cur_group_id() on the merged table.

b3_simp = b3 |>
  select(contributor_dataset_name, dataset_id, association_id, group_id, group_id_full)

final_ids_for_images = image_data |>
  left_join(b3_simp, by = "dataset_id") |>
  # Outputs should be uniquely defined by dataset_id_out, subdataset_out, and group_id_full
  arrange(dataset_id_out, subdataset_out, group_id_full) |>
  group_by(dataset_id_out, subdataset_out, group_id_full) |>
  summarize(n_images = n()) |>
  group_by(group_id_full) |>
  mutate(dataset_id_out_final = first(dataset_id_out),
         subdataset_out_final = row_number()) |>
  # We can drop group_id_full because it only serves to indicate two unique dataset IDs are related.
  # It is not necessary as a unique identifier.
  select(dataset_id_out, subdataset_out, dataset_id_out_final, subdataset_out_final)

# Pull it in to image data
image_data = image_data |>
  left_join(final_ids_for_images, by = join_by("dataset_id_out" == "dataset_id_out",
                                               "subdataset_out" == "subdataset_out")) |>
  # Format it for writing a folder name
  mutate(subdataset_out_final = str_pad(subdataset_out_final, 2, side = "left", pad = "0"),
         folder_out_final = paste(dataset_id_out_final, subdataset_out_final, sep = "-"))

inspect = image_data |>
  group_by(dataset_id, dataset_id_2, dataset_id_3, folder_in, date, SerialNumber, folder_out_final) |>
  summarize(n_images = n())
inspect

# Save a record of, for each final subdataset, what the
# original dataset ID was. This should include the what differed between them, including the
# previously computed data frame for this (datasets_not_separable).
folderid_baserow_crosswalk = image_data |>
  select(dataset_id_baserow = dataset_id, sub_mission_id = folder_out_final) |>
  mutate(mission_id = str_sub(sub_mission_id, 1, 6)) |>
  group_by(dataset_id_baserow, sub_mission_id, mission_id) |>
  summarize(n_images = n()) |>
  ungroup() |>
  mutate(project_name = IMAGERY_PROJECT_NAME)

if (nrow(datasets_not_separable) > 0) {

  datasets_not_separable2 = datasets_not_separable |>
    unite(addl_dataset_ids_baserow, dataset_id_2, dataset_id_3, sep = ",", na.rm = TRUE) |>
    select(dataset_id_baserow = dataset_id, addl_dataset_ids_baserow, addl_baserow_differ_by = differ_by, why_not_separable)

  folderid_baserow_crosswalk = folderid_baserow_crosswalk |>
    left_join(datasets_not_separable2, by = "dataset_id_baserow")

}

write_csv(folderid_baserow_crosswalk, crosswalk_output_path)

## NEW, formerly from script 04_copy-images-to-sorted-folders.R
# Determine the output folder and filename for each image

# Compute a filename for each image from: folder_out (the dataset ID), and an incrementing number
# padded to 6 digits

image_data = image_data |>
  mutate(extension = tools::file_ext(image_path_in)) |>
  rename(mission_id = dataset_id_out_final,
         sub_mission_id = folder_out_final) |>
  mutate(image_path_in_rel = str_replace(image_path_in, paste0(".*", IMAGERY_PROJECT_NAME, "\\/"), "")) |>
  mutate(image_path_in_rel = paste0(IMAGERY_PROJECT_NAME, "/", image_path_in_rel)) |>
  group_by(sub_mission_id) |>
  mutate(image_number = row_number()) |>
  ungroup() |>
  mutate(image_number_str = str_pad(image_number, 6, pad = "0")) |>
  mutate(image_filename_out = paste0(sub_mission_id, "_", image_number_str, ".", extension)) |>
  # Separate subfolders for each 10,000 images
  mutate(subfolder = floor((image_number) / 10000)) |>
  mutate(subfolder_str = str_pad(subfolder, 2, pad = "0")) |>
  mutate(image_path_ofo = file.path(mission_id, sub_mission_id, subfolder_str, image_filename_out))


# Remove intermediate columns not needed downstream

cols_remove = c("folder_in", "folder_in_2", "folder_in_3", "date", "dataset_id", "dataset_id_2", "dataset_id_3",
                "dataset_id_out", "subdataset_out", "group_id_full",
                "subdataset_out_final", "image_number", "image_path_in",
                "image_number_str", "subfolder", "subfolder_str")
image_data = image_data |>
  select(-any_of(cols_remove))

# For each sub-mission, save the exif data (which now also contains the image sorting plan) to a CSV

if(!dir.exists(IMAGE_EXIF_W_SORTING_PLAN_FOLDER)) {
  dir.create(IMAGE_EXIF_W_SORTING_PLAN_FOLDER, recursive = TRUE)
}

for (mission_id_foc in unique(image_data$mission_id)) {
  image_data_foc = image_data |>
    filter(mission_id == mission_id_foc)
  write_csv(image_data_foc, file.path(IMAGE_EXIF_W_SORTING_PLAN_FOLDER, paste0(mission_id_foc, ".csv")))
}
