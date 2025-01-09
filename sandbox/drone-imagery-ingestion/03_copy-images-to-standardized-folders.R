# Purpose: Read the processed image-level EXIF data which contains the plan for sorting the images.
# Copy the images to the new folder structure based on the plan, including renaming the images to
# have consecutive numbering, and no more than 10,000 images per folder. Also save a crosswalk CSV
# listing the original image path and the new image path.

library(tidyverse)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)

PROCESSED_EXIF_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting"
SORTED_IMAGERY_OUT_FOLDER = "/ofo-share/drone-imagery-organization/2_sorted-notcleaned"
SORTED_IMAGERY_CROSSWALK_FOLDER = "/ofo-share/drone-imagery-organization/2b_filepath-crosswalk"

exif_path = file.path(PROCESSED_EXIF_PATH, paste0(IMAGERY_PROJECT_NAME, "_exif.csv"))
sorted_imagery_out_folder = file.path(SORTED_IMAGERY_OUT_FOLDER, IMAGERY_PROJECT_NAME)
sorted_imagery_crosswalk_folder = file.path(SORTED_IMAGERY_CROSSWALK_FOLDER, IMAGERY_PROJECT_NAME)

# Processing

exif = read_csv(exif_path)

# Compute a filename for each image from: folder_out (the dataset ID), and an incrementing number
# padded to 6 digits

exif = exif |>
  mutate(extension = tools::file_ext(image_path)) |>
  mutate(image_path_rel = str_split(image_path, IMAGERY_PROJECT_NAME) |> map(2)) |>
  # drop the leading slash
  mutate(image_path_rel = str_sub(image_path_rel, 2)) |>
  group_by(folder_out_final) |>
  mutate(image_number = row_number()) |>
  ungroup() |>
  mutate(image_number_str = str_pad(image_number, 6, pad = "0")) |>
  mutate(image_filename_out = paste0(folder_out_final, "_", image_number_str, ".", extension)) |>
  # Separate subfolders for each 10,000 images
  mutate(subfolder = floor((image_number) / 10000)) |>
  mutate(subfolder_str = str_pad(subfolder, 2, pad = "0")) |>
  mutate(image_path_out_rel = file.path(folder_out_final, subfolder_str, image_filename_out)) |>
  mutate(image_path_out = file.path(sorted_imagery_out_folder, image_path_out_rel))

# Save the crosswalk CSV, one file for each dataset (output folder)
crosswalk = exif |>
  select(dataset_id = folder_out_final,
         original_image_path = image_path_rel,
         sorted_image_path = image_path_out_rel)

dir.create(sorted_imagery_crosswalk_folder, recursive = TRUE)

for (folder_out_foc in unique(exif$folder_out_final)) {
  crosswalk_foc = crosswalk |>
    filter(dataset_id == folder_out_foc) |>
    select(original_image_path, sorted_image_path)
  write_csv(crosswalk_foc, file.path(sorted_imagery_crosswalk_folder, paste0(folder_out_foc, ".csv")))
}

# also save an overall crosswalk for this project
write_csv(crosswalk, file.path(sorted_imagery_crosswalk_folder, "ALL.csv"))


# Perform the file copy, specifically as hardlinks

# Create the output folder(s)
folders_out_rel = unique(dirname(exif$image_path_out_rel))
folders_out_abs = file.path(sorted_imagery_out_folder, folders_out_rel)
sapply(folders_out_abs, dir.create, recursive = TRUE)

# Copy files as hardlinks
file.link(exif$image_path, exif$image_path_out)
