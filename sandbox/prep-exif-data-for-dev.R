# For 6 test image sets, read the EXIF data and save to file. This is to prepare EXIF data for
# development of functions to extract OFO metadata from EXIF.

# Intended to be run on an OFO Jetstream2 VM for access to the raw drone imagery.

library(exifr)
library(dplyr)

# Determine the directory to use for data (specified in a one-line text file in sandbox/data-dirs)
datadir = readLines(file.path("sandbox", "data-dirs", "derek-metadata-js.txt"))
datadir = "/ofo-share/str-disp_drone-data-partial/str-disp_drone-data_imagery-missions/Lassic/Lassic_120m"


# dataset_ids = c("20230528-0008",
#                 "20230528-0009",
#                 "20220630-0041",
#                 "20220730-0079",
#                 "20230706-0152",
#                 "20230706-0153")

# for (i in seq_along(dataset_ids)) {

  # dataset_id = dataset_ids[i]
  dataset_id = "lassic120m-cleaned"

  image_paths = list.files(datadir,
                           recursive = TRUE,
                           pattern = ".(jpg|JPG|jpeg|JPEG)$",
                           full.names = TRUE)

  exif = exifr::read_exif(image_paths)

  # drop the ThumbnailImage and PreviewImage
  exif = exif |>
    select(-ThumbnailImage, -PreviewImage)

  out_path = file.path(datadir, "exif")

  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

  write.csv(exif, file.path(out_path,
                            paste0("exif_", dataset_id, ".csv")))
# }


# See which images have oblique angles

datadir = "/ofo-share/str-disp_drone-data-partial/str-disp_drone-data_imagery-missions/Lassic/Lassic_120m"
exif = readr::read_csv(file.path(datadir, "exif", "exif_lassic80m.csv"))


# non_mapping = exif |>
#   filter(CameraPitch > -89.8 | CameraPitch < -90.1) |>
#   pull(SourceFile)
# non_mapping

non_mapping = exif |>
  filter(CameraPitch > -64.8 | CameraPitch < -70.1) |>
  pull(SourceFile)
non_mapping

filename = basename(non_mapping)
nfiles = length(filename)
filename1 = tools::file_path_sans_ext(filename)
filename2 = paste0(filename1, "_", 1:nfiles, ".jpg")

file.rename(non_mapping, file.path("/ofo-share/str-disp_drone-data-partial/str-disp_drone-data_imagery-missions/Lassic/Lassic_landscape",
                                   filename2))

newexif = exif[!(exif$SourceFile %in% non_mapping), ]

write.csv(newexif, file.path(out_path,
                            paste0("exif_", dataset_id, "_cleaned.csv")))
