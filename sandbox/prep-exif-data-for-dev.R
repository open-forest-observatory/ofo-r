# For 6 test image sets, read the EXIF data and save to file. This is to prepare EXIF data for
# development of functions to extract OFO metadata from EXIF.

# Intended to be run on an OFO Jetstream2 VM for access to the raw drone imagery.

library(exifr)
library(dplyr)

# Determine the directory to use for data (specified in a one-line text file in sandbox/data-dirs)
datadir = readLines(file.path("sandbox", "data-dirs", "derek-metadata-js.txt"))


dataset_ids = c("20230528-0008",
                "20230528-0009",
                "20220630-0041",
                "20220730-0079",
                "20230706-0152",
                "20230706-0153")

for (i in seq_along(dataset_ids)) {

  dataset_id = dataset_ids[i]

  image_paths = list.files(file.path("/ofo-share",
                                     "drone-imagery-all",
                                     "1_manually-cleaned",
                                     dataset_id),
                           recursive = TRUE,
                           pattern = ".(jpg|JPG|jpeg|JPEG)$",
                           full.names = TRUE)

  exif = exifr::read_exif(image_paths)

  # drop the ThumbnailImage and PreviewImage
  exif = exif |>
    select(-ThumbnailImage, -PreviewImage)

  out_path = file.path(datadir, "exif-examples")

  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

  write.csv(exif, file.path(out_path,
                            paste0("exif_", dataset_id, ".csv")))
}
