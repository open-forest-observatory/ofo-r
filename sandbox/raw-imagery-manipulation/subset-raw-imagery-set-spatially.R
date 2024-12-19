library(sf)
library(exifr)

FULL_IMAGERY_FOLDER = "/ofo-share/drone-imagery-organization/6_combined-across-projects/000439"
SUBSET_IMAGERY_FOLDER = "/ofo-share/benchmark-datasets/raw-images/000439-subset"

image_files = list.files(INPUT_IMAGERY_FOLDER, full.names = TRUE, recursive = TRUE, pattern = ".JPG$")

# Get the column names so we know what to use for coords
exif = read_exif(image_files[1])
names(exif)

exif = read_exif(image_files, tags = c("GPSLatitude", "GPSLongitude"))

# Make spatial
pts = st_as_sf(exif, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)

# Get bounds
bounds = pts |>
  st_transform(3310) |>
  st_buffer(50) |>
  st_union() |>
  st_buffer(-50)

bounds_smaller = bounds |>
  st_buffer(-50)

pts_subset = pts |>
  st_transform(3310) |>
  st_intersection(bounds_smaller)

subset_image_files = pts_subset$SourceFile

# Create the ouptut folder
dir.create(SUBSET_IMAGERY_FOLDER, recursive = TRUE)

# Hardlink the files
for (i in 1:nrow(pts_subset)) {
  file = subset_image_files[i]
  file_out = file.path(SUBSET_IMAGERY_FOLDER, basename(file))
  file.link(file, file_out)
}

