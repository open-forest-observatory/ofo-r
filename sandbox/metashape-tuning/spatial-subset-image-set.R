## Take a gpkg of waypoints of photo locations (produced by QGIS "Import geotagged photos")
## and copy the photos to a specified folder. The idea is that the gpkg of waypoints was
## produced in QGIS by manually selecting a subset of the photo points.

library(sf)
library(tidyverse)
library(exifr)

datadir = readLines("sandbox/data-dirs/js2-metashape-tuning.txt")

focal_polygons = st_read(file.path(datadir, "site-subsets.gpkg"))

focal_polygon = focal_polygons[1, ]
image_source_folder = file.path(datadir, "raw-images-full/delta")
image_subset_dest_folder = file.path(datadir, "raw-images-subset/delta1")

# Read the image exif to get coords and produce a sf object of image points
image_files = list.files(photo_source_folder, pattern = ".JPG", full.names = TRUE, recursive = TRUE)
exif = read_exif(image_files, tags = c("GPSLatitude", "GPSLongitude", "DateTimeOriginal"))
image_points = st_as_sf(exif, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)

image_points_subset = st_intersection(image_points, focal_polygon)

# Make path to the folder to copy the images to
image_points_subset$rel_path = str_replace(image_points_subset$SourceFile, image_source_folder, "") |>
  # remove a leading slash
  str_replace("^/", "")

image_points_subset$dest_path = file.path(image_subset_dest_folder, image_points_subset$rel_path)

# Make the destination directories
dest_dirs = unique(dirname(image_points_subset$dest_path))
sapply(dest_dirs, function(x) {
  if (!dir.exists(x)) {
    dir.create(x, recursive = TRUE)
  }
})

# Copy the subset of images to the destination
file.link(image_points_subset$SourceFile, image_points_subset$dest_path)
