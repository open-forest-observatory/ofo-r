# Read in the EXIF data from a set of absolute image paths and drop the ThumbnailImage and
# PreviewImage attributes if they exist, and convert all cols to character
#' @export
read_exif_drop_thumbnails = function(image_paths) {
  exif = exifr::read_exif(image_paths)

  exif = exif |>
    # Remove thumbnail data
    dplyr::select(-dplyr::any_of(c("ThumbnailImage", "PreviewImage"))) |>
    # Convert all cols to character
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    # Make sure SourceFile is in R-friendly format
    dplyr::mutate(SourceFile = image_paths)
}
