# This script defines dataset-level metadata extraction functions
# Written by Emily Marie Purvis, last updated April 24th 2024

library(devtools)

#### Step 1: Define image-level metadata extraction functions ####

#### dataset_id ####

#' Extract dataset id
#'
#' Pulls the dataset id to include in dataset-level metadata
#'
#' @param exif the exif metadata file
#'
#' @return dataset id
#'
#' @examples
#' extract_dataset_id(exif)
#'
#' @export

extract_dataset_id_dataset_level = function (exif) {

  dataset_id_dataset_level = exif$dataset_id[1]

  return(dataset_id_dataset_level)

}

#### image_count ####

#' Extract number of images in an exif
#'
#' Pulls the image count to include in dataset-level metadata
#'
#' @param exif the exif metadata file
#'
#' @return image count
#'
#' @examples
#' extract_image_count(exif)
#'
#' @export

extract_image_count = function (exif) {

  image_count = nrow(exif)

  return(image_count)

}

#### file_size ####

#' Extracts total file size
#'
#' Total file size of all images in set. Units: GB.
#'
#' @param exif the exif metadata file
#'
#' @return total file size
#'
#' @examples
#' extract_file_size(exif)
#'
#' @export

extract_file_size = function (exif) {

  file_size = sum(exif$FileSize) / 1000000000

  return(file_size)

}

#### percent_images_rtk ####

#' Extracts percent of images with an RTK fix
#'
#' Percent of images with an RTK fix
#'
#' @param exif the exif metadata file
#'
#' @return percent of images with an RTK fix
#'
#' @examples
#' extract_precent_images_rtk(exif)
#'
#' @export

extract_percent_images_rtk = function (exif) {

  rtk_fix = extract_rtk_fix(exif)

  percent_images_rtk = (sum(rtk_fix == TRUE) / nrow(exif)) * 100

  return(percent_images_rtk)

}

#### white_balance_mode_and_prop_derived ####

#' Extracts the most common white balance setting (mode_derived) the proportion of images with a white balance setting that matches the most common white balance setting in the dataset (mode_prop_derived)
#'
#' A data.frame of white_balance_mode_derived and white_balance_prop_derived. White_balance_mode_derived: The most common white balance setting across all images in the project. Options: auto and manual. White_balance_mode_prop_derived: The proportion of images with a white balance setting that matches the modal white balance setting in the dataset.
#'
#' @param exif the exif metadata file
#'
#' @return a data.frame of the most common white balance setting and the proportion of images matching the most common white balance setting
#'
#' @examples
#' extract_white_balance_mode_and_prop_derived(exif)
#'
#' @export

extract_white_balance_mode_and_prop_derived = function (exif) {

  white_balance = extract_white_balance(exif)

  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  white_balance_mode_derived = Mode(white_balance)

  white_balance_mode_prop_derived = (sum(white_balance == white_balance_mode_derived) / nrow(exif))

  white_balance_mode_and_prop_derived = data.frame (white_balance_mode_derived, white_balance_mode_prop_derived)

  return(white_balance_mode_and_prop_derived)

}

#### white_balance_mode_prop_derived ####

#' Extracts the proportion of images with a white balance setting that matches the most common white balance setting in the dataset
#'
#' The proportion of images with a white balance setting that matches the modal white balance setting in the dataset
#'
#' @param exif the exif metadata file
#'
#' @return the proportion of images matching the most common white balance setting
#'
#' @examples
#' extract_white_balance_mode_prop_derived(exif)
#'
#' @export

extract_white_balance_mode_prop_derived = function (exif) {

  exif$white_balance = extract_white_balance(exif)

  white_balance_mode_derived = extract_white_balance_mode_derived(exif)

  white_balance_mode_prop_derived = (sum(exif$white_balance == white_balance_mode_derived) / nrow(exif))

  return(white_balance_mode_prop_derived)

}

#### exposure_median_derived ####

#' Extracts the median exposure time
#'
#' The median exposure time across all images in the dataset. Units: sec
#'
#' @param exif the exif metadata file
#'
#' @return the median exposure time across all images in the dataset. Units: sec
#'
#' @examples
#' extract_exposure_median_derived(exif)
#'
#' @export

extract_exposure_median_derived = function (exif) {

  exposure_median_derived = median(exif$ExposureTime)

  return(exposure_median_derived)

}

#### exposure_stdev_derived ####

#' Extracts the standard deviation of exposure time
#'
#' The standard deviation of exposure time across all images in the dataset. Units: sec
#'
#' @param exif the exif metadata file
#'
#' @return the standard deviation of exposure time across all images in the dataset. Units: sec
#'
#' @examples
#' extract_exposure_stdev_derived(exif)
#'
#' @export

extract_exposure_stdev_derived = function (exif) {

  exposure_stdev_derived = sd(exif$ExposureTime)

  return(exposure_stdev_derived)

}

#### area_ha_and_image_density ####

#' Extracts the the area of the mission footprint and the image density of the dataset
#'
#' area_ha: Area of mission footprint. Units: ha. image_density: the image density computed with image count and footprint. Units: img/ha
#'
#' @param exif the exif metadata file
#'
#' @param image_merge_distance the horizontal distance between images below which they are merged into one mission polygon
#'
#' @return the area of the mission footprint (ha) and the image density based on image count and footprint (img/ha)
#'
#' @examples
#' extract_area_ha_and_image_density(exif)
#'
#' @export

extract_area_ha_and_image_density = function (exif, image_merge_distance) {

  area_ha = units::set_units(sf::st_area(create_mission_polygon(exif, image_merge_distance)), "hectare")

  image_density = (nrow(exif))/(units::set_units(sf::st_area(create_mission_polygon(exif, image_merge_distance)), "hectare"))

  area_ha_and_image_density = data.frame (area_ha, image_density)

  return(area_ha_and_image_density)

}

#### Step 2: Create wrapper for metadata extraction functions. Preps the EXIF data for passing to the extraction functions, then calls all the individual extraction functions to extract the respective attributes. ####

#' Returns a collection of dataset-level metadata parameters
#'
#' @param exif_file the exif filepath (before being prepared to pass to the functions in the wrapper)
#'
#'#' @param image_merge_distance The horizontal distance between images below which they are merged into one mission polygon
#'
#' @return a data.frame of dataset_id_dataset_level, image_count, file_size, percent_images_rtk, white_balance_mode_derived, white_balance_mode_prop_derived, exposure_median_derived, exposure_stdev_derived, image_density, and area_ha
#'
#' @examples
#' extract_dataset_metadata_emp(exif)
#'
#' @export

extract_dataset_metadata_emp = function(exif_filepath, image_merge_distance) {

  # Prep the EXIF data for extraction of metadata attributes
  exif = prep_exif(exif_filepath)

  # Extract/compute metadata attributes
  dataset_id_dataset_level = extract_dataset_id_dataset_level(exif)
  image_count = extract_image_count(exif)
  file_size = extract_file_size(exif)
  percent_images_rtk = extract_percent_images_rtk(exif)
  white_balance_mode_and_prop_derived = extract_white_balance_mode_and_prop_derived(exif)
  exposure_median_derived = extract_exposure_median_derived(exif)
  exposure_stdev_derived = extract_exposure_stdev_derived(exif)
  area_ha_and_image_density = extract_area_ha_and_image_density(exif, image_merge_distance)

  # Return extracted/computed metadata as a data frame row
  metadata = data.frame(dataset_id_dataset_level = dataset_id_dataset_level,
                        image_count = image_count,
                        file_size = file_size,
                        percent_images_rtk = percent_images_rtk,
                        white_balance_mode_and_prop_derived,
                        exposure_median_derived = exposure_median_derived,
                        exposure_stdev_derived = exposure_stdev_derived,
                        area_ha_and_image_density
  )

  return(metadata)

}
