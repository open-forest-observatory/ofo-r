# Functions to extract metadata from a drone imagery dataset, using an exif dataframe that has
# already been prepped using prep_exif.


# Flight speed
extract_flight_speed = function(exif) {

  # Get distance from each image to the next, in meters
  start_image = exif[1:(nrow(exif) - 1), ]
  end_image = exif[2:nrow(exif), ]
  distance = sf::st_distance(start_image, end_image, by_element = TRUE)

  # Get time from each image to the next, in seconds
  interval = end_image$capture_datetime - start_image$capture_datetime

  # Compute speed in meters per second
  speed = as.numeric(distance) / as.numeric(interval)

  # Compute the median speed as the value to report, to avoid influence of outliers (such as battery
  # swaps)
  speed = median(speed, na.rm = TRUE)

  return(speed)

}

# Get a polygon of the mission
# image_merge_distance: The horizontal distance between images below which they are merged into one
# mission polygon
#' @export
extract_mission_polygon = function(exif, image_merge_distance) {

  exif = sf::st_transform(exif, 3310)
  ptbuff = sf::st_buffer(exif, image_merge_distance)
  polybuff = sf::st_union(ptbuff)
  poly = sf::st_buffer(polybuff, -image_merge_distance + 1)

  # Check if multipolygon and if so, return warning and keep only largest polygon
  n_polys = length(sf::st_geometry(poly)[[1]])
  if (n_polys > 1) {

    warning("Non-contiguous images in dataset ", exif$dataset_id[1], ". Dropping all but largest contiguous set.")

    parts = sf::st_cast(poly, "POLYGON")
    areas = sf::st_area(parts)
    part = parts[which.max(areas)]
    poly = part
  } else if (n_polys == 0) {
    stop("No contiguous images in dataset ", exif$dataset_id[1])
  }

  polysimp = sf::st_simplify(poly, dTolerance = 10)

  return(polysimp)

}

# Extract camera pitch, detecting if smart oblique, and if so, report the oblique value (which is
# unfortunatley reported identically whether forward or backward)
#' @export
extract_camera_pitch <- function(exif) {
  # Extract CameraPitch directly from exif and adjust pitch values so 0 is nadir. Also take abs val
  # just in case the pitch is reported as negative for backward (even thouth it apparently never
  # is), so that for smart-oblique missions, the low quantile represents nadir and the high quantile
  # represents the oblique mission pitch
  camera_pitch_values <- abs(as.numeric(exif$CameraPitch) + 90)

  quantiles <- quantile(camera_pitch_values, c(0.1, 0.5, 0.9), na.rm = TRUE)

  # Check if the range between 0.1 and 0.9 quantiles is at least 10 degrees
  pitch_range <- quantiles[3] - quantiles[1]
  if (pitch_range >= 10) {
    # If so, then infer it's smart-oblique
    # Use 0.9 quantile directly as the mission pitch
    processed_pitch <- quantiles[3]
    smart_oblique = TRUE
  } else {
    # For non-smart oblique missions, use the median pitch value
    processed_pitch <- abs(quantiles[2])
    smart_oblique = FALSE
  }


  return(list(processed_pitch = processed_pitch, smart_oblique = smart_oblique))
}


# Date of the earliest image capture in YYYYMMDD
extract_earliest_date = function(exif) {

  earliest_date = min(exif$capture_datetime) |>
    lubridate::format("%Y%m%d")

  return(earliest_date)

}

# Is the dataset all from a single date?
extract_single_date = function(exif) {

  single_date = length(unique(lubridate::as_date(exif$capture_datetime))) == 1

  return(single_date)

}

# Get the correlation between the altitude of the drone and the ground elevation (i.e. trerrain
# follow tightness, using terrain data from AWS via elevatr package)
extract_flight_terrain_correlation = function(exif) {
  # Define the AOI as a polygon
  aoi = sf::st_convex_hull(sf::st_union(exif)) |> sf::st_as_sf()

  # Get an elev raster for this AOI
  dem = elevatr::get_elev_raster(aoi, z = 14, prj = 4326, src = "aws")

  ## Try it with USGS dem

  # Get the ground elevation beneath all the photo points
  ground_elev = terra::extract(dem, exif, method = "bilinear")

  # Get the difference between the drone's altitude and the ground elevation
  agl = exif$GPSAltitude - ground_elev

  # Get the middle 80% of AGL (to exclude outliers like landscape shots in mission)
  agl_lwr = quantile(agl, 0.1)
  agl_upr = quantile(agl, 0.9)

  agl_core = agl[agl > agl_lwr & agl < agl_upr]
  exif_elev_core = exif$GPSAltitude[agl > agl_lwr & agl < agl_upr]
  ground_elev_core = ground_elev[agl > agl_lwr & agl < agl_upr]

  # Get the correlation between the altitude of the drone and the ground elevation (i.e. trerrain
  # follow tightness)
  flight_terrain_correlation = cor(exif_elev_core, ground_elev_core)

  return(flight_terrain_correlation)

}

# Get flight elev AGL based on drone-recorded altitude (assuming we can trust it) and an opentopo DEM
# TODO: this is mostly redundant with the above and should be combined if keeping elev extraction
extract_flight_elev_agl = function(exif) {
  # Define the AOI as a polygon
  aoi = sf::st_convex_hull(sf::st_union(exif)) |> sf::st_as_sf()

  # Get an elev raster for this AOI
  dem = elevatr::get_elev_raster(aoi, z = 14, prj = 4326, src = "aws")

  # Get the ground elevation beneath all the photo points
  ground_elev = terra::extract(dem, exif, method = "bilinear")

  # Get the difference between the drone's altitude and the ground elevation
  agl = exif$GPSAltitude - ground_elev

  # Get the middle 80% of AGL (to exclude outliers like landscape shots in mission)
  agl_lwr = quantile(agl, 0.1)
  agl_upr = quantile(agl, 0.9)

  agl_core = agl[agl > agl_lwr & agl < agl_upr]
  exif_elev_core = exif$GPSAltitude[agl > agl_lwr & agl < agl_upr]
  ground_elev_core = ground_elev[agl > agl_lwr & agl < agl_upr]

  # Get the correlation between the altitude of the drone and the ground elevation (i.e. trerrain
  # follow tightness)
  flight_elev_agl = exif_elev_core - ground_elev_core

  return(flight_elev_agl)

}


# Mission centroid coords
extract_mission_centroid_sf = function(exif) {

  #Getting the coordinates listed in the exif file and for it to seperate into long/lat

  # Making it a multipoint
  multipoint <- st_union(exif)

  # Calculate the centroid of the MULTIPOINT geometry
  centroid <- st_centroid(multipoint)

  return(centroid)

}

centroid_sf_to_lonlat = function(centroid) {

  coords = st_coordinates(centroid)
  lon = coords[1, 1]
  lat = coords[1, 2]

  ret = list(lon = lon, lat = lat)
  return(ret)
}

solarnoon_from_centroid_and_date = function(centroid, date) {

  # Seperat the date from the time, since we only need the date to run this function
  date = str_split(exif$capture_datetime[1], " ", simplify = TRUE)[1]

  sncalc <- solarnoon(matrix(c(coordinate), nrow = 1),as.POSIXct(date),POSIXct.out=TRUE)

  sncalc
}

extract_earliest_latest_datetime = function(exif) {

  etl <- min(exif$capture_datetime)
  earliest_time_local <- lubridate::format(etl, "%Y%m%dT%H%M%S")
  earliest_time_local

  ltl <- max(exif$capture_datetime)
  latest_time_local <- lubridate::format(ltl, "%Y%m%dT%H%M%S")
  latest_time_local

  ret = list(earliest_time_local = earliest_time_local, latest_time_local = latest_time_local)
  return(ret)

}



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

  area_ha_and_image_density = data.frame(area_ha, image_density)

  return(area_ha_and_image_density)

}


#### SD

# Image frequency (imgs/sec)
extract_image_frequency <- function(exif) {
  # Convert DateTimeOriginal to datetime object
  exif$DateTimeOriginal <- as.POSIXct(exif$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")

  # Calculate time difference between consecutive images
  time_diff <- diff(exif$DateTimeOriginal)

  # Calculate time difference in seconds
  time_diff_seconds <- as.numeric(time_diff, units = "secs")

  # Remove NA and infinity values
  time_diff_seconds <- time_diff_seconds[is.finite(time_diff_seconds)]

  # Calculate 10th and 90th percentiles for outlier exclusion
  time_diff_10th <- quantile(time_diff_seconds, 0.1)
  time_diff_90th <- quantile(time_diff_seconds, 0.9)

  # Exclude outliers based on quantiles
  filtered_time_diff <- time_diff_seconds[time_diff_seconds >= time_diff_10th & time_diff_seconds <= time_diff_90th]

  # Calculate image frequency (images per second) from filtered values
  image_frequency <- 1 / filtered_time_diff
  mean_image_frequency <- mean(image_frequency, na.rm = TRUE)

  return(mean_image_frequency)
}

# Resolution and aspect ratio
extract_resolution_and_aspect_ratio <- function(exif) {

  # Get Xresolution and Yresolution from the EXIF data
  resolution_x <- unique(exif$XResolution)
  resolution_y <- unique(exif$YResolution)

  # Calculate mode resolution
  mode_resolution_x <- as.numeric(names(sort(table(resolution_x), decreasing = TRUE)[1]))
  mode_resolution_y <- as.numeric(names(sort(table(resolution_y), decreasing = TRUE)[1]))

  # Get aspect ratio
  aspect_ratio <- mode_resolution_x / mode_resolution_y

  return(list(resolution_x = mode_resolution_x,
              resolution_y = mode_resolution_y,
              aspect_ratio = aspect_ratio))
}


# File type
extract_file_type <- function(exif) {
  
  # Get image file format
  image_file_format <- unique(exif$FileType)

  # Check if image file format is consistent
  if (length(unique(image_file_format)) > 1) {
    # If not identical, return a warning
    warning("Image file format varies across images.")
  }

  # Calculate mode file format
  mode_image_file_format <- names(sort(table(image_file_format), decreasing = TRUE)[1])

  return(mode_image_file_format)
}

# Earliest and latest mission times
extract_earliest_latest_time_local <- function(exif) {

  earliest_time = lubridate::format("%H%M%S")
    min()

  latest_time = lubridate::format("%H%M%S")
    max()

  return(list(earliest_time = earliest_time, latest_time = latest_time))
}

# Wrapper for Derek's metadata extraction functions. Preps the EXIF data for passing to the
# extraction functions, then calls all the individual extraction functions to extract the respecive attributes.
# crop_to_contiguous: Keeps only the images within the largest contiguoug patch of images (which is
# what is returned by create_mission_polygon)
extract_metadata_dy = function(exif_filepath, plot_flightpath = FALSE, crop_to_contiguous = FALSE) {

  # Prep the EXIF data for extraction of metadata attributes
  exif = prep_exif(exif_filepath, plot_flightpath = plot_flightpath)

  # Compute geospatial features
  mission_polygon = create_mission_polygon(exif, image_merge_distance = 50)

  if (crop_to_contiguous) {

    # Keep only the images within the largest contiguous patch of images
    polygon_proj_buffer = mission_polygon |> sf::st_transform(3310) |> sf::st_buffer(20)
    exif_proj = sf::st_transform(exif, 3310)
    intersection_idxs = sf::st_intersects(exif_proj, polygon_proj_buffer, sparse = FALSE)
    exif = exif[intersection_idxs[,1], ]
  }

  # Extract/compute metadata attributes
  flight_speed_derived = extract_flight_speed(exif)
  flight_terrain_correlation = extract_flight_terrain_correlation(exif)
  flight_elev_agl = extract_flight_elev_agl(exif)

  # Return extracted/computed metadata as a data frame row
  metadata = data.frame(dataset_id = exif$dataset_id[1],
                        flight_speed_derived = flight_speed_derived,
                        flight_terrain_correlation = flight_terrain_correlation,
                        flight_elev_agl = flight_elev_agl
                        # Add more metadata variables here
                        )

  ret = list(metadata = metadata,
             mission_polygon = mission_polygon)

  return(ret)

}

# Post-processes the data returned by extract_metadata(_dy): Combines the extracted mission polygon
# and metadata by applying the metadata as attributes of the polygon
get_mission_polygon_w_metadata = function(exif_file, image_merge_distance = 10) {

  message("Extracting metadata and mission polygon from ", exif_file)

  # Get the metadata and mission polygon
  mission = extract_metadata_dy(exif_file, plot_flightpath = FALSE, crop_to_contiguous = TRUE)

  # Add the metadata as attributes to the mission polygon
  mission_polygon = mission$mission_polygon |> sf::st_as_sf()
  mission_polygon_attributed = dplyr::bind_cols(mission_polygon, mission$metadata)
  
  # Return the attributed mission polygon
  return(mission_polygon_attributed)
}