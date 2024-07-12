# Functions to extract metadata from a drone imagery dataset, using an exif dataframe that has
# already been prepped using prep_exif.


# Flight speed in meters per second
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
  speed = median(speed, na.rm = TRUE) |> round(2)

  return(speed)

}

# Get a polygon of the mission
# image_merge_distance: The horizontal distance between images below which they are merged into one
# mission polygon. Keep only contiguous patches > min_contig_area.
#' @export
extract_mission_polygon = function(exif, image_merge_distance, min_contig_area = 1600) {

  exif = sf::st_transform(exif, 3310)
  ptbuff = sf::st_buffer(exif, image_merge_distance)
  polybuff = sf::st_union(ptbuff)
  poly = sf::st_buffer(polybuff, -image_merge_distance + 1) |> sf::st_cast("MULTIPOLYGON")

  n_polys = length(sf::st_geometry(poly)[[1]])

  if (n_polys == 0) {
    stop("No contiguous images in dataset ", exif$dataset_id[1])
  }


  # Check if multipolygon and if so, keep only the poly parts > min aea, and if any removed, return
  # warning of how many removed
  if (n_polys > 1) {

    parts = sf::st_cast(poly, "POLYGON")
    areas = sf::st_area(parts)
    max_area = max(areas)
    parts_keep_idx = which(areas == max_area | areas > units::set_units(min_contig_area, "m2"))
    parts_filtered = parts[parts_keep_idx]
    poly = parts_filtered |> sf::st_union() |> sf::st_cast("MULTIPOLYGON")

    n_polys_filtered = length(parts_filtered)

    if (n_polys_filtered < n_polys) {
      warning(n_polys, " non-contiguous image clusters in dataset ", exif$dataset_id[1], ". Retaining only the ", n_polys_filtered, " clusters with area > ", min_contig_area, " m^2.")
    } else {
      warning(n_polys, " non-contiguous image clusters in dataset ", exif$dataset_id[1], ". Retaining all becaus all have area > ", min_contig_area, " m^2.")
    }

  } else if (n_polys == 0) {
    stop("No contiguous images in dataset ", exif$dataset_id[1])
  }


  polysimp = sf::st_simplify(poly, dTolerance = 10) |> sf::st_cast("MULTIPOLYGON")

  return(polysimp)

}

# Extract camera pitch, detecting if smart oblique, and if so, report the oblique value (which is
# unfortunatley reported identically whether forward or backward)
#' @export
extract_camera_pitch_summary <- function(exif) {
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

  processed_pitch = processed_pitch |> as.numeric() |> round(2)

  return(list(camera_pitch_derived = processed_pitch, smart_oblique_derived = smart_oblique))
}


# Various date and time summaries
extract_dates_times = function(exif) {

  earliest_date = min(exif$capture_datetime) |>
    format("%Y-%m-%d")

  earliest_datetime = min(exif$capture_datetime) |>
    format("%Y-%m-%d %H:%M:%S")

  latest_datetime = max(exif$capture_datetime) |>
    format("%Y-%m-%d %H:%M:%S")

  # Is the dataset all from a single date?
  single_date = length(unique(lubridate::as_date(exif$capture_datetime))) == 1

  earliest_time = format(exif$capture_datetime, "%H:%M:%S") |>
    min()

  latest_time = format(exif$capture_datetime, "%H:%M:%S") |>
    max()
    
  earliest_year = exif$capture_datetime |>
    format("%Y") |>
    min()

  ret = list(earliest_date_derived = earliest_date,
             earliest_datetime_local_derived = earliest_datetime,
             latest_datetime_local_derived = latest_datetime,
             single_date_derived = single_date,
             earliest_time_local_derived = earliest_time,
             latest_time_local_derived = latest_time,
             earliest_year_derived = earliest_year)

  return(ret)

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
  flight_terrain_correlation = cor(exif_elev_core, ground_elev_core) |> round(2)

  return(flight_terrain_correlation)

}


# Mission centroid coords
extract_mission_centroid_sf = function(exif) {

  #Getting the coordinates listed in the exif file and for it to seperate into long/lat

  # Making it a multipoint
  multipoint <- sf::st_union(exif)

  # Calculate the centroid of the MULTIPOINT geometry
  centroid <- sf::st_centroid(multipoint)

  return(centroid)

}

centroid_sf_to_lonlat = function(centroid) {

  coords = sf::st_coordinates(centroid)
  lon = coords[1, 1] |> as.numeric()
  lat = coords[1, 2] |> as.numeric()

  ret = list(centroid_lon_derived = lon, centroid_lat_derived = lat)
  return(ret)
}

solarnoon_from_centroid_and_date = function(centroid, date) {

  # Seperate the date from the time, since we only need the date to run this function
  date = stringr::str_split(date, " ", simplify = TRUE)[1]

  sncalc <- suntools::solarnoon(sf::st_as_sf(centroid), as.POSIXct(date), POSIXct.out = TRUE)

  solarnoon = sncalc$time |> format("%H:%M:%S")
  # The time is in UTC

  return(solarnoon)
}



#### dataset_id ####

#' Extract dataset id
#'
#' Pulls the dataset id to include in dataset-level metadata. Relies on dataset_id being set
#' properly by the prep_exif function.
#'
#' @param exif the exif metadata file
#'
#' @return dataset id
#'
#' @examples
#' extract_dataset_id(exif)
#'
#' @export
extract_dataset_id_summary = function (exif) {

  if (is.null(exif$dataset_id[1])) {
    stop("Dataset ID not set in exif dataframe. Set the dataset_id column in the exif dataframe before calling this function.")
  }

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
extract_file_size_summary = function (exif) {

  file_size = sum(exif$FileSize) / 1000000000

  file_size = round(file_size, 2)

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
extract_pct_images_rtk = function (exif) {

  rtk_fix = extract_rtk_fix(exif)

  percent_images_rtk = round((sum(rtk_fix == TRUE) / nrow(exif)) * 100)

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
#' extract_white_balance_mode_and_prop(exif)
#'
#' @export
extract_white_balance_summary = function (exif) {

  white_balance = extract_white_balance(exif)

  unique_white_balance <- unique(white_balance)
  white_balance_mode_derived = unique_white_balance[which.max(tabulate(match(white_balance, unique_white_balance)))][1]

  white_balance_pct_mode_derived = round((sum(white_balance == white_balance_mode_derived) / nrow(exif)) * 100)

  ret = data.frame(white_balance_mode_derived, white_balance_pct_mode_derived)

  return(ret)

}


#### exposure_median_derived ####

#' Extracts the median exposure time and its coefficient of variation
#'
#' The median exposure time across all images in the dataset. Units: sec
#'
#' @param exif the exif metadata file
#'
#' @return the median exposure time across all images in the dataset. Units: sec
#'
#' @examples
#' extract_exposure(exif)
#'
#' @export
extract_exposure_summary = function (exif) {

  exposure_median_derived = median(exif$ExposureTime)
  exposure_stdev_derived = sd(exif$ExposureTime)
  exposure_cv_derived = exposure_stdev_derived / exposure_median_derived
  exposure_median_derived = round(exposure_median_derived, 6)
  exposure_stdev_derived = round(exposure_stdev_derived, 6)
  exposure_cv_derived = round(exposure_cv_derived, 2)

  ret = data.frame(exposure_median_derived = exposure_median_derived,
                   exposure_cv_derived)

  return(ret)

}


#### area_ha_and_image_density ####

#' Extracts the the area of the mission footprint and the image density of the dataset
#'
#' area_ha: Area of mission footprint. Units: ha. image_density: the image density computed with image count and footprint. Units: img/ha
#'
#' @param exif the exif metadata file
#'
#' @param mission_polygon the sf polygon object of the mission footprint
#'
#' @return the area of the mission footprint (ha) and the image density based on image count and footprint (img/ha)
#'
#' @examples
#' extract_area_and_density(exif)
#'
#' @export
extract_area_and_density = function(exif, mission_polygon) {

  area_ha_derived = units::set_units(sf::st_area(mission_polygon), "hectare")

  # Crop images to the mission polygon, in case there were outlier images, or smaller outlier
  # polygons of images that were removed in the mission polygon creation
  intersects = sf::st_intersects(exif,
                                 mission_polygon |>
                                   sf::st_transform(sf::st_crs(exif)),
                                 sparse = FALSE)
  intersects = apply(intersects, 1, any, simplify = TRUE)
  imgs_intersecting = exif[intersects, ]

  image_density_derived = (nrow(imgs_intersecting)) / area_ha_derived

  area_derived = round(area_ha_derived, 2)
  image_density_derived = round(image_density_derived, 2)

  ret = data.frame(area_derived, image_density_derived)

  return(ret)

}


# Image frequency (imgs/sec)
extract_image_frequency <- function(exif) {
  # Convert DateTimeOriginal to datetime object
  datetime <- lubridate::as_datetime(exif$capture_datetime)

  # Calculate time difference between consecutive images
  time_diff <- diff(datetime)

  # Calculate time difference in seconds
  time_diff_seconds <- as.numeric(time_diff, units = "secs")

  # Remove NA and infinity values
  time_diff_seconds <- time_diff_seconds[is.finite(time_diff_seconds)]

  # Calculate 20th and 80th percentiles for outlier exclusion
  time_diff_20th <- quantile(time_diff_seconds, 0.2)
  time_diff_80th <- quantile(time_diff_seconds, 0.8)

  # Exclude outliers based on quantiles
  filtered_time_diff <- time_diff_seconds[time_diff_seconds >= time_diff_20th & time_diff_seconds <= time_diff_80th]

  mean_time_diff = mean(filtered_time_diff, na.rm = TRUE)
  
  if(mean_time_diff == 0) {
    warning("Mean image capture rate is < 0.5 seconds; cannot calculate capture rate from capture time. Assuming 0.1 sec.")
    mean_time_diff = 0.1
  }

  # Calculate image frequency (images per second) from filtered values
  mean_image_frequency <- 1 / mean_time_diff

  mean_image_frequency = round(mean_image_frequency, 2)

  return(mean_image_frequency)
}

# Resolution and aspect ratio
extract_resolution_and_aspect_ratio_summary <- function(exif) {

  # Get Xresolution and Yresolution from the EXIF data
  resolution_x <- unique(exif$ImageWidth)
  resolution_y <- unique(exif$ImageHeight)

  # Calculate mode resolution
  mode_resolution_x <- as.numeric(names(sort(table(resolution_x), decreasing = TRUE)[1]))
  mode_resolution_y <- as.numeric(names(sort(table(resolution_y), decreasing = TRUE)[1]))

  # Get aspect ratio
  aspect_ratio <- round(mode_resolution_x / mode_resolution_y, 2)

  return(list(resolution_x_derived = mode_resolution_x,
              resolution_y_derived = mode_resolution_y,
              aspect_ratio_derived = aspect_ratio))
}


# File type
extract_file_format_summary <- function(exif) {
  
  # Get image file format
  image_file_format <- unique(exif$FileType)

  # Check if image file format is consistent
  if (length(unique(image_file_format)) > 1) {
    # If not identical, return a warning
    warning("Image file format varies across images.")
  }

  # Calculate mode file format
  file_format <- names(sort(table(image_file_format), decreasing = TRUE)[1])

  return(file_format)
}


# Preps the EXIF data for passing to the
# extraction functions, then calls all the individual extraction functions to extract the respecive attributes.
# crop_to_contiguous: Keeps only the images within the sets of contiguous images that are larger
# than min_contig_areain m^2 (could be multiple clumps). It will always include the largest clump, even if
# it is smaller than min_contig_area.
#' @export
extract_imagery_dataset_metadata = function(input,
                                            input_type = "dataframe",
                                            plot_flightpath = FALSE,
                                            crop_to_contiguous = TRUE,
                                            min_contig_area = 1600) {

  if (input_type == "filepath") {
    exif = prep_exif(input, plot_flightpath = plot_flightpath)
  } else if (input_type == "dataframe") {
    exif = input
  }

  # Compute geospatial features
  mission_polygon = extract_mission_polygon(exif, image_merge_distance = 50, min_contig_area = min_contig_area)

  if (crop_to_contiguous) {

    # Keep only the images within the largest contiguous patch of images, buffered to 10 m to
    # account for the simplified polygon
    polygon_proj_buffer = mission_polygon |> sf::st_transform(3310) |> sf::st_buffer(10)
    exif_proj = sf::st_transform(exif, 3310)
    intersection_idxs = sf::st_intersects(exif_proj, polygon_proj_buffer, sparse = FALSE)
    full_exif_length = nrow(exif)
    exif = exif[intersection_idxs[, 1], ]
    cropped_exif_length = nrow(exif)
    if (cropped_exif_length < full_exif_length) {
      n_cropped = full_exif_length - cropped_exif_length
      message("Dropped ", n_cropped, " images that were not within the largest contiguous patch(es) of images retained for dataset ", exif$dataset_id[1], ".")
    }

  }

  images_retained = extract_image_id(exif)

  # Extract/compute metadata attributes
  dataset_id = extract_dataset_id_summary(exif)
  flight_speed_derived = extract_flight_speed(exif)
  flight_terrain_correlation_derived = extract_flight_terrain_correlation(exif)
  camera_pitch = extract_camera_pitch_summary(exif)
  dates_times = extract_dates_times(exif)
  centroid_internal = extract_mission_centroid_sf(exif)
  centroid_lonlat = centroid_sf_to_lonlat(centroid_internal)
  solarnoon_utc_derived = solarnoon_from_centroid_and_date(centroid_internal, dates_times$earliest_date_derived)
  image_count_derived = extract_image_count(exif)
  file_size_derived = extract_file_size_summary(exif)
  percent_images_rtk_derived = extract_pct_images_rtk(exif)
  white_balance = extract_white_balance_summary(exif)
  exposure = extract_exposure_summary(exif)
  area_and_density = extract_area_and_density(exif, mission_polygon)
  image_frequency_derived = extract_image_frequency(exif)
  resolution_and_aspect_ratio = extract_resolution_and_aspect_ratio_summary(exif)
  file_format_derived = extract_file_format_summary(exif)

  # Return extracted/computed metadata as a data frame row
  dataset_metadata = data.frame(
    dataset_id,
    flight_speed_derived,
    flight_terrain_correlation_derived,
    camera_pitch, # this is a multi-column dataframe; preserving its column names
    dates_times,
    centroid_lonlat, # this is a multi-column dataframe; preserving its column names
    solarnoon_utc_derived,
    image_count_derived,
    file_size_derived,
    percent_images_rtk_derived,
    white_balance, # this is a multi-column dataframe; preserving its column names
    exposure, # this is a multi-column dataframe; preserving its column names
    area_and_density, # this is a multi-column dataframe; preserving its column names
    image_frequency_derived,
    resolution_and_aspect_ratio,
    file_format_derived
  )

  mission_polygon = sf::st_as_sf(mission_polygon)
  mission_polygon$dataset_id = dataset_id

  return(list(dataset_metadata = dataset_metadata, mission_polygon = mission_polygon, images_retained = images_retained))

}
