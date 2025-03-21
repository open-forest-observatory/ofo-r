# Functions to take image-level metadata that has already been assigned standardized column names
# (regardless of drone platform) and compute summary metrics at the dataset level.


# Flight speed in meters per second
extract_flight_speed = function(metadata) {
  # Get distance from each image to the next, in meters
  start_image = metadata[1:(nrow(metadata) - 1), ]
  end_image = metadata[2:nrow(metadata), ]
  distance = sf::st_distance(start_image, end_image, by_element = TRUE)

  # Get time from each image to the next, in seconds
  interval = end_image$datetime_local - start_image$datetime_local

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
extract_mission_polygon = function(
    metadata,
    dataset_id,
    image_merge_distance,
    min_contig_area = 1600,
    boundary_method = "dilate_erode",
    simplification_tol = 10.0,
    identify_images_in_polygon = FALSE) {
  # Check if the data is a data_frame, in which case it needs to be converted into an SF object.
  # Otherwise, it's assumed to be a valid SF object.
  if (is.data.frame(metadata)) {
    # TODO ensure that these columns are always the correct/only ones to use
    metadata = sf::st_as_sf(metadata, crs = 4326, coords = c("lon", "lat"))
  }

  # Retain the initial CRS to transform back to at the end
  initial_crs = sf::st_crs(metadata)

  # Transform to a meters-based CRS that is appropriate for that region
  metadata = transform_to_local_utm(metadata)

  # Extract the boundary using one of two different methods
  if (boundary_method == "concaveman") {
    poly = concaveman::concaveman(metadata, concavity = 4)
    poly = poly |> sf::st_cast("MULTIPOLYGON")
  } else if (boundary_method == "dilate_erode") {
    ptbuff = sf::st_buffer(metadata, image_merge_distance)
    polybuff = sf::st_union(ptbuff)
    # This line is dangerous for single lines of images since
    poly = sf::st_buffer(polybuff, -image_merge_distance + 1) |> sf::st_cast("MULTIPOLYGON")
  } else {
    stop(paste("Invalid boundary_method: ", boundary_method, ". Only 'concaveman' and 'dilate_erode' are supported"))
  }

  n_polys = length(sf::st_geometry(poly)[[1]])

  # Error out if there are no contigious images
  if (n_polys == 0) {
    stop("No contiguous images in dataset ", dataset_id)
  }

  # Check if multipolygon and if so, keep only the poly parts > min aea, and if any removed, return
  # warning of how many removed
  if (n_polys > 1) {
    parts = sf::st_cast(poly, "POLYGON")
    areas = sf::st_area(parts)
    max_area = max(areas)
    parts_keep_idx = which(areas == max_area | areas > units::set_units(min_contig_area, "m2"))
    parts_filtered = parts[parts_keep_idx]
    poly = parts_filtered |>
      sf::st_union() |>
      sf::st_cast("MULTIPOLYGON")

    n_polys_filtered = length(parts_filtered)

    # TODO it's possible all polygons could be filtered out in this step
    if (n_polys_filtered < n_polys) {
      warning(
        n_polys,
        " non-contiguous image clusters in dataset ",
        dataset_id,
        ". Retaining only the ",
        n_polys_filtered,
        " clusters with area > ",
        min_contig_area,
        " m^2."
      )
    } else {
      warning(
        n_polys,
        " non-contiguous image clusters in dataset ",
        dataset_id,
        ". Retaining all becaus all have area > ",
        min_contig_area,
        " m^2."
      )
    }
  }
  # Simplify the polygon
  simplified_poly = sf::st_simplify(poly, dTolerance = simplification_tol) |> sf::st_cast("MULTIPOLYGON")

  # If the polygon was simplified so much that it became empty, then redo with less simp
  if (sf::st_is_empty(simplified_poly)) {
    simplified_poly = sf::st_simplify(poly,
                                      dTolerance = simplification_tol / 10) |>
      sf::st_cast("MULTIPOLYGON")
  }

  if (sf::st_is_empty(simplified_poly)) {
    simplified_poly = sf::st_simplify(poly,
                                      dTolerance = simplification_tol / 100) |>
      sf::st_cast("MULTIPOLYGON")
  }

  if (sf::st_is_empty(simplified_poly)) {
    warning("Simplification of polygon for mission/sub-mission",
            dataset_id,
            "resulted in an empty polygon. No images will be retained.")
  }

  # Identify which images are within the polygon if requested
  if (identify_images_in_polygon) {
    # increase the region to account for the simplified polygon
    simplified_poly_buffer = simplified_poly |> sf::st_buffer(simplification_tol)
    intersection_mat = sf::st_intersects(metadata, simplified_poly_buffer, sparse = FALSE)
    # Does the image intersect any of the polygons?
    intersection_bool = apply(intersection_mat, 1, any) 
    # Get the image IDs of the images that intersect the polygon
    intersection_image_ids = metadata[intersection_bool, ]$image_id
    # Transform back to the initial CRS
    simplified_poly = sf::st_transform(simplified_poly, crs = initial_crs) |> sf::st_cast("MULTIPOLYGON")

    return(list(polygon = simplified_poly, intersection_image_ids = intersection_image_ids))
  }

  # Transform back to the initial CRS
  simplified_poly = sf::st_transform(simplified_poly, crs = initial_crs)
  return(simplified_poly)
}

# Extract camera pitch, detecting if smart oblique, and if so, report the oblique value (which is
# unfortunatley reported identically whether forward or backward)
#' @export
extract_camera_pitch_summary <- function(metadata) {
  camera_pitch_values <- abs(as.numeric(metadata$camera_pitch))

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

  processed_pitch = processed_pitch |>
    as.numeric() |>
    round(2)

  return(list(camera_pitch_derived = processed_pitch, smart_oblique_derived = smart_oblique))
}


# Various date and time summaries
extract_dates_times = function(metadata) {
  datetime = metadata$datetime_local

  # See if there are more than one unique dates (days)
  single_date = length(unique(lubridate::as_date(datetime))) == 1

  # Compute earliest and latest dates
  earliest_datetime_obj = min(datetime)
  latest_datetime_obj = max(datetime)

  # Compute different string representations
  earliest_datetime = earliest_datetime_obj |> format("%Y-%m-%d %H:%M:%S")
  latest_datetime = latest_datetime_obj |> format("%Y-%m-%d %H:%M:%S")

  earliest_date = earliest_datetime_obj |> format("%Y-%m-%d")

  earliest_time = earliest_datetime_obj |> format("%H:%M:%S")
  latest_time = latest_datetime_obj |> format("%H:%M:%S")

  earliest_year = earliest_datetime_obj |> format("%Y")

  # Aggregate into a list
  ret = list(
    earliest_date_derived = earliest_date,
    earliest_datetime_local_derived = earliest_datetime,
    latest_datetime_local_derived = latest_datetime,
    single_date_derived = single_date,
    earliest_time_local_derived = earliest_time,
    latest_time_local_derived = latest_time,
    earliest_year_derived = earliest_year
  )

  return(ret)
}


# Get the correlation between the altitude of the drone and the ground elevation (i.e. trerrain
# follow tightness, using terrain data from AWS via elevatr package)
extract_flight_terrain_correlation = function(metadata) {
  # Define the AOI as a polygon
  aoi = sf::st_convex_hull(sf::st_union(metadata)) |> sf::st_as_sf()

  # Get an elev raster for this AOI
  dem = elevatr::get_elev_raster(aoi, z = 14, prj = 4326, src = "aws")

  ## Try it with USGS dem

  # Get the ground elevation beneath all the photo points
  ground_elev = terra::extract(dem, metadata, method = "bilinear")
  drone_altitude = metadata$altitude_asl_drone

  # Get the difference between the drone's altitude and the ground elevation
  agl = drone_altitude - ground_elev

  # Get the middle 80% of AGL (to exclude outliers like landscape shots in mission)
  agl_lwr = quantile(agl, 0.1)
  agl_upr = quantile(agl, 0.9)

  agl_core_mask = agl > agl_lwr & agl < agl_upr

  drone_altitude_core = drone_altitude[agl_core_mask]
  ground_elev_core = ground_elev[agl_core_mask]

  # Get the correlation between the altitude of the drone and the ground elevation (i.e. trerrain
  # follow tightness)
  ground_elev_no_variation = length(unique(ground_elev_core)) == 1
  if (ground_elev_no_variation) {
    # Note that for some small regions the DEM will not have any variation so this
    # correlation would be NA. Instead we set it to 1 in this case, since the primary goal of this
    # metric is to filter out missions with poor terrain following.
    flight_terrain_correlation = 1
  } else {
    # Compute the correlation between the DEM and drone altitude
    flight_terrain_correlation = cor(drone_altitude_core, ground_elev_core) |> round(2)
  }

  return(flight_terrain_correlation)
}


# Mission centroid coords
extract_mission_centroid_sf = function(metadata) {
  # Getting the coordinates listed in the metadata file and for it to seperate into long/lat

  # Making it a multipoint
  multipoint <- sf::st_union(metadata)

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
#' properly by the prep_metadata function.
#'
#' @param metadata the metadata metadata file
#'
#' @return dataset id
#'
#' @examples
#' extract_dataset_id(metadata)
#'
#' @export
extract_dataset_id_summary = function(metadata) {
  if (is.null(metadata$dataset_id[1])) {
    stop("Dataset ID not set in metadata dataframe. Set the dataset_id column in the metadata dataframe before calling this function.")
  }

  dataset_id_dataset_level = metadata$dataset_id[1]

  return(dataset_id_dataset_level)
}

#### image_count ####

#' Extract number of images in an metadata
#'
#' Pulls the image count to include in dataset-level metadata
#'
#' @param metadata the metadata metadata file
#'
#' @return image count
#'
#' @examples
#' extract_image_count(metadata)
#'
#' @export
extract_image_count = function(metadata) {
  image_count = nrow(metadata)

  return(image_count)
}

#### file_size ####

#' Extracts total file size
#'
#' Total file size of all images in set. Units: GB.
#'
#' @param metadata the metadata metadata file
#'
#' @return total file size
#'
#' @examples
#' extract_file_size(metadata)
#'
#' @export
extract_file_size_summary = function(metadata) {
  file_size = sum(metadata$file_size_gb)

  file_size = round(file_size, 2)

  return(file_size)
}

#### percent_images_rtk ####

#' Extracts percent of images with an RTK fix
#'
#' Percent of images with an RTK fix
#'
#' @param metadata the metadata metadata file
#'
#' @return percent of images with an RTK fix
#'
#' @examples
#' extract_precent_images_rtk(metadata)
#'
#' @export
extract_pct_images_rtk = function(metadata) {
  rtk_fix = extract_rtk_fix(metadata)

  percent_images_rtk = round((sum(rtk_fix == TRUE) / nrow(metadata)) * 100)

  return(percent_images_rtk)
}

#### white_balance_mode_and_prop_derived ####

#' Extracts the most common white balance setting (mode_derived) the proportion of images with a white balance setting that matches the most common white balance setting in the dataset (mode_prop_derived)
#'
#' A data.frame of white_balance_mode_derived and white_balance_prop_derived. White_balance_mode_derived: The most common white balance setting across all images in the project. Options: auto and manual. White_balance_mode_prop_derived: The proportion of images with a white balance setting that matches the modal white balance setting in the dataset.
#'
#' @param metadata the metadata metadata file
#'
#' @return a data.frame of the most common white balance setting and the proportion of images matching the most common white balance setting
#'
#' @examples
#' extract_white_balance_mode_and_prop(metadata)
#'
#' @export
extract_white_balance_summary = function(metadata) {
  white_balance = metadata$white_balance

  unique_white_balance <- unique(white_balance)
  white_balance_mode_derived = unique_white_balance[which.max(tabulate(match(white_balance, unique_white_balance)))][1]

  white_balance_pct_mode_derived = round((sum(white_balance == white_balance_mode_derived) / nrow(metadata)) * 100)

  ret = data.frame(white_balance_mode_derived, white_balance_pct_mode_derived)

  return(ret)
}


#### exposure_median_derived ####

#' Extracts the median exposure time and its coefficient of variation
#'
#' The median exposure time across all images in the dataset. Units: sec
#'
#' @param metadata the metadata metadata file
#'
#' @return the median exposure time across all images in the dataset. Units: sec
#'
#' @examples
#' extract_exposure(metadata)
#'
#' @export
extract_exposure_summary = function(metadata) {
  exposure = metadata$exposure

  exposure_median_derived = median(exposure)
  exposure_stdev_derived = sd(exposure)

  exposure_cv_derived = exposure_stdev_derived / exposure_median_derived
  exposure_median_derived = round(exposure_median_derived, 6)
  exposure_stdev_derived = round(exposure_stdev_derived, 6)
  exposure_cv_derived = round(exposure_cv_derived, 2)

  ret = data.frame(
    exposure_median_derived = exposure_median_derived,
    exposure_cv_derived
  )

  return(ret)
}


#### area_ha_and_image_density ####

#' Extracts the the area of the mission footprint and the image density of the dataset
#'
#' area_ha: Area of mission footprint. Units: ha. image_density: the image density computed with image count and footprint. Units: img/ha
#'
#' @param metadata the metadata metadata file
#'
#' @param mission_polygon the sf polygon object of the mission footprint
#'
#' @return the area of the mission footprint (ha) and the image density based on image count and footprint (img/ha)
#'
#' @examples
#' extract_area_and_density(metadata)
#'
#' @export
extract_area_and_density = function(metadata, mission_polygon) {
  area_ha_derived = units::set_units(sf::st_area(mission_polygon), "hectare")

  # Crop images to the mission polygon, in case there were outlier images, or smaller outlier
  # polygons of images that were removed in the mission polygon creation
  intersects = sf::st_intersects(metadata,
    mission_polygon |>
      sf::st_transform(sf::st_crs(metadata)),
    sparse = FALSE
  )
  intersects = apply(intersects, 1, any, simplify = TRUE)
  imgs_intersecting = metadata[intersects, ]

  image_density_derived = (nrow(imgs_intersecting)) / area_ha_derived

  area_derived = round(area_ha_derived, 2)
  image_density_derived = round(image_density_derived, 2)

  ret = data.frame(area_derived, image_density_derived)

  return(ret)
}


# Image frequency (imgs/sec)
#' @export
extract_image_frequency <- function(metadata) {
  # Convert DateTimeOriginal to datetime object
  datetime <- lubridate::as_datetime(metadata$datetime_local)

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

  if (mean_time_diff == 0) {
    warning("Mean image capture rate is < 0.5 seconds; cannot calculate capture rate from capture time. Assuming 0.1 sec.")
    mean_time_diff = 0.1
  }

  # Calculate image frequency (images per second) from filtered values
  mean_image_frequency <- 1 / mean_time_diff

  mean_image_frequency = round(mean_image_frequency, 2)

  return(mean_image_frequency)
}

# Resolution and aspect ratio
extract_resolution_and_aspect_ratio_summary <- function(metadata) {
  # Get Xresolution and Yresolution from the metadata data
  resolution_x <- unique(metadata$image_width)
  resolution_y <- unique(metadata$image_height)

  # Calculate mode resolution
  mode_resolution_x <- as.numeric(names(sort(table(resolution_x), decreasing = TRUE)[1]))
  mode_resolution_y <- as.numeric(names(sort(table(resolution_y), decreasing = TRUE)[1]))

  # Get aspect ratio
  # TODO instead could you directly calculate the aspect ratio of all images and then take the mode
  aspect_ratio <- round(mode_resolution_x / mode_resolution_y, 2)

  return(list(
    resolution_x_derived = mode_resolution_x,
    resolution_y_derived = mode_resolution_y,
    aspect_ratio_derived = aspect_ratio
  ))
}


# File type
extract_file_format_summary <- function(metadata) {
  # Get image file format
  # TODO fix this column
  image_file_format <- unique(metadata$file_format)

  # Check if image file format is consistent
  if (length(unique(image_file_format)) > 1) {
    # If not identical, return a warning
    warning("Image file format varies across images.")
  }

  # Calculate mode file format
  file_format <- names(sort(table(image_file_format), decreasing = TRUE)[1])

  return(file_format)
}


# Preps the metadata data for passing to the
# extraction functions, then calls all the individual extraction functions to extract the respecive attributes.
# crop_to_contiguous: Keeps only the images within the sets of contiguous images that are larger
# than min_contig_areain m^2 (could be multiple clumps). It will always include the largest clump, even if
# it is smaller than min_contig_area.
#' @export
extract_imagery_dataset_metadata = function(metadata, mission_polygon, dataset_id) {
  # Print which dataset is being processed
  message("Processing dataset ", dataset_id, "...")

  # Transform dataframe into sf object
  metadata = sf::st_as_sf(metadata, crs = 4326, coords = c("lon", "lat"))

  # If there are < 10 images left in the largest contiguoug polygon, skip
  if (nrow(metadata) < 10) {
    message(
      "Less than 10 images in the largest contiguous patch of images retained for dataset ",
      dataset_id,
      "; skipping metadata extraction."
    )
    return()
  }

  # Extract/compute metadata attributes
  flight_speed_derived = extract_flight_speed(metadata)
  flight_terrain_correlation_derived = extract_flight_terrain_correlation(metadata)
  camera_pitch = extract_camera_pitch_summary(metadata)
  dates_times = extract_dates_times(metadata)
  centroid_internal = extract_mission_centroid_sf(metadata)
  centroid_lonlat = centroid_sf_to_lonlat(centroid_internal)
  solarnoon_utc_derived = solarnoon_from_centroid_and_date(centroid_internal, dates_times$earliest_date_derived)
  image_count_derived = extract_image_count(metadata)
  file_size_derived = extract_file_size_summary(metadata)
  percent_images_rtk_derived = extract_pct_images_rtk(metadata)
  white_balance = extract_white_balance_summary(metadata)
  exposure = extract_exposure_summary(metadata)
  area_and_density = extract_area_and_density(metadata, mission_polygon)
  image_frequency_derived = extract_image_frequency(metadata)
  resolution_and_aspect_ratio = extract_resolution_and_aspect_ratio_summary(metadata)
  file_format_derived = extract_file_format_summary(metadata)

  dataset_metadata = data.frame(
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

  return(dataset_metadata)
}
