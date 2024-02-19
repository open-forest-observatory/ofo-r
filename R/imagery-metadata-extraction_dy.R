# Functions to extract metadata from a drone imagery dataset, specifically using a data frame of
# EXIF data that has already been created by extracting EXIF data from all the images. Note that the
# last function defined in this script is a function that wraps all the individual extraction
# functions to extract all the metadata attributes at once.

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
# image_merge_distance: The horizontal distance between images below which they are merged into one mission polygon
create_mission_polygon = function(exif, image_merge_distance) {

  exif = sf::st_transform(exif, 3310)
  ptbuff = sf::st_buffer(exif, image_merge_distance)
  polybuff = sf::st_union(ptbuff)
  poly = sf::st_buffer(polybuff, -image_merge_distance + 1)

  # Check if multipolygon and return warning if so
  n_polys = length(sf::st_geometry(poly)[[1]])
  if (n_polys > 1) warning("Non-contiguous images in dataset ", exif$dataset_id)

  polysimp = sf::st_simplify(poly, dTolerance = 10)

  return(polysimp)

}

# Get the correlation between the altitude of the drone and the ground elevation (i.e. trerrain
# follow tightness)
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

# Wrapper for Derek's metadata extraction functions. Preps the EXIF data for passing to the
# extraction functions, then calls all the individual extraction functions to extract the respecive attributes.
extract_metadata_dy = function(exif_filepath, plot_flightpath = FALSE) {

  # Prep the EXIF data for extraction of metadata attributes
  exif = prep_exif(exif_filepath, plot_flightpath = plot_flightpath)

  # Extract/compute metadata attributes
  flight_speed_derived = extract_flight_speed(exif)
  flight_terrain_correlation = extract_flight_terrain_correlation(exif)

  # Return extracted/computed metadata as a data frame row
  metadata = data.frame(dataset_id = exif$dataset_id[1],
                        flight_speed_derived = flight_speed_derived,
                        flight_terrain_correlation = flight_terrain_correlation
                        # Add more metadata variables here
                        )

  # Compute geospatial features
  mission_polygon = create_mission_polygon(exif, image_merge_distance = 50)

  ret = list(metadata = metadata,
             mission_polygon = mission_polygon)

  return(ret)

}

# Post-processes the data returned by extract_metadata(_dy): Combines the extracted mission polygon
# and metadata by applying the metadata as attributes of the polygon
get_mission_polygon_w_metadata = function(exif_file, image_merge_distance = 10) {

  # Get the metadata and mission polygon
  mission = extract_metadata_dy(exif_file, plot_flightpath = TRUE)

  # Add the metadata as attributes to the mission polygon
  mission_polygon = mission$mission_polygon |> sf::st_as_sf()
  mission_polygon_attributed = dplyr::bind_cols(mission_polygon, mission$metadata)
  
  # Return the attributed mission polygon
  return(mission_polygon_attributed)
}