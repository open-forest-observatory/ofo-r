# This script has two purposes: 1. Demonstrate how to run functions to extract metadata from imagery
# dataset EXIF data, and 2. Demonstrate how to write a new metadata extraction function.

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "derek-metadata-laptop.txt"))
datadir = "/ofo-share/str-disp_drone-data-partial/str-disp_drone-data_imagery-missions/Lassic/Lassic_120m"

# --- 1. Workflow for running metadata extraction ---

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
exif_filepath = exif_files[1]
plot_flightpath = TRUE
crop_to_contiguous = TRUE

# Run for that one EXIF file.
extract_metadata_dy(exif_file, plot_flightpath = TRUE)


  # Prep the EXIF data for extraction of metadata attributes
  exif = prep_exif(exif_filepath, plot_flightpath = plot_flightpath)

  # Compute geospatial features
  mission_polygon = extract_mission_polygon(exif, image_merge_distance = 50)

  if (crop_to_contiguous) {

    # Keep only the images within the largest contiguous patch of images
    polygon_proj_buffer = mission_polygon |> sf::st_transform(3310) |> sf::st_buffer(20)
    exif_proj = sf::st_transform(exif, 3310)
    intersection_idxs = sf::st_intersects(exif_proj, polygon_proj_buffer, sparse = FALSE)
    exif = exif[intersection_idxs[,1], ]
  }

  # Extract/compute metadata attributes
  flight_speed_derived = extract_flight_speed(exif)
  flight_terrain_correlation_derived = extract_flight_terrain_correlation(exif)
  flight_elev_agl_derived = extract_flight_elev_agl(exif)
  camera_pitch_pre = extract_camera_pitch(exif)
  camera_pitch_derived = camera_pitch_pre$processed_pitch
  smart_oblique_derived = camera_pitch_pre$smart_oblique
  earliest_date_internal = extract_earliest_date(exif)
  single_date_derived = extract_single_date(exif)
  centroid_internal = extract_mission_centroid_sf(exif)
  lonlat_pre = centroid_sf_to_lonlat(mission_centroid_internal)
  centroid_lon_derived = lonlat_pre$lon
  centroid_lat_derived = lonlat_pre$lat
  solarnoon_derived = solarnoon_from_centroid_and_date(centroid_internal, earliest_date_internal)
  
  
  
  
  
  
  
  
  

  # Return extracted/computed metadata as a data frame row
  metadata = data.frame(dataset_id = exif$dataset_id[1],
                        flight_speed_derived = flight_speed_derived,
                        flight_terrain_correlation = flight_terrain_correlation,
                        flight_elev_agl = flight_elev_agl
                        # Add more metadata variables here
                        )

  ret = list(metadata = metadata,
             mission_polygon = mission_polygon)
