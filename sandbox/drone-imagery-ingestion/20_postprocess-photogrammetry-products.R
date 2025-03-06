# Purpose: Take the photogrammetry products and compute the "deliverable" versions of the outputs by
# postprocessing. Perform at the mission level.

# This script requires the existince of a conda environment named `pdal` with the `pdal` package
# installed. This can be created with the system command: `conda create -n pdal -c conda-forge pdal
# -y`

library(tidyverse)
library(sf)
library(lidR)

METASHAPE_OUTPUTS_PATH = "/ofo-share/drone-imagery-processed/01/metashape-outputs/"
PHOTOGRAMMETRY_PUBLISH_PATH = "/ofo-share/drone-imagery-processed/01/photogrammetry-publish"
MISSION_FOOTPRINTS_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/all-mission-polygons-w-metadata.gpkg"

# Visualize products as they are being created
VISUALIZE = FALSE
# Skip output files already present on disk. Does not check for validity.
SKIP_EXISTING = FALSE

# Move data to the publish_path and run cropping and conversion to cloud optimized format if
# appropriate for that file type
RUN_CONVERSION = TRUE
# Create the canopy height model
RUN_CHM = TRUE
# Create the thumbnail image preview from raster data
RUN_THUMBNAIL = TRUE

# Upper/lower bounds for which dataset IDs to process
CONVERSION_LOWER_BOUND_DATASET = 1
CONVERSION_UPPER_BOUND_DATASET = 10e+6

# What fraction of the system RAM can TERRA use. The terra default is 0.6, you cannot go above 0.9
# without a warning.
TERRA_MEMFRAC = 0.9

RELEVANT_FILETYPES = c("dsm-ptcloud.tif", "dsm-mesh.tif", "dtm-ptcloud.tif", "model_local.ply", "model_georeferenced.ply", "ortho_dsm-mesh.tif", "cameras.xml", "points.laz", "log.txt")


## Functions
get_dataset_ID = function(filename) {
  return(substr(filename, start = 1, stop = 6))
}

get_timestamp = function(filename) {
  return(substr(filename, start = 8, stop = 20))
}

list_photogrammetry_outputs = function(input_folder, lower_bounds_dataset = 0, upper_bound_dataset = 1e+06) {
  # List all files that start with six digits
  files = as.vector(list.files(input_folder, "^[0-9]{6}_"))
  # Extract the dataset IDs and timestamps from these filenames
  dataset_ids = as.vector(sapply(files, FUN = get_dataset_ID))
  timestamps = as.vector(sapply(files, FUN = get_timestamp))

  # Create a dataframe for the ID and timestamp
  processed_files = data.frame(
    file = files,
    dataset_ID = dataset_ids,
    timestamp = timestamps
  )

  # Convert dataset_ids to indices and determine which ones are within the specified range of IDs
  int_dataset_ids = strtoi(dataset_ids, base = 10)
  elements_in_bounds = which(
    (int_dataset_ids >= lower_bounds_dataset) &
      (int_dataset_ids <= upper_bound_dataset)
  )
  # Only retain the rows for datasets within the specified IDs
  processed_files = processed_files[elements_in_bounds, ]

  processing_runs = processed_files |>
    dplyr::select(-file) |>
    # Remove duplicated rows
    dplyr::distinct() |>
    # Sort by the dataset ID and then timestamp
    dplyr::arrange(dataset_ID, timestamp)

  # Iterate over files. Even though the naming of the files is consistent, we don't know whether
  # all outputs will actually be present
  for (i in 1:nrow(processed_files)) {
    # Get the ID, timestamp, and final characters representing the filetype
    file_foc = processed_files[i, ]

    file_type = substr(file_foc$file, start = 22, stop = 100)


    if (!(file_type %in% RELEVANT_FILETYPES)) {
      next()
    }

    # If the file matched one of the expected types, find which processing run record it corresponds
    # to and set the appropriate field
    selected_row = which(processing_runs$dataset_ID == file_foc$dataset_ID & processing_runs$timestamp == file_foc$timestamp)

    processing_runs[selected_row, file_type] = file_foc$file
  }

  # Rename the columns such that the column names are what the output filenames should be
  processing_runs = processing_runs |>
    dplyr::rename(
      "orthomosaic.tif" = "ortho_dsm-mesh.tif", # TODO: swap in ptcloud-based ortho once generated
      "mesh-internal.ply" = "model_local.ply",
      "mesh-georeferenced.ply" = "model_georeferenced.ply",
    )
  # TODO the columns need to be renamed
  return(processing_runs)
}

convert_to_cloud_optimized = function(
    mission_footprints_path,
    photogrammetry_output_files,
    photogrammetry_files_folder,
    cloud_optimized_output_folder,
    visualize = FALSE,
    skip_existing = TRUE) {
  # Check if the pdal environment exists, and error out if not
  if (system("conda list --name pdal", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("Conda environment `pdal` not found. Please create an environmented named `pdal` with `pdal` installed")
  }


  mission_polygons = sf::st_read(mission_footprints_path)
  crs = sf::st_crs(mission_polygons)

  if (visualize) {
    X11()
  }

  # Iterate over the rows, each corresponding to a different processing run
  # This places the data in the output file tree in the appropriate format. For tif and laz data,
  # it creates a cloud optimized version and subsets to the mission polygon. For all other data,
  # it just creates a copy.
  for (i in 1:nrow(photogrammetry_output_files)) {
    # Get the row and associated timestamp and dataset_ID for the processing run
    row <- photogrammetry_output_files[i, ]
    timestamp = row[["timestamp"]]
    dataset_ID = row[["dataset_ID"]]

    # Determine the corresponding mission polygon
    mission_polygon = mission_polygons[mission_polygons$mission_id == dataset_ID, ]

    # Construct the output folder and create it if needed
    output_folder = file.path(cloud_optimized_output_folder, dataset_ID, paste0("processed-", timestamp), "full")
    if (!dir.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }

    # Iterate over the columns (i.e. files), skipping the first two columns which are the dataset_ID
    # and processing ID
    for (j in 3:ncol(row)) {
      elem = row[j]
      # The input file is the data contained in the table, the output file is the column name
      input_file = elem[[1]]
      output_file = names(elem)

      if (is.null(input_file) || is.na(input_file)) {
        next
      }

      # Compute the full input and output paths
      input_file_path = file.path(photogrammetry_files_folder, input_file)
      output_file_path = file.path(output_folder, output_file)
      if (skip_existing && file.exists(output_file_path)) {
        print(paste0("Skipping existing file: ", output_file_path))
        next
      }
      print(paste0("Converting ", input_file_path, " to ", output_file_path))

      # Compute the extension
      # Make sure to escape the period symbol so it's not treated as a wildcard
      extension = strsplit(input_file, split = "[.]")[[1]][2]

      if (extension == "tif") {
        # Process raster data
        # Read the raster
        raster = terra::rast(input_file_path)
        # Convert the polygon to the same CRS
        raster_crs = terra::crs(raster)
        mission_polygon_in_raster_crs = sf::st_transform(mission_polygon, raster_crs)
        # Crop the raster to the minimum bounding rectangle of the polygon, filling in pixels outside
        # the bounds with nan.
        cropped_raster = terra::crop(raster, mission_polygon_in_raster_crs, mask = TRUE)

        # Visualize if requested
        if (visualize) {
          terra::plot(cropped_raster)
          plot(mission_polygon_in_raster_crs["geom"], add = TRUE)
        }
        # Write out the cropped raster as a cloud-optimized geotif
        # Use bigtiff format if the size might be bigger than 4GB after compression. According to
        # the docs, this is a heuristic and may fail. An alternative is "YES" to force bigtiff
        terra::writeRaster(
          cropped_raster,
          output_file_path,
          overwrite = TRUE,
          filetype = "COG",
          gdal = "BIGTIFF=IF_SAFER"
        )
      } else if (extension == "laz") {
        # Process pointcloud data
        # Read the pointcloud
        time = system.time(point_cloud <- lidR::readLAS(input_file_path))
        print(paste0("Point cloud loading took ", time[[3]]))
        point_cloud_crs = sf::st_crs(point_cloud)
        mission_polygon_in_point_cloud_crs = sf::st_transform(mission_polygon, point_cloud_crs)

        time = system.time(cropped_point_cloud <- lidR::clip_roi(point_cloud, mission_polygon_in_point_cloud_crs))
        print(paste0("Cropping point cloud took ", time[[3]]))
        cropped_file = tempfile(fileext = ".laz")
        time = system.time(lidR::writeLAS(cropped_point_cloud, cropped_file))
        print(paste0("Saving point cloud took ", time[[3]]))

        pdal_command = paste0("pdal translate ", cropped_file, " ", output_file_path, " -w writers.copc")
        conda_command = paste0("conda run -n pdal ", pdal_command)
        time = system.time(system(conda_command))
        print(paste0("Converting to COPC took ", time[[3]]))

        file.remove(cropped_file)
      } else {
        # TODO subset the mesh data spatially instead of just copying
        file.copy(input_file_path, output_file_path)
      }

      if (!file.exists(output_file_path)) {
        stop(paste0("Failed to create ", output_file_path))
      }
    }
  }
}

create_chms = function(exported_data_folder, res = 0.25, skip_existing = TRUE) {
  full_folders = as.vector(
    list.files(
      exported_data_folder,
      pattern = "^full", recursive = TRUE, include.dirs = TRUE, full.names = TRUE
    )
  )

  for (i in seq_along(full_folders)) {
    full_folder = full_folders[i]
    dsm_files = as.vector(
      list.files(
        full_folder,
        pattern = "dsm.*tif", recursive = TRUE, include.dirs = TRUE, full.names = TRUE
      )
    )
    dtm_files = as.vector(
      list.files(
        full_folder,
        pattern = "dtm.*tif", recursive = TRUE, include.dirs = TRUE, full.names = TRUE
      )
    )
    if (length(dtm_files) == 0) {
      next()
    } else if (length(dtm_files) > 1) {
      stop(paste0("Expected only one DTM file but found: ", str(dtm_files)))
    }
    # Take the only DSM file
    dtm_file = dtm_files[1]

    for (j in seq_along(dsm_files)) {
      dsm_file = dsm_files[j]
      output_chm_file = str_replace(dsm_file, "dsm", "chm")
      if (!(skip_existing && file.exists(output_chm_file))) {
        dsm = terra::rast(dsm_file)
        dtm = terra::rast(dtm_file)
        chm = ofo::chm_from_coregistered_dsm_dtm(dsm, dtm, res = res)
        terra::writeRaster(chm, output_chm_file, filetype = "COG", overwrite = TRUE)
        print(paste0(
          "Creating ",
          output_chm_file,
          " from ",
          dsm_file,
          " and ", dtm_file
        ))
      }
    }
  }
}

generate_thumbnails = function(exported_data_folder, output_max_dim = 512, skip_existing = TRUE) {
  # Get a list of folders for each processed dataset and sort them
  processing_folders = as.vector(
    list.files(
      exported_data_folder,
      pattern = "^processed-", recursive = TRUE, include.dirs = TRUE, full.names = TRUE
    )
  )
  processing_folders = sort(processing_folders)

  # Iterate over the folders for each processing run
  for (folder in processing_folders) {
    # Append the 'full' subfolder which is where the full-resolution assets are and the 'thumbnails'
    # folder which thumbnails will be placed into
    full_folder = file.path(folder, "full")
    output_folder = file.path(folder, "thumbnails")
    # List all tifs within the output folder
    tif_files = as.vector(list.files(full_folder, "*.tif", full.names = FALSE))

    # Create the output directory if it doesn't exist
    if (!dir.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }

    # For each full-resolution tif file
    for (tif_file in tif_files) {
      # Full path to the tif file
      tif_file_path = file.path(full_folder, tif_file)
      # Create the output file in the thumbnails folder with the same name but png extension
      output_file = str_replace(file.path(output_folder, tif_file), "tif$", "png")

      if ((skip_existing && file.exists(output_file))) {
        print(paste0("Skipping creation of existing thumbnail: ", output_file))
        next()
      }

      print(paste0("Creating thumbnail: ", output_file))

      # Read the raster
      raster = terra::rast(tif_file_path)
      # Compute the number of rows and columns
      n_row = terra::nrow(raster)
      n_col = terra::ncol(raster)
      # Compute the maximum dimension and determine the scale factor to make it match the specified
      # maximum size
      max_dim = max(n_row, n_col)
      scale_factor = output_max_dim / max_dim
      new_n_row = floor(n_row * scale_factor)
      new_n_col = floor(n_col * scale_factor)

      # Specify a PNG file as the output device
      # Make sure the background is transperant
      png(output_file, width = new_n_col, height = new_n_row, bg = "transparent")

      # Determine whether this is scalar or RGB data
      n_lyr = terra::nlyr(raster)
      if (n_lyr == 1) {
        # The mar argument ensures that there is not an excessive white border around the image
        plot(raster, axes = FALSE, legend = FALSE, mar = c(0, 0, 0, 0)) # , bg = "transparent")
      } else if (n_lyr %in% c(3, 4)) {
        # Make sure the background is transperant
        terra::plotRGB(raster, bgalpha = 0)
      } else {
        stop(paste0("Input data had an unexpected number of layers: ", str(n_lyr)))
      }

      # Close the PNG
      dev.off()
    }
  }
}


## Workflow
terra::terraOptions(memfrac = TERRA_MEMFRAC)

if (RUN_CONVERSION) {
  # Get all the output files from photogrammetry
  photogrammetry_outputs = list_photogrammetry_outputs(
    METASHAPE_OUTPUTS_PATH,
    CONVERSION_LOWER_BOUND_DATASET,
    CONVERSION_UPPER_BOUND_DATASET
  )
  # Convert data to cloud optimized format
  convert_to_cloud_optimized(
    MISSION_FOOTPRINTS_PATH,
    photogrammetry_outputs,
    METASHAPE_OUTPUTS_PATH,
    PHOTOGRAMMETRY_PUBLISH_PATH,
    VISUALIZE,
    skip_existing = SKIP_EXISTING
  )
}
if (RUN_CHM) {
  # Create the CHM layers for each file pair that has been copied to the output folder
  create_chms(PHOTOGRAMMETRY_PUBLISH_PATH, skip_existing = SKIP_EXISTING)
}
if (RUN_THUMBNAIL) {
  # Generate the thumbnails for each of the produced files
  generate_thumbnails(PHOTOGRAMMETRY_PUBLISH_PATH, skip_existing = SKIP_EXISTING)
}
