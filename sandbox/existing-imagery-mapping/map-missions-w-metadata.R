# Take EXIF data and extract metadata (e.g. altitude, gimbal pitch) and mission polygon and write to
# gpkg

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()
library(terra)

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "derek-map-imagery-js.txt"))


# --- 1. Workflow for running metadata extraction ---

# Get a list of the files containing the EXIF data (one file per image dataset).
exif_files = list.files(file.path(datadir, "extracted-exif"), pattern = "^exif.+\\.csv$", recursive = TRUE, full.names = TRUE)


future::plan(future::multicore)
polys = furrr::future_map(exif_files, get_mission_polygon_w_metadata, .options = furrr::furrr_options(chunk_size = 1, seed = TRUE))
future::plan(sequential)

polys_df = dplyr::bind_rows(polys)
polys = polys_df


# If the dataset ID includes the string '_and_', split it into two separate datasets on that string
polys = polys |>
  tidyr::separate_longer_delim(dataset_id, delim = "_and_")



# Write the tabular data as CSV for a human to add flight altitude and camera pitch info
polys_nonspatial = sf::st_drop_geometry(polys)
polys_nonspatial$altitude_annotated = NA
polys_nonspatial$pitch_annotated = NA
readr::write_csv(polys_nonspatial, file.path(datadir, "ancillary", "altitude-pitch-annotation", "missions-for-annotation.csv"))

# Pull in the annotated data and add it to the spatial data
annotated = readr::read_csv(file.path(datadir, "ancillary", "altitude-pitch-annotation", "missions-for-annotation-annotated.csv"))
annotated = annotated |> dplyr::select(dataset_id, altitude_annotated, pitch_annotated)
polys = dplyr::left_join(polys, annotated, by = "dataset_id")

# Pull in the baserow data and add it to the spatial data
baserow = readr::read_csv(file.path(datadir, "ancillary", "baserow-snapshots", "export - datasets-imagery.csv"))
baserow = baserow |> dplyr::select(dataset_id, altitude_agl_nominal, altitude_ahp_obs, camera_pitch_nominal)
polys = dplyr::left_join(polys, baserow, by = "dataset_id")

# Create a definitive altitude and pitch column, based on the following priority:
# for pitch: altitude_ahp_obs, altitude_agl_nominal, altitude_annotated
# for altitude: camera_pitch_nominal, pitch_annotated
polys = polys |>
  dplyr::mutate(altitude = dplyr::coalesce(altitude_ahp_obs, altitude_agl_nominal, altitude_annotated),
                pitch = dplyr::coalesce(camera_pitch_nominal, pitch_annotated))

# Save to gpkg
sf::st_write(polys, file.path(datadir, "ancillary", "dataset-polys", "dataset-polys_v1.gpkg"), delete_dsn = TRUE)
