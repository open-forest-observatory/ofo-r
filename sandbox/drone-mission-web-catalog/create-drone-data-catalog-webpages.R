# Purpose: Create the Hugo markdown pages for all drone mission datasets including
# visualizations/links to the actual data on CyVerse, as well as a dataset index
# page

library(dplyr)
library(leaflet)
library(stringr)
library(sf)
library(htmlwidgets)
library(DT)
library(jinjar)
library(readr)
library(furrr)

devtools::load_all()


BASE_OFO_URL = "https://openforestobservatory.org/"
# BASE_OFO_URL = "http://localhost:1313/"
WEBSITE_REPO_PATH = "/ofo-share/repos-derek/ofo-website-3"

# Path to the plot details template page within this repo
MISSION_DETAILS_TEMPLATE_FILEPATH = fs::path(file.path("sandbox", "drone-mission-web-catalog", "templates", "drone-mission-details.md"))

# Path to plot details dir relative to the 'content' dir in the website repo. No leading slash but
# trailing slash
MISSION_DETAILS_PAGE_DIR = "data/drone/mission-details/"

# Path to static website files dirs within website repo relative to 'static'. Leading slash but no
# trailing slash
DATATABLE_HEADER_FILES_DIR = "/datatable-header-files_drone-mission-catalog"
LEAFLET_HEADER_FILES_DIR = "/leaflet-header-files_drone-mission-catalog"
MISSION_CATALOG_DATATABLE_DIR = "/drone-mission-catalog-datatable/"
MISSION_CATALOG_DATATABLE_FILENAME = "drone-mission-catalog-datatable.html"
MISSION_CATALOG_MAP_DIR = "/drone-mission-catalog-map/"
MISSION_CATALOG_MAP_FILENAME = "drone-mission-catalog-map.html"
MISSION_DETAILS_DATATABLE_DIR = "/drone-mission-details-datatables"
MISSION_DETAILS_MAP_DIR = "/drone-mission-details-maps"
ITD_MAP_DIR = "/itd-maps"

WEBSITE_STATIC_PATH = file.path(WEBSITE_REPO_PATH, "static", "")
WEBSITE_CONTENT_PATH = file.path(WEBSITE_REPO_PATH, "content", "")

# Data paths
MISSION_POLYGONS_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/all-mission-polygons-w-metadata.gpkg"
MISSION_POINTS_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/all-points-w-metadata.gpkg"

# # The path to all the published files for the drone mission catalog
# PUBLISHED_DATA_PATH = "/ofo-share/drone-data-publish/01/"

MISSION_METADATA_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/"

# The base URL for the data server (Cyverse Data Store)
DATA_SERVER_BASE_URL = "https://data.cyverse.org/dav-anon/iplant/projects/ofo/public/missions/"

IMAGERY_PROJECT_NAME = "2023-ny-ofo"


# ---- Processing

## Get all mission geospatial polygons from CyVerse
footprint_paths = cyverse_list_files("/iplant/home/shared/ofo/public/missions/%/footprint", "footprint.gpkg")
footprint_urls = cyverse_url(footprint_paths)

## Download and merge them all
plan(multisession)
footprint_list = furrr::future_map(footprint_urls, sf_from_url, .options = furrr::furrr_options(seed = TRUE))
footprints = bind_rows(footprint_list)
plan(sequential)

# Add a mission_id column
# Look up what the mission ID is for each polygon based on the file name
mission_id_part = str_extract(footprint_paths, "missions/([0-9]{6})/footprint")
mission_id = str_extract(mission_id_part, "[0-9]{6}")
footprints$mission_id = mission_id

## Pull in all metadata from CyVerse ## Is this needed? Looks like mission footprints are already attributed with metadata
metadata_paths = cyverse_list_files("/iplant/home/shared/ofo/public/metadata/by-imagery-project", "mission-full-metadata_%.csv")
metadata_urls = cyverse_url(metadata_paths)

## Download and merge all metadata files
plan(multisession)
metadata_list = furrr::future_map(metadata_urls, df_from_url, .options = furrr::furrr_options(seed = TRUE))
metadata = bind_rows(metadata_list)
plan(sequential)

# Attribute the polygons with the mission metadata
mission_polygons_w_metadata = inner_join(footprints, metadata, by = "mission_id")

# Add a dataset_id column to match the expected format for web catalog creation functions
mission_polygons_w_metadata$dataset_id = mission_polygons_w_metadata$mission_id




# # Load and prep metadata
# mission_polygons_w_metadata = st_read(MISSION_POLYGONS_PATH)
# mission_points = st_read(MISSION_POINTS_PATH)

# # Add dataset_id field to match expected format (for mission_polygons this is done in
# # compile_mission_summary_data)
# mission_points$dataset_id = mission_points$mission_id

# Save header library files required by embedded HTML datatables and leaflet maps
save_dt_header_files(WEBSITE_STATIC_PATH, DATATABLE_HEADER_FILES_DIR)
save_leaflet_header_files(WEBSITE_STATIC_PATH, LEAFLET_HEADER_FILES_DIR)

# # ** Unique to drone mission catalog beta: keep only nadir missions (or keep only missions with photogrammetry products, etc)
# mission_polygons_w_metadata = mission_polygons_w_metadata |>
#   filter(abs(camera_pitch_derived) < 10)


# Compile relevant and human-readable values from mission attributes as additional columns of the
# mission polygons object
mission_polygons_w_summary_data = compile_mission_summary_data(
  mission_level_metadata = mission_polygons_w_metadata,
  base_ofo_url = BASE_OFO_URL,
  mission_details_dir = MISSION_DETAILS_PAGE_DIR
)

# Make a HTML data table of plot catalog
dt = make_mission_catalog_datatable(
  mission_summary = mission_polygons_w_summary_data,
  website_static_path = WEBSITE_STATIC_PATH,
  datatable_header_files_dir = DATATABLE_HEADER_FILES_DIR,
  mission_catalog_datatable_dir = MISSION_CATALOG_DATATABLE_DIR,
  mission_catalog_datatable_filename = MISSION_CATALOG_DATATABLE_FILENAME
)

# Make leaflet map of field data catalog
m = make_mission_catalog_map(
  mission_summary = mission_polygons_w_summary_data,
  website_static_path = WEBSITE_STATIC_PATH,
  leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
  mission_catalog_map_dir = MISSION_CATALOG_MAP_DIR,
  mission_catalog_map_filename = MISSION_CATALOG_MAP_FILENAME
)


############################# Detail pages






######## TODO: in the commented block here, is the framework for filtering to only focal missions.
# But it is not possible to run for some missions only, beause we need to know all missions in order
# to creat the "next" and "previous" links on the detail pages.
# # Get a list of the focal mission IDs to create pages for, based on project IDs. An alternative here
# # is to use the Baserow project IDs and filter the polygons (from CyVerse) by those IDs.
# mission_metadata = read_csv(file.path(MISSION_METADATA_PATH, paste0("mission-full-metadata_",
# IMAGERY_PROJECT_NAME, ".csv"))) mission_ids = mission_metadata$mission_id
# # Filter the mission polygons to the focal polygons to create pages for
# mission_polygons_w_summary_data_focal = mission_polygons_w_summary_data |>
#   filter(mission_id %in% mission_ids)

# TODO: Was this block necessary? Commented out so we run partial new page updates. It should be fine
# to overwrite existing mission detail page components, don't need to delete them first, right?
# # For website directories that house mission-level page components, delete existing directories and
# # create new empty directory
# reset_detail_dirs(
#   WEBSITE_STATIC_PATH,
#   WEBSITE_CONTENT_PATH,
#   MISSION_DETAILS_PAGE_DIR,
#   MISSION_DETAILS_MAP_DIR,
#   ITD_MAP_DIR,
#   MISSION_DETAILS_DATATABLE_DIR
# )

## Loop through each mission and make a details page, including its media (map and datatable)
make_mission_details_pages(
  mission_summary = mission_polygons_w_summary_data,
  mission_points = mission_points,
  website_static_path = WEBSITE_STATIC_PATH,
  website_content_path = WEBSITE_CONTENT_PATH,
  leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
  datatable_header_files_dir = DATATABLE_HEADER_FILES_DIR,
  mission_details_datatable_dir = MISSION_DETAILS_DATATABLE_DIR,
  mission_details_map_dir = MISSION_DETAILS_MAP_DIR,
  itd_map_dir = ITD_MAP_DIR,
  mission_details_template_filepath = MISSION_DETAILS_TEMPLATE_FILEPATH,
  mission_details_page_dir = MISSION_DETAILS_PAGE_DIR,
  # TODO: remove: published_data_path = PUBLISHED_DATA_PATH,
  data_server_base_url = DATA_SERVER_BASE_URL
)
