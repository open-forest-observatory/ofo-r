# Purpose: Create the Hugo markdown pages for all drone imagery datasets, as well as a dataset index
# page

library(dplyr)
library(leaflet)
library(stringr)
library(sf)
library(htmlwidgets)
library(DT)
library(jinjar)

devtools::load_all()


BASE_OFO_URL = "https://openforestobservatory.org/"
#BASE_OFO_URL = "http://localhost:1313/"
WEBSITE_REPO_PATH = "/ofo-share/repos-derek/ofo-website-3"

# Path to the plot details template page within theis repo
MISSION_DETAILS_TEMPLATE_FILEPATH = fs::path(file.path("sandbox", "drone-imagery-curation", "templates", "drone-mission-curation-details.md"))

# Path to plot details dir relative to the 'content' dir in the website repo. No leading slash but
# trailing slash
MISSION_DETAILS_PAGE_DIR = "data/drone/curation/mission-details/"

# Path to static website files dirs within website repo relative to 'static'. Leading slash but no
# trailing slash
DATATABLE_HEADER_FILES_DIR = "/datatable-header-files_curation"
LEAFLET_HEADER_FILES_DIR = "/leaflet-header-files_curation"
MISSION_CATALOG_DATATABLE_DIR = "/curation-mission-catalog-datatable/"
MISSION_CATALOG_DATATABLE_FILENAME = "curation-mission-catalog-datatable.html"
MISSION_CATALOG_MAP_DIR = "/curation-mission-catalog-map/"
MISSION_CATALOG_MAP_FILENAME = "curation-mission-catalog-map.html"
MISSION_DETAILS_DATATABLE_DIR = "/curation-mission-details-datatables"
MISSION_DETAILS_MAP_DIR = "/curation-mission-details-maps"

WEBSITE_STATIC_PATH = file.path(WEBSITE_REPO_PATH, "static", "")
WEBSITE_CONTENT_PATH = file.path(WEBSITE_REPO_PATH, "content", "")

# Data paths
MISSION_POLYGONS_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/all-mission-polygons-w-metadata.gpkg"
MISSION_POINTS_PATH = "/ofo-share/drone-imagery-organization/3c_metadata-extracted/all-mission-points-w-metadata.gpkg"


# ---- Processing

# Load and prep metadata
mission_polygons_w_metadata = st_read(MISSION_POLYGONS_PATH)
mission_points = st_read(MISSION_POINTS_PATH)

# Save header library files required by embedded HTML datatables and leaflet maps
save_dt_header_files(WEBSITE_STATIC_PATH, DATATABLE_HEADER_FILES_DIR)
save_leaflet_header_files(WEBSITE_STATIC_PATH, LEAFLET_HEADER_FILES_DIR)
# Compile relevant and human-readable values from mission attributes as additional columns of the
# mission polygons object
mission_polygons_w_summary_data = compile_mission_summary_data(mission_polygons_w_metadata,
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

# For website directories that house mission-level page components, delete existing directories and
# create new empty directory
reset_detail_dirs(
  WEBSITE_STATIC_PATH,
  WEBSITE_CONTENT_PATH,
  MISSION_DETAILS_PAGE_DIR,
  MISSION_DETAILS_MAP_DIR,
  MISSION_DETAILS_DATATABLE_DIR
)

## Loop through each mission and make a details page, including its media (map and datatable)
make_mission_details_pages(
  mission_summary = mission_polygons_w_summary_data,
  website_static_path = WEBSITE_STATIC_PATH,
  website_content_path = WEBSITE_CONTENT_PATH,
  leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
  datatable_header_files_dir = DATATABLE_HEADER_FILES_DIR,
  mission_details_datatable_dir = MISSION_DETAILS_DATATABLE_DIR,
  mission_details_map_dir = MISSION_DETAILS_MAP_DIR,
  mission_details_template_filepath = MISSION_DETAILS_TEMPLATE_FILEPATH,
  mission_details_page_dir = MISSION_DETAILS_PAGE_DIR
)
