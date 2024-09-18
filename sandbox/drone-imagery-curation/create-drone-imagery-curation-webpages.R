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
MISSION_DETAILS_TEMPLATE_FILEPATH = fs::path(file.path("sandbox", "drone-imagery-curation", "templates", "drone-mission-details.md"))

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
mission_polygons_w_summary_data = compile_mission_summary_table(mission_polygons_w_metadata,
                                                base_ofo_url = BASE_OFO_URL,
                                                mission_details_dir = MISSION_DETAILS_PAGE_DIR)

# Make a HTML data table of plot catalog
dt = make_mission_catalog_datatable(mission_summary = mission_polygons_w_summary_data,
                            website_static_path = WEBSITE_STATIC_PATH,
                            datatable_header_files_dir = DATATABLE_HEADER_FILES_DIR,
                            mission_catalog_datatable_dir = MISSION_CATALOG_DATATABLE_DIR,
                            mission_catalog_datatable_filename = MISSION_CATALOG_DATATABLE_FILENAME)



# Make leaflet map of field data catalog
m = make_mission_catalog_map(mission_summary = mission_polygons_w_summary_data,
                      website_static_path = WEBSITE_STATIC_PATH,
                      leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
                      mission_catalog_map_dir = MISSION_CATALOG_MAP_DIR,
                      mission_catalog_map_filename = MISSION_CATALOG_MAP_FILENAME)



# For website directories that house mission-level page components, delete existing directories and
# create new empty directory
reset_detail_dirs(WEBSITE_STATIC_PATH,
                       WEBSITE_CONTENT_PATH,
                       MISSION_DETAILS_PAGE_DIR,
                       MISSION_DETAILS_MAP_DIR,
                       MISSION_DETAILS_DATATABLE_DIR)

## For each mission, create a page with a map and a table of mission data





sub_mission_from_image_id = function(image_id) {
  mission_w_sub_mission = str_split(image_id, fixed("_")) |> purrr::map(1) |> unlist()
  sub_mission = str_split(mission_w_sub_mission, fixed("-")) |> purrr::map(2) |> unlist()
  return(sub_mission)
}

#### Make mission-level leaflet stem map

make_mission_details_map = function(mission_polygons_w_summary_data,
                                    mission_points,
                                    website_static_path,
                                    leaflet_header_files_dir,
                                    mission_details_map_dir) {

  # Get the mission boundary, flightpath, photo points w attributes

  mission_polygons_w_summary_data

  mission_ids = mission_polygons_w_summary_data$dataset_id

  mission_id = mission_ids[1]

  mission_polygon_foc = mission_polygons_w_metadata |>
    filter(dataset_id == !!mission_id)

  mission_points_foc = mission_points |>
    rename(dataset_id = dataset_id_image_level) |>
    filter(dataset_id == !!mission_id) |>
    # Get which sub-mission they're from
    mutate(sub_mission = sub_mission_from_image_id(image_id)) |>
    # Create a column "seconds from mission start" for legend coloring
    mutate(time_secs = as.numeric(datetime_local))

  initial_time = min(mission_points_foc$time_secs)

  mission_points_foc = mission_points_foc |>
    mutate(hours_elapsed = (time_secs - initial_time)/60/60) |>
    # Create a popup text
    mutate(popup = paste0("<b>Image ID: </b>", image_id, "<br>",
                          "<b>Altitude ASL: </b>", altitude_asl_drone, " m<br>",
                          "<b>Camera pitch: </b>", camera_pitch, " deg<br>",
                          "<b>RTK fix: </b>", rtk_fix, "<br>",
                          "<b>Capture datetime: </b>", datetime_local, "<br>",
                          "<b>Hours elapsed: </b>", round(hours_elapsed, 2)))

  # Compute flightpath
  flightpath = mission_points_foc |>
    summarize(do_union = FALSE) |>
    st_cast("LINESTRING")

  # Make leaflet map

  js_for_legend = function(x) {
    htmlwidgets::onRender(x, "
      function(el, x) {
        var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1).replace(/[^a-zA-Z]+/g, '');
          document.querySelectorAll('.legend').forEach( a => a.hidden=true );
          document.querySelectorAll('.legend').forEach( l => { if (l.classList.contains(selectedGroup)) l.hidden=false; } );
        };
        updateLegend();
        this.on('baselayerchange', el => updateLegend());
        }"
    )
  }

  pal_hourselapsed = colorNumeric("viridis", domain = mission_points_foc$hours_elapsed)
  pal_asl = colorNumeric("viridis", domain = mission_points_foc$altitude_asl_drone)
  pal_pitch = colorNumeric("viridis", domain = mission_points_foc$camera_pitch)
  pal_rtk = colorFactor("viridis", domain = mission_points_foc$rtk)
  pal_sub_mission = colorFactor("viridis", domain = mission_points_foc$sub_mission)

  m = leaflet() |>
    addPolygons(data = mission_polygon_foc, group = "bounds",
                fillOpacity = 0) |>
    addProviderTiles(providers$Esri.WorldTopo, group = "Topo",
                    options = providerTileOptions(minZoom = 1, maxZoom = 20)) |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery",
                    options = providerTileOptions(minZoom = 1, maxZoom = 20)) |>
    addLayersControl(overlayGroups = c("Imagery", "Topo"),
                    baseGroups = c("Hours elapsed", "Altitude ASL (m)", "Camera pitch (deg)", "RTK fix", "Sub-mission"),
                    options = layersControlOptions(collapsed = FALSE)) |>
    # Flight lines
    addPolylines(data = flightpath, color = "black", weight = 1, group = "Flight path") |>
    # Altitude ASL
    addCircleMarkers(data = mission_points_foc,
                    radius = 3,
                      stroke = FALSE,
                      fillOpacity = 1,
                    #  popup = ~popup,
                      color = pal_asl(mission_points_foc$altitude_asl_drone),
                      group = "Altitude ASL (m)") |>
    addLegend(pal = pal_asl,
              values = mission_points_foc$altitude_asl_drone,
              title = "Altitude ASL (m)", opacity = 1,
              group = "AltitudeASLm",
              className = "info legend AltitudeASLm") |>
    # Camera pitch
    addCircleMarkers(data = mission_points_foc,
                    radius = 3,
                      stroke = FALSE,
                      fillOpacity = 1,
                    #  popup = ~popup,
                      color = pal_pitch(mission_points_foc$camera_pitch),
                      group = "Camera pitch (deg)") |>
    addLegend(pal = pal_pitch,
              values = mission_points_foc$camera_pitch,
              title = "Camera pitch (deg)", opacity = 1,
              group = "Camerapitchdeg",
              className = "info legend Camerapitchdeg") |>
    # RTK
    addCircleMarkers(data = mission_points_foc,
                    radius = 3,
                      stroke = FALSE,
                      fillOpacity = 1,
                    #  popup = ~popup,
                      color = pal_rtk(mission_points_foc$rtk),
                      group = "RTK fix") |>
    addLegend(pal = pal_rtk,
              values = mission_points_foc$rtk,
              title = "RTK fix", opacity = 1,
              group = "RTKfix",
              className = "info legend RTKfix") |>
    # Hours elapsed
    addCircleMarkers(data = mission_points_foc,
                    radius = 3,
                      stroke = FALSE,
                      fillOpacity = 1,
                    #  popup = ~popup,
                      color = pal_hourselapsed(mission_points_foc$hours_elapsed),
                      group = "Hours elapsed") |>
    addLegend(pal = pal_hourselapsed,
              values = mission_points_foc$hours_elapsed,
              title = "Hours elapsed", opacity = 1,
              group = "Hourselapsed",
              className = "info legend Hourselapsed") |>
    # Sub-mission
    addCircleMarkers(data = mission_points_foc,
                    radius = 3,
                      stroke = FALSE,
                      fillOpacity = 1,
                    #  popup = ~popup,
                      color = pal_sub_mission(mission_points_foc$sub_mission),
                      group = "Sub-mission") |>
    addLegend(pal = pal_sub_mission,
              values = mission_points_foc$sub_mission,
              title = "Sub-mission", opacity = 1,
              group = "Submission",
              className = "info legend Submission") |>
    # Invisible markers on top of all for popup
    addCircleMarkers(data = mission_points_foc,
                    radius = 10,
                    stroke = FALSE,
                    fillOpacity = 0,
                    # popup = ~popup,
                    group = "dummyforpopup") |>
    hideGroup("Imagery") |>
    hideGroup("Topo") |>
    #groupOptions("bounds", zoomLevels = 13:20) |>
    js_for_legend()

  # Customize background color (can also use this to make transparent)
  backg <- htmltools::tags$style(".leaflet-container { background: rgba(200,200,200,1) }")
  m = prependContent(m, backg)
  print(m)

  # -- Save map HTML to website repo
  mission_details_map_filename = paste0(mission_id, ".html")
  save_widget_html(m,
                    website_static_path = website_static_path,
                    header_files_dir = leaflet_header_files_dir,
                    html_dir = mission_details_map_dir,
                    html_filename = mission_details_map_filename)

  # Record where it was saved to
  map_html_path = paste(mission_details_map_dir, mission_details_map_filename, sep = "/")

  return(map_html_path)

}


make_mission_details_map(mission_polygons_w_summary_data,
                         mission_points,
                         WEBSITE_STATIC_PATH,
                         LEAFLET_HEADER_FILES_DIR,
                         MISSION_DETAILS_MAP_DIR)
