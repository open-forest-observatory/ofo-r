# Turn a data store path into a public HTTP url
cyverse_url = function(data_store_path) {
  # data_store_path is the path to a file in the CyVerse Data Store, e.g. "/iplant/home/shared/ofo/public/missions/000001/processed_000001-0001/full/chm-mesh.tif"
  # Returns a public HTTP URL to the file, e.g. "https://data.cyverse.org/dav-anon/iplant/home/shared/ofo/public/missions/000001/processed_000001-0001/full/chm-mesh.tif"

  base_url = "https://data.cyverse.org/dav-anon/"
  url = paste0(base_url, data_store_path)
  return(url)
}

# Query cyverse data store for a list of files matching a provided directory pattern and file
# pattern (with % as wildcard)
cyverse_list_files = function(dir_pattern, file_pattern) {
  # dir_pattern is the directory pattern to search for, e.g. "/iplant/projects/ofo/public/missions/%/processed_%/full"
  # file_pattern is the file pattern to search for, e.g. "chm-mesh.tif"
  call = paste0("iquest --no-page '%s/%s' \"select COLL_NAME, DATA_NAME where COLL_NAME like '", dir_pattern, "' and DATA_NAME like '", file_pattern, "'\"")
  output = system(call, intern = TRUE)
  return(output)
}

cyverse_list_dirs_recursive = function(dir_pattern) {
  call = paste0("iquest --no-page '%s' \"select COLL_NAME where COLL_NAME like '", dir_pattern, "'\"")
  output = system(call, intern = TRUE)
  return(output)
}

cyverse_list_subdirs = function(dir) {
  call = paste0("ils ", dir, "")
  output = system(call, intern = TRUE)

  # Remove the first line which is the header
  output = output[-1]

  # Remove the leading and trailing whitespace
  output = str_trim(output)

  # Remove the leading "C- "
  output = str_replace(output, "^C- ", "")

  return(output)
}

df_from_url = function(url) {
  temp_file = tempfile(fileext = ".csv")
  download.file(url, temp_file, quiet = TRUE, method = "wget")
  read_csv(temp_file, col_types = cols(.default = "c"))
}

sf_from_url = function(url) {
  temp_file = tempfile(fileext = ".gpkg")
  download.file(url, temp_file, quiet = TRUE, method = "wget")
  sf = st_read(temp_file, quiet = TRUE)
  return(sf)
}

# Pull in all attributes to the plot summary table, compute relevant columns for display, (like
# plot area in ha instead of m2), round numeric columns, create links to project and dataset pages
compile_mission_summary_data = function(mission_level_metadata, base_ofo_url, mission_details_dir, dataset_type = "mission") {
  # dataset_type can be "mission" or "sub-mission"

  # TODO: The dataset_id determination is flexible to either mission or sub-mission, but there is
  # other hardcoded references to mission that would need to be updated if sub-mission is a
  # possibility

  if (dataset_type == "mission") {
    dataset_id_column = "mission_id"
  } else if (dataset_type == "sub-mission") {
    dataset_id_column = "sub_mission_id"
  } else {
    stop("dataset_type must be either 'mission' or 'sub-mission'")
  }

  mission_level_metadata$dataset_id = pull(mission_level_metadata, dataset_id_column)

  # Pre-process the display text for the relevant attributes, based on the metadata in the database
  d = mission_level_metadata |>
    dplyr::mutate(
      overlap_combined_nominal = paste(overlap_front_nominal, overlap_side_nominal, sep = "/"),
      dataset_id_link = paste0('<a href="', base_ofo_url, mission_details_dir, dataset_id, '/"', ' target="_PARENT">', dataset_id, "</a>"),
      time_range_local_derived = paste0(earliest_time_local_derived, " to ", latest_time_local_derived),
      overlap_front_side_nominal = paste0(overlap_front_nominal, "/", overlap_side_nominal),
      image_dimensions_derived = paste0(resolution_x_derived, " x ", resolution_y_derived),
      altitude_agl_mean_derived = NA,
      embargoed = FALSE, # Dummy, need to remove if add this attribute to the database
      display_message = NA) # Dummy, need to remove if add this attribute to the database
  
  # Image count is currently a list of sub-mission image counts. Turn it into a total with
  # sub-counts in the format: "total (sub1 + sub2 + sub3 + ...)"
  image_counts_sep = str_split(d$n_images, fixed(", "))
  totals = sapply(image_counts_sep, function(x) sum(as.numeric(x)))

  make_sub_count = function(x) {
    if (length(x) > 1) {
      x = paste0("(", paste(x, collapse = " + "), ")")
      return(x)
    } else {
      return("")
    }
  }

  sub_counts = sapply(image_counts_sep, make_sub_count)

  d$image_count_w_subtotals = paste(totals, sub_counts)

  # remove trailing whitespace
  d$image_count_w_subtotals = gsub("\\s+$", "", d$image_count_w_subtotals)

  return(d)

}



make_mission_catalog_datatable = function(mission_summary,
                                       website_static_path,
                                       datatable_header_files_dir,
                                       mission_catalog_datatable_dir,
                                       mission_catalog_datatable_filename) {

  d = mission_summary |>
    sf::st_drop_geometry() |>
    select("ID" = dataset_id_link,
           "Area (ha)" = area_derived,
           "Date" = earliest_date_derived,
           "Altitude (m) (N)" = altitude_agl_nominal,
           "Overlap (N)" = overlap_combined_nominal,
           "Camera pitch" = camera_pitch_derived,
           "Terrain follow (N)" = terrain_follow,
           "Flight pattern" = flight_pattern,
           "Image count" = n_images,
           "RTK (N)" = rtk_nominal,
           "RTK images (%)" = percent_images_rtk_derived,
           "Contributor dataset name" = contributor_dataset_name,
           "Project" = project_id,
           "Aircraft" = aircraft_model_name,
           dataset_id) |>
    arrange(dataset_id) |>
    select(-dataset_id)

  # Prep formatting code to pass to datatable creation
  format_js = DT::JS("function(settings, json) {",
                     "$('body').css({'font-family': 'Arial'});",
                     "}")

  dt = DT::datatable(d,
                     escape = FALSE,
                     options = list(paging = FALSE, initComplete = format_js))

  # Save the datatable HTML to the website repo
  save_widget_html(dt,
                   website_static_path = website_static_path,
                   header_files_dir = datatable_header_files_dir,
                   html_dir = mission_catalog_datatable_dir,
                   html_filename = mission_catalog_datatable_filename,
                   delete_folder_first = TRUE)

  return(dt)

}



make_mission_catalog_map = function(mission_summary,
                                   website_static_path,
                                   leaflet_header_files_dir,
                                   mission_catalog_map_dir,
                                   mission_catalog_map_filename) {

  mission_summary = mission_summary |>
    dplyr::mutate(popup = paste0(
      "<b>Mission ID: </b>", dataset_id_link, "<br>",
      "<b>Date: </b>", earliest_date_derived, "<br>",
      "<b>Altitude (m): </b>", altitude_agl_nominal, "<br>",
      "<b>Camera pitch (deg): </b>", camera_pitch_derived, "<br>",
      "<b>Overlap (front/side): </b>", overlap_combined_nominal, "<br>"
    ))

  mission_centroids = sf::st_centroid(mission_summary)

  m = leaflet() |>
    addTiles(options = providerTileOptions(maxZoom = 16)) |>
    addMarkers(data = mission_centroids, popup = ~popup, clusterOptions = markerClusterOptions(freezeAtZoom = 16)) |>
    addPolygons(data = mission_summary, popup = ~popup, group = "bounds") |>
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo", options = providerTileOptions(maxZoom = 16)) |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
    groupOptions("bounds", zoomLevels = 13:20) |>
    addLayersControl(baseGroups = c("Topo", "Imagery"),
                     options = layersControlOptions(collapsed = FALSE))

  save_widget_html(m,
                   website_static_path = website_static_path,
                   header_files_dir = leaflet_header_files_dir,
                   html_dir = mission_catalog_map_dir,
                   html_filename = mission_catalog_map_filename,
                   delete_folder_first = TRUE)

  return(m)

}



sub_mission_from_image_id = function(image_id) {
  mission_w_sub_mission = str_split(image_id, fixed("_")) |> purrr::map(1) |> unlist()
  sub_mission = str_split(mission_w_sub_mission, fixed("-")) |> purrr::map(2) |> unlist()
  return(sub_mission)
}

#### Make mission-level leaflet map of image points and flight path
make_mission_details_map = function(mission_summary_foc,
                                    mission_points_foc,
                                    mission_polygons_for_mission_details_map,
                                    mission_centroids,
                                    website_static_path,
                                    leaflet_header_files_dir,
                                    mission_details_map_dir) {

  dataset_id = mission_summary_foc$dataset_id

  mission_points_foc = mission_points_foc |>
    # Get which sub-mission each image is from
    mutate(sub_mission = sub_mission_from_image_id(image_id)) |>
    # Create a column "hours elapsed since mission start" for legend coloring
    mutate(time_secs = as.numeric(datetime_local))

  initial_time = min(mission_points_foc$time_secs)

  mission_points_foc = mission_points_foc |>
    mutate(hours_elapsed = (time_secs - initial_time)/60/60) |>
    # Create a popup text
    mutate(popup = paste0("<b>Image ID: </b>", image_id, "<br>"))

  # Optoinal addl rows for popup
    #   "<b>Altitude ASL: </b>", altitude_asl_drone, " m<br>",
    # "<b>Camera pitch: </b>", camera_pitch, " deg<br>",
    # "<b>RTK fix: </b>", rtk_fix, "<br>",
    # "<b>Capture datetime: </b>", datetime_local, "<br>",
    # "<b>Hours elapsed: </b>", round(hours_elapsed, 2), "<br>",
    # "<b>Sub-mission: </b>", sub_mission

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

  # Define color palettes
  pal_hourselapsed = colorNumeric("viridis", domain = mission_points_foc$hours_elapsed)
  pal_pitch = colorNumeric("viridis", domain = mission_points_foc$camera_pitch)
  pal_rtk = colorFactor("viridis", domain = mission_points_foc$rtk)
  pal_sub_mission = colorFactor("viridis", domain = mission_points_foc$sub_mission)
  # All the values of the altitude may be the same. In this case, use a categorical color map to
  # ensure the value is still shown on the legend.
  if (min(mission_points_foc$altitude_asl_drone) == max(mission_points_foc$altitude_asl_drone)) {
    pal_asl = colorFactor("viridis", domain = mission_points_foc$altitude_asl_drone)
  } else {
    pal_asl = colorNumeric("viridis", domain = mission_points_foc$altitude_asl_drone)
  }

  m = leaflet() |>
    addPolygons(data = mission_summary_foc, group = "bounds",
                fillOpacity = 0) |>
    addProviderTiles(providers$Esri.WorldTopo, group = "Topo",
                     options = providerTileOptions(minZoom = 1, maxZoom = 20)) |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery",
                     options = providerTileOptions(minZoom = 1, maxZoom = 20)) |>
    addLayersControl(overlayGroups = c("Imagery", "Topo"), #  ", Nearby missions"
                     baseGroups = c("Hours elapsed", "Altitude ASL (m)", "Camera pitch (deg)", "RTK fix", "Sub-mission"),
                     options = layersControlOptions(collapsed = FALSE)) |>
    # # Nearby mission polygons and centroids
    # addMarkers(data = mission_centroids, popup = ~dataset_id_link, clusterOptions = markerClusterOptions(freezeAtZoom = 16), group = "Nearby missions") |>
    # addPolygons(data = mission_polygons_for_mission_details_map, popup = ~dataset_id_link, group = "Nearby missions") |>
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
                    popup = ~popup,
                    group = "dummyforpopup") |>
    hideGroup("Imagery") |>
    hideGroup("Topo") |>
    hideGroup("Nearby missions") |>
    js_for_legend()

  # Customize background color (can also use this to make transparent)
  backg <- htmltools::tags$style(".leaflet-container { background: rgba(200,200,200,1) }")
  m = prependContent(m, backg)

  # -- Save map HTML to website repo
  mission_details_map_filename = paste0(dataset_id, ".html")
  save_widget_html(m,
                    website_static_path = website_static_path,
                    header_files_dir = leaflet_header_files_dir,
                    html_dir = mission_details_map_dir,
                    html_filename = mission_details_map_filename)

  # Record where it was saved to
  map_html_path = paste(mission_details_map_dir, mission_details_map_filename, sep = "/")

  return(map_html_path)

}



#### Make mission-level leaflet map of image points and flight path
make_itd_map = function(mission_summary_foc,
                                    itd_points_foc,
                                    mission_polygons_for_mission_details_map,
                                    mission_centroids,
                                    website_static_path,
                                    leaflet_header_files_dir,
                                    itd_map_dir) {

  dataset_id = mission_summary_foc$dataset_id

  #mission_summary_foc is the polygon. Buffer it in by 10 because that's the area if detected trees
  #that we retained

  mission_summary_foc = mission_summary_foc |>
    transform_to_local_utm() |>
    st_buffer(-10) |>
    st_transform(4326)

  itd_points_foc = itd_points_foc |>
    mutate(height = round(Z, 1)) |>
    # Create a popup text
    mutate(popup = paste0("<b>Height: </b>", height, " m<br> 
                          <b>Predicted species: </b> <i>coming soon</i><br>")) |>
    st_transform(4326)

  # Optoinal addl rows for popup
    #   "<b>Altitude ASL: </b>", altitude_asl_drone, " m<br>",
    # "<b>Camera pitch: </b>", camera_pitch, " deg<br>",
    # "<b>RTK fix: </b>", rtk_fix, "<br>",
    # "<b>Capture datetime: </b>", datetime_local, "<br>",
    # "<b>Hours elapsed: </b>", round(hours_elapsed, 2), "<br>",
    # "<b>Sub-mission: </b>", sub_mission

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

  # Define color palettes
  pal_height = colorNumeric("viridis", domain = itd_points_foc$height)

  m = leaflet() |>
    addPolygons(data = mission_summary_foc, group = "bounds",
                fillOpacity = 0) |>
    addProviderTiles(providers$Esri.WorldTopo, group = "Topo",
                     options = providerTileOptions(minZoom = 1, maxZoom = 20)) |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery",
                     options = providerTileOptions(minZoom = 1, maxZoom = 20)) |>
    addLayersControl(overlayGroups = c("Imagery", "Topo"), #  ", Nearby missions"
                     options = layersControlOptions(collapsed = FALSE)) |>
    # # Nearby mission polygons and centroids
    # addMarkers(data = mission_centroids, popup = ~dataset_id_link, clusterOptions = markerClusterOptions(freezeAtZoom = 16), group = "Nearby missions") |>
    # addPolygons(data = mission_polygons_for_mission_details_map, popup = ~dataset_id_link, group = "Nearby missions") |>
    # ITD points
    addCircleMarkers(data = itd_points_foc,
                    radius = itd_points_foc$height/5,
                      stroke = FALSE,
                      fillOpacity = 1,
                    #  popup = ~popup,
                      color = pal_height(itd_points_foc$height),
                      group = "Height") |>
    addLegend(pal = pal_height,
              values = itd_points_foc$height,
              title = "Height", opacity = 1,
              group = "Height",
              className = "info legend Height") |>
    # Invisible markers on top of all for popup
    addCircleMarkers(data = itd_points_foc,
                    radius = 10,
                    stroke = FALSE,
                    fillOpacity = 0,
                    popup = ~popup,
                    group = "dummyforpopup") |>
    hideGroup("Imagery") |>
    hideGroup("Topo") |>
    hideGroup("Nearby missions") |>
    js_for_legend()

  # Customize background color (can also use this to make transparent)
  backg <- htmltools::tags$style(".leaflet-container { background: rgba(200,200,200,1) }")
  m = prependContent(m, backg)

  # -- Save map HTML to website repo
  itd_map_filename = paste0(dataset_id, ".html")
  save_widget_html(m,
                    website_static_path = website_static_path,
                    header_files_dir = leaflet_header_files_dir,
                    html_dir = itd_map_dir,
                    html_filename = itd_map_filename)

  # Record where it was saved to
  map_html_path = paste(itd_map_dir, itd_map_filename, sep = "/")

  return(map_html_path)

}



make_mission_details_datatable = function(mission_summary_foc,
                                          website_static_path,
                                          datatable_header_files_dir,
                                          mission_details_datatable_dir) {

  # Select the desired columns, name them, and pivot longer
  d = mission_summary_foc |>
    st_drop_geometry() |>
    # Select just what's needed for a datatable
    dplyr::select(
      "Mission ID" = dataset_id,
      "Sub-mission IDs" = sub_mission_ids,
      "Date" = earliest_date_derived,
      "Altitude (nominal) (m)" = altitude_agl_nominal,
      "Altitude AGL mean (actual) (m)" = altitude_agl_mean_derived,
      "Flight pattern" = flight_pattern,
      "Overlap (nominal) (front/side)" = overlap_front_side_nominal,
      "Camera pitch (deg up from nadir)" = camera_pitch_derived,
      "Smart oblique" = smart_oblique_derived,
      "Terrain follow (nominal)" = terrain_follow,
      "Terrain follow fidelity" = flight_terrain_correlation_derived,
      "Percent RTK" = percent_images_rtk_derived,
      "Flight speed (m/s)" = flight_speed_derived,
      "Exposure median (sec)" = exposure_median_derived,
      "Exposure CV" = exposure_cv_derived,
      "Exposure compensation (nominal)" = exposure_compensation,
      "White balance mode" = white_balance_mode_derived,
      "White balance percent" = white_balance_pct_mode_derived,
      "Flight time range (local)" = time_range_local_derived,
      "Aircraft model" = aircraft_model_name,
      "Sensor model" = sensor_name,
      "Flight planner" = flight_planner_name,
      "Base station latitude" = base_lat,
      "Base station longitude" = base_lon,
      "Base station altitude (m)" = base_alt,
      "Permanent base marker" = base_marked_permanently,
      "Flight footprint area (ha)" = area_derived,
      "Image count" = n_images,
      "Image dimensions" = image_dimensions_derived,
      "Dataset size (GB)" = file_size_derived,
      "File format" = file_format_derived,
      "Project ID" = project_id,
      "Contributor dataset name" = contributor_dataset_name,
      "Creator" = contributor_names,
      "License" = license
    ) |>
      # Pivot longer
      dplyr::mutate(across(everything(), as.character)) |>
      tidyr::pivot_longer(cols = everything(), names_to = "Attribute", values_to = "Value")

  # Prep the JS formatting code
  format_js = DT::JS("function(settings, json) {",
                     "$('body').css({'font-family': 'Arial'});",
                     "}")

  dt = datatable(d, rownames = FALSE, escape = TRUE,
                 colnames = NULL,
                 options = list(paging = FALSE, scrollY = "100%",
                                dom = 't',
                                bSort = FALSE,
                                autoWidth = TRUE,
                                columnDefs = list(list(width = '40%', targets = "Attribute")),
                                #headerCallback = headerCallbackJS,
                                initComplete = format_js))

  dt$sizingPolicy$browser$padding = 0
  dt$sizingPolicy$browser$fill = FALSE

  # Save the datatable HTML to the website repo
  mission_details_datatable_filename = paste0(mission_summary_foc$dataset_id, ".html")
  save_widget_html(dt,
                   website_static_path = website_static_path,
                   header_files_dir = datatable_header_files_dir,
                   html_dir = mission_details_datatable_dir,
                   html_filename = mission_details_datatable_filename)

  # Record where it was saved to
  datatable_html_path = paste(mission_details_datatable_dir, mission_details_datatable_filename, sep = "/")

  return(datatable_html_path)

}


# Compose and render the {dataset_id}.md page for a plot based on the Jinjar template
# TODO: Consider merging this with the render_plot_details_page function (for ground ref plots). It
# would require both data types to have the same page template I think.
render_mission_details_page = function(
    template_filepath,
    mission_summary_foc,
    mission_details_map_path,
    itd_map_path,
    mission_details_datatable_path,
    next_dataset_page_path,
    previous_dataset_page_path,
    website_repo_content_path,
    mission_details_page_dir,
    display_data = FALSE,
    published_data_path = NULL,
    data_server_base_url = "") {

  # The argument `display_data` determines whether to display actual drone data (e.g., images,
  # orthomosaic), as opposed to metadata only 

  dataset_id = mission_summary_foc$dataset_id


  # Determine if this is an oblique mission, so we can enable a message to the top of the page
  # explaining that the photogrammetry products are not expected to look good on their own.
  oblique = abs(mission_summary_foc$camera_pitch_derived) > 10

  # Initialize drone data display parameters to pass to jinjar, starting with value FALSE or NULL,
  # but will be populated during the "display data" step below if specified
  ortho_exists = FALSE
  ortho_url_thumb = NULL
  ortho_url_full = NULL
  chm_exists = FALSE
  chm_url_thumb = NULL
  chm_url_full = NULL
  dsm_exists = FALSE
  dsm_url_thumb = NULL
  dsm_url_full = NULL
  dtm_exists = FALSE
  dtm_url_thumb = NULL
  dtm_url_full = NULL
  pc_exists = FALSE
  pc_url_full = NULL
  mesh_exists = FALSE
  mesh_url_full = NULL
  images_example_exists = FALSE
  images_example_url_thumb = NULL
  images_example_url_full = NULL
  images_zip_exists = FALSE
  images_zip_url = NULL
  footprint_exists = FALSE
  footprint_url = NULL
  cameras_exists = FALSE
  cameras_url = NULL
  log_exists = FALSE
  log_url = NULL
  ttops_exists = FALSE
  ttops_url = NULL


  if (display_data) {

    # Get the processed photogrammetry folder name (for now taking the first if there are multiple)
    sfm_folder = list.files(file.path(published_data_path, dataset_id),
                            pattern = "^processed-",
                            full.names = FALSE,
                            recursive = FALSE) |> rev()
    # In case there is more than one match, take the first (of the reversed data frame -- so
    # actually the most recent)
    sfm_folder = sfm_folder[1]
    
    # Check if ITD products exist and if so, get the URLs needed to add them to the page
    sfm_path = file.path(published_data_path, dataset_id, sfm_folder)
    itd_folder = list.files(sfm_path, full.names = FALSE, pattern = "^itd-", include.dirs = TRUE) |> rev()
    itd_folder = itd_folder[1]
    ttops_file_path = file.path(sfm_path, itd_folder, "treetops.gpkg")

    # Check if products exist and if so, get the URLs needed to add them to the page

    # Orthomosaic
    ortho_exists = file.exists(file.path(published_data_path, dataset_id, sfm_folder, "thumbnails", "orthomosaic.png"))
    ortho_url_thumb = paste(data_server_base_url, dataset_id, sfm_folder, "thumbnails/orthomosaic.png", sep = "/")
    ortho_url_full = paste(data_server_base_url, dataset_id, sfm_folder, "full/orthomosaic.tif", sep = "/")

    # CHM
    chm_exists = file.exists(file.path(published_data_path, dataset_id, sfm_folder, "thumbnails", "chm-mesh.png"))
    chm_url_thumb = paste(data_server_base_url, dataset_id, sfm_folder, "thumbnails/chm-mesh.png", sep = "/")
    chm_url_full = paste(data_server_base_url, dataset_id, sfm_folder, "full/chm-mesh.tif", sep = "/")

    # DSM
    dsm_exists = file.exists(file.path(published_data_path, dataset_id, sfm_folder, "thumbnails", "dsm-mesh.png"))
    dsm_url_thumb = paste(data_server_base_url, dataset_id, sfm_folder, "thumbnails/dsm-mesh.png", sep = "/")
    dsm_url_full = paste(data_server_base_url, dataset_id, sfm_folder, "full/dsm-mesh.tif", sep = "/")

    # DTM
    dtm_exists = file.exists(file.path(published_data_path, dataset_id, sfm_folder, "thumbnails", "dtm-ptcloud.png"))
    dtm_url_thumb = paste(data_server_base_url, dataset_id, sfm_folder, "thumbnails/dtm-ptcloud.png", sep = "/")
    dtm_url_full = paste(data_server_base_url, dataset_id, sfm_folder, "full/dtm-ptcloud.tif", sep = "/")

    # Point cloud
    pc_exists = file.exists(file.path(published_data_path, dataset_id, sfm_folder, "full", "points.laz"))
    pc_url_full = paste(data_server_base_url, dataset_id, sfm_folder, "full/points.laz", sep = "/")

    # Mesh model
    mesh_exists = file.exists(file.path(published_data_path, dataset_id, sfm_folder, "full", "mesh-georeferenced.ply"))
    mesh_url_full = paste(data_server_base_url, dataset_id, sfm_folder, "full/mesh-georeferenced.ply", sep = "/")

    # Raw images
    images_example_exists = file.exists(file.path(published_data_path, dataset_id, "images", "examples", "thumbnails", "example_4.JPG"))
    images_example_url_thumb = paste(data_server_base_url, dataset_id, "images/examples/thumbnails/", sep = "/")
    images_example_url_full = paste(data_server_base_url, dataset_id, "images/examples/fullsize/", sep = "/")

    images_zip_exists = file.exists(file.path(published_data_path, dataset_id, "images", "images.zip"))
    images_zip_url = paste(data_server_base_url, dataset_id, "images/images.zip", sep = "/")

    # Mission footprint
    footprint_exists = file.exists(file.path(published_data_path, dataset_id, "footprint", "footprint.gpkg"))
    footprint_url = paste(data_server_base_url, dataset_id, "footprint/footprint.gpkg", sep = "/")

    # Cameras
    cameras_exists = file.exists(file.path(published_data_path, dataset_id, sfm_folder, "full", "cameras.xml"))
    cameras_url = paste(data_server_base_url, dataset_id, sfm_folder, "full/cameras.xml", sep = "/")

    # Log
    log_exists = file.exists(file.path(published_data_path, dataset_id, sfm_folder, "full", "log.txt"))
    log_url = paste(data_server_base_url, dataset_id, sfm_folder, "full/log.txt", sep = "/")

    # ITD
    ttops_exists = file.exists(ttops_file_path)
    ttops_url = paste(data_server_base_url, dataset_id, sfm_folder, itd_folder, "treetops.gpkg", sep = "/")

  }


  rendered = jinjar::render(
    template_filepath,
    dataset_id = dataset_id,
    oblique = oblique,
    map_html_path = mission_details_map_path,
    itd_map_html_path = itd_map_path,
    datatable_html_path = mission_details_datatable_path,
    next_dataset_page_path = next_dataset_page_path,
    previous_dataset_page_path = previous_dataset_page_path,
    ortho_exists = ortho_exists,
    ortho_url_thumb = ortho_url_thumb,
    ortho_url_full = ortho_url_full,
    chm_exists = chm_exists,
    chm_url_thumb = chm_url_thumb,
    chm_url_full = chm_url_full,
    dsm_exists = dsm_exists,
    dsm_url_thumb = dsm_url_thumb,
    dsm_url_full = dsm_url_full,
    dtm_exists = dtm_exists,
    dtm_url_thumb = dtm_url_thumb,
    dtm_url_full = dtm_url_full,
    pc_exists = pc_exists,
    pc_url_full = pc_url_full,
    mesh_exists = mesh_exists,
    mesh_url_full = mesh_url_full,
    images_example_exists = images_example_exists,
    images_example_url_thumb = images_example_url_thumb,
    images_example_url_full = images_example_url_full,
    images_zip_exists = images_zip_exists,
    images_zip_url = images_zip_url,
    footprint_exists = footprint_exists,
    footprint_url = footprint_url,
    cameras_exists = cameras_exists,
    cameras_url = cameras_url,
    log_exists = log_exists,
    log_url = log_url,
    ttops_exists = ttops_exists,
    ttops_url = ttops_url,
    .config = jinjar_config(variable_open = "{*", variable_close = "*}")
  )

  write_path = file.path(website_repo_content_path,
                         mission_details_page_dir,
                         paste0(dataset_id, ".md"))
  writeLines(rendered, write_path)

  return(TRUE)

}


## Loop through each mission and make a details page, including its media (map and datatable)
make_mission_details_pages = function(
    mission_summary,
    mission_points,
    website_static_path,
    website_content_path,
    leaflet_header_files_dir,
    datatable_header_files_dir,
    mission_details_datatable_dir,
    mission_details_map_dir,
    itd_map_dir,
    mission_details_template_filepath,
    mission_details_page_dir,
    published_data_path = "",
    data_server_base_url = "") {
  mission_summary = mission_summary |> dplyr::arrange(mission_id)

  mission_ids = mission_summary$mission_id
  ndatasets = length(mission_ids)

  mission_centroids = sf::st_centroid(mission_summary)

  for (i in 1:ndatasets) {
    # Get a single row from the summary statistics
    mission_id_foc = mission_ids[[i]]
    # Extract the mission-level metadata that's associated with that dataset
    mission_summary_foc = mission_summary |> filter(mission_id == mission_id_foc)


    ##!!!!!#### TODO: RESUME HERE once image points for each dataset are uploaded

    # From CyVerse, get the image-level metadata associated with this dataset
    folder_foc = paste0("/iplant/home/shared/ofo/public/missions/", mission_id_foc)
    image_point_path = cyverse_list_files("/iplant/home/shared/ofo/public/missions/%/footprint", "footprint.gpkg")
    footprint_urls = cyverse_url(footprint_paths)

    ## Download and merge them all
    plan(multisession)
    footprint_list = furrr::future_map(footprint_urls, sf_from_url, .options = furrr::furrr_options(seed = TRUE))
    footprints = bind_rows(footprint_list)
    plan(sequential)





    # Extract the image-level metadata that's associated with that dataset
    mission_points_foc = mission_points |> filter(mission_id == mission_id_foc)

    cat("\rGenerating details pages (", i, "of", ndatasets, ")    ")

    # Make details map and datatable
    mission_details_map_path = make_mission_details_map(
      mission_summary_foc = mission_summary_foc,
      mission_points_foc = mission_points_foc,
      mission_polygons_for_mission_details_map = mission_summary,
      mission_centroids = mission_centroids,
      website_static_path = website_static_path,
      leaflet_header_files_dir = leaflet_header_files_dir,
      mission_details_map_dir = mission_details_map_dir
    )

    mission_details_datatable_path = make_mission_details_datatable(
      mission_summary_foc = mission_summary_foc,
      website_static_path = website_static_path,
      datatable_header_files_dir = datatable_header_files_dir,
      mission_details_datatable_dir = mission_details_datatable_dir
    )
    
    # Make detected tree map, if ITD data exists
    # Get the ITD folder name (for now taking the first if there are multiple)
    sfm_folder = list.files(file.path(published_data_path, mission_id_foc),
                            pattern = "^processed-",
                            full.names = FALSE,
                            recursive = FALSE) |> rev()
    # In case there is more than one match, take the first (of the reversed data frame -- so
    # actually the most recent)
    sfm_folder = sfm_folder[1]

    # Check if ITD products exist and if so, get the data needed to add them to the page
    sfm_path = file.path(published_data_path, mission_id_foc, sfm_folder)
    itd_folder = list.files(sfm_path, full.names = FALSE, pattern = "^itd-", include.dirs = TRUE) |> rev()
    itd_folder = itd_folder[1]
    ttops_file_path = file.path(sfm_path, itd_folder, "treetops.gpkg")
    ttops_exists = file.exists(ttops_file_path)

    if (ttops_exists) {
      ttops_url = paste(data_server_base_url, mission_id_foc, sfm_folder, itd_folder, "treetops.gpkg", sep = "/")

      itd_points = st_read(ttops_file_path)

      # Make ITD map
      itd_map_path = make_itd_map(
        mission_summary_foc = mission_summary_foc,
        itd_points_foc = itd_points,
        mission_polygons_for_mission_details_map = mission_summary,
        mission_centroids = mission_centroids,
        website_static_path = website_static_path,
        leaflet_header_files_dir = leaflet_header_files_dir,
        itd_map_dir = itd_map_dir
      )
    } else {
      itd_map_path = NA
    }



    # Compute previous and next dataset, looping around as needed
    next_mission_id = ifelse(i < ndatasets, mission_ids[i + 1], mission_ids[1])
    previous_mission_id = ifelse(i > 1, mission_ids[i - 1], mission_ids[ndatasets])

    next_dataset_page_path = paste0("/", mission_details_page_dir, "/", next_mission_id)
    previous_dataset_page_path = paste0("/", mission_details_page_dir, "/", previous_mission_id)

    # Render plot details page from template
    render_mission_details_page(
      template_filepath = mission_details_template_filepath,
      mission_summary_foc = mission_summary_foc,
      mission_details_map_path = mission_details_map_path,
      itd_map_path = itd_map_path,
      mission_details_datatable_path = mission_details_datatable_path,
      next_dataset_page_path = next_dataset_page_path,
      previous_dataset_page_path = previous_dataset_page_path,
      website_repo_content_path = website_content_path,
      mission_details_page_dir = mission_details_page_dir,
      published_data_path = published_data_path,
      data_server_base_url = data_server_base_url,
      display_data = TRUE
    )
  }

}
