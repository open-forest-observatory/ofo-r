
# Pull in all attributes to the plot summary table, compute relevant columns for display, (like
# plot area in ha instead of m2), round numeric columns, create links to project and dataset pages
compile_mission_summary_data = function(mission_level_metadata, base_ofo_url, mission_details_dir) {

  # TODO: join relevant data to the mission metadata

  # Select relevant columns to display
  d = mission_level_metadata |>
    dplyr::mutate(
      overlap_combined_nominal = paste(overlap_front_nominal, overlap_side_nominal, sep = "/"),
      license = "License will display here",
      dataset_id_link = paste0('<a href="', base_ofo_url, mission_details_dir, dataset_id, '/"', ' target="_PARENT">', dataset_id, "</a>"),
      time_range_local_derived = paste0(earliest_time_local_derived, " to ", latest_time_local_derived),
      overlap_front_side_nominal = paste0(overlap_front_nominal, "/", overlap_side_nominal),
      image_dimensions_derived = paste0(resolution_x_derived, " x ", resolution_y_derived),
      altitude_agl_mean_derived = NA,
      embargoed = FALSE, # Dummy, need to remove if add this attribute to the database
      display_message = NA) # Dummy, need to remove if add this attribute to the database

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
  pal_asl = colorNumeric("viridis", domain = mission_points_foc$altitude_asl_drone)
  pal_pitch = colorNumeric("viridis", domain = mission_points_foc$camera_pitch)
  pal_rtk = colorFactor("viridis", domain = mission_points_foc$rtk)
  pal_sub_mission = colorFactor("viridis", domain = mission_points_foc$sub_mission)

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
      "Sub-mission IDs" = sub_mission_id,
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
      "Dataset license" = license) |>
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
    mission_details_datatable_path,
    next_dataset_page_path,
    previous_dataset_page_path,
    website_repo_content_path,
    mission_details_page_dir) {

  dataset_id = mission_summary_foc$dataset_id

  # Determine if we need a dataset message at the top of the page, and if so, prepare it
  if (mission_summary_foc$embargoed == TRUE) {
    top_message = "This dataset has been submitted to the OFO and is planned for release, but it is currently under embargo. The tree locations have been randomized. For questions about data availability, please contact the dataset contributors."
  } else if (!is.na(mission_summary_foc$display_message)) {
    top_message = mission_summary_foc$display_message
  } else {
    top_message = NA
  }



  rendered = jinjar::render(
    template_filepath,
    dataset_id = dataset_id,
    map_html_path = mission_details_map_path,
    datatable_html_path = mission_details_datatable_path,
    top_message = top_message,
    next_dataset_page_path = next_dataset_page_path,
    previous_dataset_page_path = previous_dataset_page_path
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
    website_static_path,
    website_content_path,
    leaflet_header_files_dir,
    datatable_header_files_dir,
    mission_details_datatable_dir,
    mission_details_map_dir,
    mission_details_template_filepath,
    mission_details_page_dir) {

  mission_summary = mission_summary |>
    dplyr::arrange(dataset_id)

  dataset_ids = mission_summary$dataset_id
  ndatasets = length(dataset_ids)

  mission_centroids = sf::st_centroid(mission_summary)

  for (i in 1:ndatasets) {

    dataset_id_foc = dataset_ids[i]

    cat("\rGenerating details pages (", i, "of", ndatasets, ")    ")

    mission_summary_foc = mission_summary |>
      filter(dataset_id == dataset_id_foc)

    mission_points_foc = mission_points |>
      rename(dataset_id = dataset_id_image_level) |>
      filter(dataset_id == dataset_id_foc)

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

    next_dataset_id = ifelse(i < ndatasets, dataset_ids[i + 1], dataset_ids[1])
    previous_dataset_id = ifelse(i > 1, dataset_ids[i - 1], dataset_ids[ndatasets])

    next_dataset_page_path = paste0("/", mission_details_page_dir, "/", next_dataset_id)
    previous_dataset_page_path = paste0("/", mission_details_page_dir, "/", previous_dataset_id)

    # Render plot details page from template
    render_mission_details_page(
      template_filepath = mission_details_template_filepath,
      mission_summary_foc = mission_summary_foc,
      mission_details_map_path = mission_details_map_path,
      mission_details_datatable_path = mission_details_datatable_path,
      next_dataset_page_path = next_dataset_page_path,
      previous_dataset_page_path = previous_dataset_page_path,
      website_repo_content_path = website_content_path,
      mission_details_page_dir = mission_details_page_dir
    )
  }

}
