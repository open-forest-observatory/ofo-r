
# Pull in all attributes to the plot summary table, compute relevant columns for display, (like
# plot area in ha instead of m2), round numeric columns, create links to project and dataset pages
compile_mission_summary_table = function(mission_level_metadata, base_ofo_url, mission_details_dir) {

  # TODO: join relevant data to the mission metadata

  # Select relevant columns to display
  d = mission_level_metadata |>
    dplyr::mutate(overlap_combined_nominal = paste(overlap_front_nominal, overlap_side_nominal, sep = "/"),
           license = "License will display here")

  # Create links to project and dataset pages
  d = d |>
    dplyr::mutate(dataset_id_link = paste0('<a href="', base_ofo_url, mission_details_dir, dataset_id, '/"', ' target="_PARENT">', dataset_id, "</a>"))

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
           "Aircraft" = aircraft_model_name)

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

  mission_centroids = sf::st_centroid(mission_summary)

  m = leaflet() |>
    addTiles(options = providerTileOptions(maxZoom = 16)) |>
    addMarkers(data = mission_centroids, popup = ~dataset_id_link, clusterOptions = markerClusterOptions(freezeAtZoom = 16)) |>
    addPolygons(data = mission_summary, group = "bounds") |>
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
