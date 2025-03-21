
# Save the library files required for display of datatables
save_dt_header_files = function(website_static_path, datatable_header_files_dir) {

  # Make a temp dir to save the table package to before pulling out the header files
  tempdir = tempdir()
  tabledir = file.path(tempdir, "table-staging")
  if (dir.exists(tabledir)) unlink(tabledir, recursive = TRUE)
  dir.create(tabledir)
  tablepath = file.path(tabledir, "table.html")

  dt = datatable(data.frame(dummy = 1))
  htmlwidgets::saveWidget(dt, tablepath, selfcontained = FALSE)

  # Where did the header files get saved
  table_files_rel = list.files(file.path(tabledir, "table_files"), include.dirs = TRUE)
  table_files_abs = file.path(tabledir, "table_files", table_files_rel)

  header_dir = file.path(website_static_path, datatable_header_files_dir)

  # Copy to the website repo
  if (dir.exists(header_dir)) unlink(header_dir, recursive = TRUE)
  dir.create(header_dir)
  file.copy(table_files_abs,
            header_dir,
            recursive = TRUE)

  return(TRUE)

}

# Save the library files required for display of leaflet maps
save_leaflet_header_files = function(website_static_path, leaflet_header_files_dir) {

  # Make a temp dir to save the map package to before pulling out the header files
  tempdir = tempdir()
  mapdir = file.path(tempdir, "map-staging")
  if (dir.exists(mapdir)) unlink(mapdir, recursive = TRUE)
  dir.create(mapdir)
  mappath = file.path(mapdir, "map.html")

  dummy_markers = data.frame(lat = 38, lon = -121)

  map = leaflet() |>
    addMarkers(data = dummy_markers, clusterOptions = markerClusterOptions(freezeAtZoom = 16)) |>
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo")

  htmlwidgets::saveWidget(map, mappath, selfcontained = FALSE)

  # Where did the header files get saved
  map_files_rel = list.files(file.path(mapdir, "map_files"), include.dirs = TRUE)
  map_files_abs = file.path(mapdir, "map_files", map_files_rel)

  header_dir = file.path(website_static_path, leaflet_header_files_dir)

  # Copy to the website repo
  if (dir.exists(header_dir)) unlink(header_dir, recursive = TRUE)
  dir.create(header_dir)
  file.copy(map_files_abs,
            header_dir,
            recursive = TRUE)

  return(TRUE)

}

# Function for saving leaflet or datatable HTML (only, not headers) to the website repo, based on a
# HTML widget object it is passed
save_widget_html = function(widget,
                            website_static_path,
                            header_files_dir,
                            html_dir,
                            html_filename,
                            delete_folder_first = FALSE) {

  # Make a temp dir to save the map package to before pulling out the map HTML
  tempdir = tempdir()
  widgetdir = file.path(tempdir, "widget-staging")
  if (dir.exists(widgetdir)) unlink(widgetdir, recursive = TRUE)
  dir.create(widgetdir)
  widgetpath = file.path(widgetdir, "widget.html")

  htmlwidgets::saveWidget(widget, widgetpath, selfcontained = FALSE)

  # Extract the widget HTML file (only) to save to the website repo
  widget_html = readLines(widgetpath) |>
    # Update the paths to the header files
    str_replace_all("widget_files", header_files_dir)

  write_dir = file.path(website_static_path,
                        html_dir)

  if(delete_folder_first) {
    if (dir.exists(write_dir)) unlink(write_dir, recursive = TRUE)
    dir.create(write_dir)
  } else {
    if (!dir.exists(write_dir)) dir.create(write_dir)
  }

  # Write table HTML to the website repo
  writeLines(widget_html, file.path(write_dir, html_filename))

  return(TRUE)

}


# For website directories that house plot- or mission-level page components, delete existing directories and
# create new empty directory
reset_detail_dirs = function(website_static_path,
                                  website_content_path,
                                  plot_details_page_dir,
                                  plot_details_map_dir,
                                  itd_map_dir,
                                  plot_details_datatable_dir) {

  plot_details_page_path = file.path(website_content_path, plot_details_page_dir)
  plot_details_map_path = file.path(website_static_path, plot_details_map_dir)
  itd_map_path = file.path(website_static_path, itd_map_dir)
  plot_details_datatable_path = file.path(website_static_path, plot_details_datatable_dir)

  if (dir.exists(plot_details_page_path)) unlink(plot_details_page_path, recursive = TRUE)
  dir.create(plot_details_page_path)
  file.create(file.path(plot_details_page_path, "_index.md"))

  if (dir.exists(plot_details_map_path)) unlink(plot_details_map_path, recursive = TRUE)
  dir.create(plot_details_map_path)
  
  if (dir.exists(itd_map_path)) unlink(itd_map_path, recursive = TRUE)
  dir.create(itd_map_path)

  if (dir.exists(plot_details_datatable_path)) unlink(plot_details_datatable_path, recursive = TRUE)
  dir.create(plot_details_datatable_path)

  return(TRUE)

}