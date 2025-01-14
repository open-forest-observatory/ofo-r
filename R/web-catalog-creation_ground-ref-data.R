# Functions for populating OFO website repo with reference data pages

read_and_standardize_tabular_field_ref_data = function(google_sheet_id) {

  ## Load field ref data (tabular)
  projects = read_sheet(google_sheet_id, sheet = "field-projects")
  plots = read_sheet(google_sheet_id, sheet = "field-plots")
  subplots = read_sheet(google_sheet_id, sheet = "field-subplots")
  trees = read_sheet(google_sheet_id, sheet = "field-trees")
  species_codes = read_sheet(google_sheet_id, sheet = "species-codes")

  # Fix a col that's being imported as a list col even though it's character
  plot = plots |>
    mutate(contributor_plot_id = as.character(contributor_plot_id))

  # Standardize tabular plot ID formatting
  plots = plots |>
    mutate(plot_id = str_pad(plot_id, 4, pad = "0", side = "left"))

  trees = trees |>
    mutate(plot_id = str_pad(plot_id, 4, pad = "0", side = "left")) |>
    # Fix a col that's being imported as a list col even though it's character
    mutate(species = as.character(species))

  # Return a list
  ret = list(projects = projects,
             plots = plots,
             subplots = subplots,
             trees = trees,
             species_codes = species_codes)

  return(ret)

}


# Read in plot boundary files (one file per plot) and merge to one object
read_and_merge_plot_boundaries = function(plot_boundaries_dir, base_ofo_url, plot_details_dir) {

  ## Load plot boundaries and merge to one layer
  bound_files = list.files(plot_boundaries_dir, full.names = TRUE,  pattern = ".gpkg$")
  bounds_sf_list = lapply(bound_files, sf::st_read)

  for (i in seq_along(bounds_sf_list)) {
    # remove all cols except geometry and assign plot_id based on the filename, and project to 4326
    bounds_sf_list[[i]] = bounds_sf_list[[i]] |>
      select() |>
      mutate(plot_id = gsub(".gpkg", "", basename(bound_files[i]))) |>
      st_transform(4326)
  }

  bounds = bind_rows(bounds_sf_list)

  # Remove Z dim if it exists
  bounds = sf::st_zm(bounds)

  ## Standardize boundary plot ID formatting and compute area
  bounds = bounds |>
    mutate(plot_id = str_pad(plot_id, 4, pad = "0", side = "left"))

  bounds$area_ha_sf = (sf::st_area(bounds) |> as.numeric() / 10^4) |> round(4)  # Convert to hectares

  bounds = bounds |>
    mutate(bounds_plot_id_link = paste0('<a href="', base_ofo_url, plot_details_dir, plot_id, '/"', ' target="_PARENT">', plot_id, "</a>")) |>
    arrange(-area_ha_sf)

  # TODO: Simplifying plot boundaries here to reduce file size of resulting leaflet map

  return(bounds)

}


# Checks for completness of field ref tabular data and plot boundaries
check_field_ref_data = function(tabular_data, plot_bounds) {

  # Make sure there's a boundary for every plot in the tabular data
  plots_no_boundaries = setdiff(tabular_data$plots$plot_id, plot_bounds$plot_id)
  if(length(plots_no_boundaries) > 0) {
    plot_list = paste(plots_no_boundaries, collapse = ", ")
    warning("The following plots are missing boundaries:", plot_list)
  }

  # Make sure there's a plot for every tree in the tabular data
  trees_no_plots = setdiff(tabular_data$trees$plot_id, plot_bounds$plot_id)
  if(length(trees_no_plots) > 0) {
    plot_list = paste(trees_no_plots, collapse = ", ")
    warning("The following trees are missing plots:", plot_list)
  }
  
  # See which numeric species codes  exist in the dataset but do not have a corresponding code_usda
  # or code_supp
  numeric_codes_in_use = tabular_data$trees$species
  numeric_codes_defined = tabular_data$species_codes$code_numeric
  codes_in_use_not_defined = numeric_codes_in_use[!numeric_codes_in_use %in% numeric_codes_defined]
  table = table(codes_in_use_not_defined) |> sort(decreasing = TRUE)
  code_list = names(table) |> paste(collapse = ", ")
  if (code_list != "") {
    warning("The following numeric species codes are in use but not defined in the species codes table (in order or decreasing frequency); using the codes as they appear here: ", code_list)
    print(table)
  }

}


# Prepare tree data computing and merging in the necessary fields
prep_trees = function(trees, species_codes) {

  # Compute tree basal area
  trees = trees |>
    mutate(ba = pi * (dbh / 2)^2 / 10000) |>
    # If there is no DBH, use height for size plotting
    # TODO: Make this check whether the whole plot had height
    mutate(size = ifelse(is.na(dbh), height, dbh))

  # Pull in 4-letter USDA species codes
  species_codes = species_codes |>
    mutate(code_numeric = as.character(code_numeric)) |>
    mutate(sp_code = ifelse(!is.na(code_usda), code_usda, code_supp))

  trees = trees |>
    mutate(species = as.character(species)) |>
    left_join(species_codes, by = join_by(species == code_numeric)) |>
    # If there's an entry in the tree table species column, but no matching species in the code
    # table, use whatever is in the tree table (we sometimes record USDA codes there for species
    # that don't have a numeric tree code, like shrubs)
    mutate(sp_code = ifelse(is.na(sp_code), species, sp_code))

  # If the tree is dead and species is UNK, set species to SNAG
  trees = trees |>
    mutate(sp_code = ifelse(((live_dead == "D" & !is.na(sp_code)) & sp_code == "UNK"), "UNKSNAG", sp_code))

  return(trees)

}


prep_plots = function(plots) {

  # If plot-level min_dbh_ohvis is missing, then min_dbh applied to overhead trees too, so use that value
  plots = plots |>
    mutate(min_ht_ohvis = ifelse(is.na(min_ht_ohvis), min_ht, min_ht_ohvis)) |>
    mutate(embargoed = ifelse(is.na(embargoed), FALSE, embargoed))

}


# Prepare tree data for plot-level summary by dropping outliers
prep_trees_for_plot_summary = function(trees) {

  # Remove outlier trees for purposes of summarizing plot data (not for the actual tree-level data to report)
  trees_clean = trees |>
    filter((is.na(dbh) | dbh < 500) & (is.na(height) | height < 100))

  return(trees_clean)

}


# Compute plot-level summary of tree data, including species proportions. Includes computation of
# tree proportion by BA, height, and "size", which is previously computed as DBH or height (which
# height is not available). TODO: switch to using "size" once "size" is corrected per github issue.
summarize_trees_by_plot = function(trees_clean) {

  # Summarize tree data at the plot level
  tree_summ = trees_clean |>
    group_by(plot_id) |>
    summarize(dbh_mean = mean(dbh) |> round(1),
              dbh_quad_mean = sqrt(mean(dbh^2)) |> round(1),
              dbh_sd = sd(dbh) |> round(1),
              dbh_cv = (sd(dbh) / mean(dbh)) |> round(2),
              n_trees = n(),
              n_gt40in = sum(dbh > (40*2.54), na.rm = TRUE),
              ba_tot = sum(ba, na.rm = TRUE),
              dbh_tot = sum(dbh, na.rm = TRUE),
              ht_tot = sum(height, na.rm = TRUE),
              size_tot = sum(size, na.rm = TRUE),
              height_measured = (sum(!is.na(height)) / n()) > 0.9) |>
    # if no trees with DBH measured, set BA to NA instead of 0
    mutate(ba_tot = ifelse(ba_tot == 0, NA, ba_tot))

  # Add summarized plot-level tree data to tree-level table to enable computing proportions
  trees_w_summ = left_join(tree_summ, trees_clean, by = "plot_id") |>
    rename(n_trees_plot = n_trees,
           ba_plot = ba_tot,
           ht_plot = ht_tot,
           size_plot = size_tot)

  # Compute top tree species by plot using BA, or if no BA, by ht (long format)
  top_species = trees_w_summ |>
    filter(!is.na(sp_code)) |>
    mutate(species = as.character(sp_code)) |>
    group_by(plot_id, sp_code) |>
    summarize(n_trees_sp = n(),
              ba_sp = sum(ba, na.rm = TRUE),
              size_sp = sum(size, na.rm = TRUE),
              ht_sp = sum(height, na.rm = TRUE),
              n_trees_plot = median(n_trees_plot, na.rm = TRUE),
              ba_plot = median(ba_plot, na.rm = TRUE),
              ht_plot = median(ht_plot, na.rm = TRUE),
              size_plot = median(size_plot, na.rm = TRUE)) |>
    mutate(prop_trees_sp_ba = ba_sp / ba_plot,
           prop_trees_sp_size = size_sp / size_plot,
           prop_trees_sp_ht = ht_sp / ht_plot,
           prop_trees_sp = ifelse(is.na(prop_trees_sp_ba), prop_trees_sp_ht, prop_trees_sp_ba)) |>
    mutate(prop_trees_sp = round(prop_trees_sp, 2) * 100) |>
    group_by(plot_id) |>
    arrange(plot_id, desc(prop_trees_sp)) |>
    slice_head(n = 3)

  # Merge species code and proportion for top 3 species into one column, and again for just the top
  # 1 species. Two option for the format of the species code and proportion. sp_w_prop_alt is a
  # nicer but wider version of sp_w_prop
  top_species = top_species |>
    mutate(sp_w_prop = paste0(prop_trees_sp, "-", sp_code),
           sp_w_prop_alt = paste0(sp_code, " (", prop_trees_sp, "%)")) |>
    group_by(plot_id) |>
    summarize(top_species = paste(sp_w_prop_alt, collapse = ", "),
              top_one_species = first(sp_w_prop_alt))

  # Merge top species into plot-level tree summary

  tree_summ = tree_summ |>
    left_join(top_species, by = "plot_id")

  return(tree_summ)

}

# Pull in all attributes to the plot summary table, compute relevant columns for display, (like
# plot area in ha instead of m2), round numeric columns, create links to project and dataset pages
compile_plot_summary_table = function(plots, projects, plot_level_tree_summary, bounds, base_ofo_url, plot_details_dir) {

  # Prep plot bounds data (just plot area) for merging
  bounds_nosp = st_drop_geometry(bounds) |>
    select(plot_id, area_ha_sf)

  # To the plot data, join project data, tree-summary data, plot bounds area
  plotproj = left_join(plots, projects, by = "project_id") |>
    left_join(plot_level_tree_summary, by = "plot_id") |>
    left_join(bounds_nosp, by = "plot_id") |>
    # for some reason this is converting contributor_plot_id to a list column, so convert back to
    # character
    mutate(contributor_plot_id = as.character(contributor_plot_id))

  # Compute relevant columns to display
  plotproj = plotproj |>
    mutate(survey_year = str_sub(survey_date, 1, 4),
          plot_area_ha = plot_area / 10000,
          ba_ha = ba_tot / area_ha_sf) |>
    # round numeric columns
    mutate(area_ha_sf = round(area_ha_sf, 2),
          ba_ha = round(ba_ha, 0),
          min_ht_ohvis = round(min_ht_ohvis, 2),
          min_ht = round(min_ht, 2),
          min_dbh = round(min_dbh, 1))

  # Create links to project and dataset pages
  plotproj = plotproj |>
    mutate(plot_id_link = paste0('<a href="', base_ofo_url, plot_details_dir, plot_id, '/"', ' target="_PARENT">', plot_id, "</a>"))

  return(plotproj)

}



make_plot_catalog_datatable = function(plot_summary,
                                       website_static_path,
                                       datatable_header_files_dir,
                                       plot_catalog_datatable_dir,
                                       plot_catalog_datatable_filename) {

  # Select relevant columns to display
  d = plot_summary |>
    select("ID" = plot_id_link,
           "Area (ha)" = area_ha_sf,
           "N Trees" = n_trees,
           "BA (m2/ha)" = ba_ha,
           "Top Species" = top_one_species,
           "Min DBH (cm)" = min_dbh,
           "Min Ht OH (m)" = min_ht_ohvis,
           "Height Meas" = height_measured,
           "Year" = survey_year,
           "Project" = name_short,
           "License" = license_short)

  # Prep formatting code to pass to datatable creation
  format_js = DT::JS("function(settings, json) {",
                     "$('body').css({'font-family': 'Arial'});",
                     "}")

  dt = DT::datatable(d,
                     rownames = FALSE,
                     escape = FALSE,
                     options = list(paging = FALSE, scrollY = "100%",initComplete = format_js)) |>
    DT::formatStyle(names(d),
                    lineHeight = '100%',
                    padding = '4px 15px 4px 15px')

  # Save the datatable HTML to the website repo
  save_widget_html(dt,
                   website_static_path = website_static_path,
                   header_files_dir = datatable_header_files_dir,
                   html_dir = plot_catalog_datatable_dir,
                   html_filename = plot_catalog_datatable_filename,
                   delete_folder_first = TRUE)

  return(dt)

}


make_plot_catalog_map = function(plot_summary,
                                 plot_centroids,
                                 plot_bounds,
                                 website_static_path,
                                 leaflet_header_files_dir,
                                 plot_catalog_map_dir,
                                 plot_catalog_map_filename) {

  d_map = left_join(plot_centroids, plot_summary, by = "plot_id")

  m = leaflet() |>
    addTiles(options = providerTileOptions(maxZoom = 20)) |>
    addMarkers(data = d_map, popup = ~plot_id_link, clusterOptions = markerClusterOptions(freezeAtZoom = 20)) |>
    addPolygons(data = plot_bounds, popup = ~bounds_plot_id_link, group = "bounds") |>
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
    groupOptions("bounds", zoomLevels = 13:20) |>
    addLayersControl(baseGroups = c("Topo", "Imagery"),
                     options = layersControlOptions(collapsed = FALSE))

  save_widget_html(m,
                   website_static_path = website_static_path,
                   header_files_dir = leaflet_header_files_dir,
                   html_dir = plot_catalog_map_dir,
                   html_filename = plot_catalog_map_filename,
                   delete_folder_first = TRUE)

  return(m)

}

# Prepare the tree data for the stem map by pulling in plot summary data for each tree. To prevent
# bad scaling of tree point sizes, supply tree data with anomalous outliers removed (though this
# will result in the outlier trees not being displayed on the map)
prep_trees_for_stem_map = function(trees, plot_summary) {

  trees_map = trees |>
    left_join(plot_summary, by = "plot_id")

  return(trees_map)

}

# Scale tree point sizes so they look nice on the leaflet map
rescale_size = function(x, min_size = 5, max_size = 20) {

  # If there's only one tree, the math below won't work, so just return the mean of the min and max
  # disaplay sizes
  if(length(x) == 1) {
    return(mean(c(min_size, max_size)))
  }

  x = (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  x = x * (max_size - min_size) + min_size
  return(x)
}


# Make plot-level leaflet stem map
make_plot_details_map = function(plot_summary_foc,
                                 bound_foc,
                                 trees_foc,
                                 website_static_path,
                                 leaflet_header_files_dir,
                                 plot_details_map_dir) {

  # Prep trees as sf object
  trees_prepped = st_as_sf(trees_foc, coords = c("tree_lon", "tree_lat"), crs = 4326) |>
    # largest trees on top
    arrange(-size)

  # TODO: move the rounding outside of the function
  # Prep fields for display
  trees_prepped = trees_prepped |>
    mutate(dbh = round(dbh),
           height = round(height, 1)) |>
    mutate(popup = paste0("<b>Species:</b> ", sp_code, "<br> <b>DBH (cm):</b> ", dbh, "<br> <b>Height (m):</b> ", height, "<br> <b>Live/dead:</b> ", live_dead, "<br> <b>Contrib. tree ID:</b> ", contributor_tree_id))

  # If embargoed, scramble the tree locations
  if (plot_summary_foc$embargoed == TRUE) {

    # Make random points within the bounds, as many as the original dataset
    n = nrow(trees_prepped)
    newpoints = st_sample(bound_foc, size = n, exact = TRUE)
    # Set the geometry of the original points to the new random points
    geom = st_geometry(newpoints)
    st_geometry(trees_prepped) = geom

  }

  # Set up a popup

  # Define discrete color palette for species
  pal = colorFactor("viridis", trees_prepped$sp_code)

  # Make the map
  m = leaflet() |>
    addPolygons(data = bound_foc, group = "bounds") |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery",
                    options = providerTileOptions(minZoom = 1, maxZoom = 20)) |>
    addCircleMarkers(data = trees_prepped,
                     popup = ~popup,
                     stroke = FALSE,
                     fillOpacity = 1,
                     color = pal(trees_prepped$sp_code),
                     radius = rescale_size(trees_prepped$size, 5, 20)) |>
    hideGroup("Imagery") |>
    #groupOptions("bounds", zoomLevels = 13:20) |>
    addLayersControl(overlayGroups = c("Imagery"),
                     options = layersControlOptions(collapsed = FALSE)) |>
    addLegend(pal = pal, values = trees_prepped$sp_code, title = "Species", opacity = 1)

  # Customize background color (can also use this to make transparent)
  backg <- htmltools::tags$style(".leaflet-container { background: rgba(200,200,200,1) }")
  m = prependContent(m, backg)

  # -- Save map HTML to website repo
  plot_details_map_filename = paste0(plot_summary_foc$plot_id, ".html")
  save_widget_html(m,
                   website_static_path = website_static_path,
                   header_files_dir = leaflet_header_files_dir,
                   html_dir = plot_details_map_dir,
                   html_filename = plot_details_map_filename)

  # Record where it was saved to
  map_html_path = paste(plot_details_map_dir, plot_details_map_filename, sep = "/")

  return(map_html_path)

}


make_plot_details_datatable = function(plot_summary_foc,
                                       trees_foc,
                                       website_static_path,
                                       datatable_header_files_dir,
                                       plot_details_datatable_dir) {
  # Select the correct columns, name them, and pivot table longer
  d = plot_summary_foc |>
    # Select just what's needed for a datatable
    select("Plot ID" = plot_id,
          "Project" = name_short,
          "Measurement year" = survey_year,
          "Plot area (ha)" = area_ha_sf,
          "Tree count" = n_trees,
          "Basal area (m2/ha)" = ba_ha,
          "Top species" = top_species,
          "Minimum DBH measured (cm)" = min_dbh,
          "Minimum height measured (for trees visible from overhead) (m)" = min_ht_ohvis,
          "Minimum height measured (for other trees) (m)" = min_ht,
          "Data license" = license_short,
          "Data license details" = license,
          "Contributor/Investigator" = investigator_names,
          "Project description" = description,
          "Contributor plot ID" = contributor_plot_id) |>
    # Pivot longer
    mutate(across(everything(), as.character)) |>
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
  plot_details_datatable_filename = paste0(plot_summary_foc$plot_id, ".html")
  save_widget_html(dt,
                   website_static_path = website_static_path,
                   header_files_dir = datatable_header_files_dir,
                   html_dir = plot_details_datatable_dir,
                   html_filename = plot_details_datatable_filename)

  # Record where it was saved to
  datatable_html_path = paste(plot_details_datatable_dir, plot_details_datatable_filename, sep = "/")

  return(datatable_html_path)

}

# Compose and render the {plot_id}.md page for a plot based on the Jinjar template
render_plot_page = function(template_filepath,
                            plot_summary_foc,
                            plot_details_map_path,
                            plot_details_datatable_path,
                            website_repo_content_path,
                            plot_details_page_dir) {

  plot_id = plot_summary_foc$plot_id

  # Determine if we need a dataset message at the top of the page, and if so, prepare it
  if (plot_summary_foc$embargoed == TRUE) {
    top_message = "This dataset has been submitted to the OFO and is planned for release, but it is currently under embargo. The tree locations have been randomized. For questions about data availability, please contact the dataset contributors."
  } else if (!is.na(plot_summary_foc$display_message)) {
    top_message = plot_summary_foc$display_message
  } else {
    top_message = NA
  }

  rendered = jinjar::render(template_filepath,
                            plot_id = plot_id,
                            map_html_path = plot_details_map_path,
                            datatable_html_path = plot_details_datatable_path,
                            top_message = top_message)

  write_path = file.path(website_repo_content_path,
                         plot_details_page_dir,
                         paste0(plot_id, ".md"))
  writeLines(rendered, write_path)

  return(TRUE)

}

## Loop through each plot and make a details page, including its media (map and datatable)
make_plot_details_pages = function(plot_summary,
                              bounds,
                              trees_vis,
                              website_static_path,
                              leaflet_header_files_dir,
                              datatable_header_files_dir,
                              plot_details_datatable_dir,
                              plot_details_map_dir,
                              plot_details_template_dir,
                              plot_details_page_dir) {

  plot_ids = plot_summary$plot_id
  nplots = length(plot_ids)

  for (plot_id_foc in plot_ids) {

    cat("\rGenerating details page for plot ", plot_id_foc, "of", nplots, "    ")

    plot_summary_foc = plot_summary |>
      filter(plot_id == plot_id_foc)

    bound_foc = bounds |>
      filter(plot_id == plot_id_foc)

    trees_foc = trees_vis |>
      filter(plot_id == plot_id_foc)

    plot_details_map_path = make_plot_details_map(
      plot_summary_foc = plot_summary_foc,
      bound_foc = bound_foc,
      trees_foc = trees_foc,
      website_static_path = WEBSITE_STATIC_PATH,
      leaflet_header_files_dir = LEAFLET_HEADER_FILES_DIR,
      plot_details_map_dir = PLOT_DETAILS_MAP_DIR
    )

    plot_details_datatable_path = make_plot_details_datatable(
      plot_summary_foc = plot_summary_foc,
      website_static_path = WEBSITE_STATIC_PATH,
      datatable_header_files_dir = DATATABLE_HEADER_FILES_DIR,
      plot_details_datatable_dir = PLOT_DETAILS_DATATABLE_DIR
    )

    render_plot_page(template_filepath = PLOT_DETAILS_TEMPLATE_FILEPATH,
                    plot_summary_foc = plot_summary_foc,
                    plot_details_map_path = plot_details_map_path,
                    plot_details_datatable_path = plot_details_datatable_path,
                    website_repo_content_path = WEBSITE_CONTENT_PATH,
                    plot_details_page_dir = PLOT_DETAILS_PAGE_DIR)
  }

}