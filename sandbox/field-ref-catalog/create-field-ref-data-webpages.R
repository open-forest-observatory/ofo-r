# Purpose: Create the Hugo markdown pages for all field reference sites, as well as a dataset index
# page

library(dplyr)
library(googlesheets4)
library(leaflet)
library(stringr)
library(sf)
library(htmlwidgets)
library(DT)

datadir = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt", n = 1)

# ---- File path constants

OVERVIEW_DATA_DIR = "~/repos/ofo-website-3/static/field-data-overviews/"
DATATABLE_HEADER_FILES_DIR = "~/repos/ofo-website-3/static/datatable-header-files/"
LEAFLET_HEADER_FILES_DIR = "~/repos/ofo-website-3/static/leaflet-header-files/"
PLOT_DATATABLE_HTML_DIR = "~/repos/ofo-website-3/static/field-plot-datatables/"
PLOT_MAP_HTML_DIR = "~/repos/ofo-website-3/static/field-plot-maps/"
PLOT_DATA_PAGE_DIR = "~/repos/ofo-website-3/content/data-field-ref-plot/"

BASE_OFO_URL = "https://openforestobservatory.netlify.app/"




# Load field ref data (tabular)
projects = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "field-projects")
plots = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "field-plots")
subplots = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "field-subplots")
trees = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "field-trees")
species_codes = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "species-codes")


# --- Data cleaning and prep

# -- Compiling and prepping plot boundaries

# Load plot boundaries and merge to one layer
bound_files = list.files(file.path(datadir, "field-plot-boundaries"), full.names = TRUE,  pattern = ".gpkg$")
bounds_sf_list = lapply(bound_files, sf::st_read)

for (i in seq_along(bounds_sf_list)) {
  # remove all cols except geometry and assign plot_id based on the filename
  bounds_sf_list[[i]] = bounds_sf_list[[i]] |>
    select() |>
    mutate(plot_id = gsub(".gpkg", "", basename(bound_files[i])))
}

bounds = bind_rows(bounds_sf_list)

# Standardize boundary plot ID formatting and compute area
bounds = bounds |>
  mutate(plot_id = str_pad(plot_id, 4, pad = "0", side = "left"))

bounds$area_ha_sf = (sf::st_area(bounds) |> as.numeric() / 10^4) |> round(4) # Convert to hectares


# -- Cleaning tabular data

# Fix a col that's being imported as a list col even though it's character
plot = plots |>
  mutate(contributor_plot_id = as.character(contributor_plot_id))

# Standardize tabular plot ID formatting
plots = plots |>
  mutate(plot_id = str_pad(plot_id, 4, pad = "0", side = "left"))

trees = trees |>
  mutate(plot_id = str_pad(plot_id, 4, pad = "0", side = "left"))


# --- Data checking

# Make sure there's a boundary for every plot in the tabular data
plots_no_boundaries = setdiff(plots$plot_id, bounds$plot_id)
plots_no_boundaries

# Make sure there's a plot for every tree in the tabular data
trees_no_plots = setdiff(trees$plot_id, plots$plot_id)
trees_no_plots


# --- Notes on data issues:
# - tree 4310 (VP dataset) has `crown_position` of 7, which is nonsensical and translates to ohvis
#   of 7
# - ******* Johnston plot cleaning

# --- TODO:
# - Options to color markers by project? Plot size? Etc. https://stackoverflow.com/questions/47966375/how-to-create-leaflet-markers-colored-by-a-numeric-variable

# ---- Processing

# -- General prep of tree-level data, including summarizing at plot level

# Compute tree basal area
trees = trees |>
  mutate(ba = pi * (dbh / 2)^2 / 10000)

# Remove outlier trees for purposes of summarizing plot data (not for the actual tree-level data to report)
trees_clean = trees |>
  filter((is.na(dbh) | dbh < 500) & (is.na(height) | height < 100))
  
# Pull in 4-letter USDA species codes
species_codes = species_codes |>
  mutate(code_numeric = as.character(code_numeric)) |>
  mutate(sp_code = ifelse(!is.na(code_usda), code_usda, code_supp))
trees_clean = trees_clean |>
  mutate(species = as.character(species)) |>
  left_join(species_codes, by = join_by(species == code_numeric))
  
# If the tree is dead, set species to SNAG
trees_clean = trees_clean |>
  mutate(species = ifelse(live_dead == "D", "SNAG", species))
  
# Summarize tree data at the plot level
tree_summ = trees_clean |>
  group_by(plot_id) |>
  summarize(dbh_mean = mean(dbh) |> round(1),
            dbh_sd = sd(dbh) |> round(1),
            dbh_cv = (sd(dbh) / mean(dbh)) |> round(2),
            n_trees = n(),
            ba_tot = sum(ba, na.rm = TRUE),
            ht_tot = sum(height, na.rm = TRUE),
            height_measured = (sum(!is.na(height)) / n()) > 0.9)

# Add plot-level summary data to tree table to enable computing proportions
trees_w_summ = left_join(tree_summ, trees_clean, by = "plot_id") |>
  rename(n_trees_plot = n_trees,
         ba_plot = ba_tot,
         ht_plot = ht_tot)

# Compute top tree species by BA, or if no BA, by ht (long format)
top_species = trees_w_summ |>
  filter(!is.na(sp_code)) |>
  mutate(species = as.character(sp_code)) |>
  group_by(plot_id, sp_code) |>
  summarize(n_trees_sp = n(),
            ba_sp = sum(ba),
            ht_sp = sum(height),
            n_trees_plot = median(n_trees_plot),
            ba_plot = median(ba_plot),
            ht_plot = median(ht_plot)) |>
  mutate(prop_trees_sp_ba = ba_sp / ba_plot,
         prop_trees_sp_ht = ht_sp / ht_plot,
         prop_trees_sp = ifelse(is.na(prop_trees_sp_ba), prop_trees_sp_ht, prop_trees_sp_ba)) |>
  mutate(prop_trees_sp = round(prop_trees_sp, 2) * 100) |>
  group_by(plot_id) |>
  arrange(plot_id, desc(n_trees_sp)) |>
  slice_head(n = 3)

# Merge species USDA code and proportion into one column
# sp_w_prop_alt is a nicer but wider version of sp_w_prop
top_species = top_species |>
  mutate(sp_w_prop = paste0(prop_trees_sp, "-", sp_code),
         sp_w_prop_alt = paste0(sp_code, " (", prop_trees_sp, "%)")) |>
  group_by(plot_id) |>
  summarize(top_species = paste(sp_w_prop_alt, collapse = ", "),
            top_one_species = first(sp_w_prop_alt))


# -- General prep of plot-level data

# If plot-level min_dbh_ohvis is missing, use min_dbh
plots = plots |>
  mutate(min_ht_ohvis = ifelse(is.na(min_ht_ohvis), min_ht, min_ht_ohvis))

# Compute plot centroids
plot_centroids = sf::st_centroid(bounds)

# ----  Export and save datatable header files

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

# Copy to the website repo
if (dir.exists(DATATABLE_HEADER_FILES_DIR)) unlink(DATATABLE_HEADER_FILES_DIR, recursive = TRUE)
dir.create(DATATABLE_HEADER_FILES_DIR)
file.copy(table_files_abs,
          DATATABLE_HEADER_FILES_DIR,
          recursive = TRUE)


# ----  Export and save leaflet header files

# Make a temp dir to save the map package to before pulling out the header files
tempdir = tempdir()
mapdir = file.path(tempdir, "map-staging")
if (dir.exists(mapdir)) unlink(mapdir, recursive = TRUE)
dir.create(mapdir)
mappath = file.path(mapdir, "map.html")

map = leaflet()
htmlwidgets::saveWidget(map, mappath, selfcontained = FALSE)

# Where did the header files get saved
map_files_rel = list.files(file.path(mapdir, "map_files"), include.dirs = TRUE)
map_files_abs = file.path(mapdir, "map_files", map_files_rel)

# Copy to the website repo
if (dir.exists(LEAFLET_HEADER_FILES_DIR)) unlink(LEAFLET_HEADER_FILES_DIR, recursive = TRUE)
dir.create(LEAFLET_HEADER_FILES_DIR)
file.copy(map_files_abs,
          LEAFLET_HEADER_FILES_DIR,
          recursive = TRUE)



# # Extract the datatable HTML header that links in these files, and sub in the new paths
# THIS IS NOW OBSOLETE because embedding the entire table HTML as an iframe

# dt_html = readLines(tablepath)
# dt_html_header = dt_html[grep("<head>", dt_html)[1]:grep("</head>", dt_html)[1]]
# header_file_line_idxs = grep("(<link)|(<script)", dt_html_header)
# header_file_lines = dt_html_header[header_file_line_idxs]

# header_file_lines_updated = header_file_lines |>
#   str_replace_all("table_files", "/datatable-header-files") |>
#   str_replace_all("table_files", "/datatable-header-files") |>
#   paste(collapse = "\n")
# # ^ these lines are now ready to write into the markdown templates







# -- Make a HTML data table of plot catalog

# Prep plot bounds data for merging
bounds_nosp = st_drop_geometry(bounds) |>
  select(plot_id, area_ha_sf)

# To the plot data, join project data, top-species data, tree-summary data, plot bounds area
plotproj = left_join(plots, projects, by = "project_id") |>
  left_join(top_species, by = "plot_id") |>
  left_join(tree_summ, by = "plot_id") |>
  left_join(bounds_nosp, by = "plot_id") |>
  # for some reason this is converting contributor_plot_id to a list column
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
         min_dbh = round(min_dbh, 1))
  
# Create links to project and dataset pages
plotproj = plotproj |>
  mutate(plot_id_link = paste0('<a href="', BASE_OFO_URL, "data-field-ref-plot/", plot_id, '.html"', ' target="_PARENT">', plot_id, "</a>"))

# Select relevant columns to display
d_dt = plotproj |>
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
d_dt

formatJS = JS("function(settings, json) {",
    "$('body').css({'font-family': 'Arial'});",
    "}")

dt = datatable(d_dt, rownames = FALSE, escape = FALSE, options = list(paging = FALSE, scrollY = "100%",
                                                   initComplete = formatJS)) |>
  formatStyle(names(d_dt), lineHeight = '100%',
                        padding = '4px 15px 4px 15px')

dt

htmlwidgets::saveWidget(dt, file.path(OVERVIEW_DATA_DIR, "field-plot-data-table.html"))


#TODO: make 0 BA be NA (for FOCAL plots)

# TODO: constrain what columns are not escaped




# Make leaflet map of field data catalog

d_map = left_join(plot_centroids, plotproj, by = join_by("plot_id" == "plot_id"))

m = leaflet() %>%
  addMarkers(data = d_map, popup = ~plot_id_link) |>
  addPolygons(data = bounds, group = "bounds") |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
  groupOptions("bounds", zoomLevels = 13:20) |>
  addLayersControl(baseGroups = c("Topo", "Imagery"),
                   options = layersControlOptions(collapsed = FALSE))
m

htmlwidgets::saveWidget(m, file.path(OVERVIEW_DATA_DIR, "field-plot-map.html"))


# ---- Create individual plot pages (tree-level detail)
if (dir.exists(PLOT_DATA_PAGE_DIR)) unlink(PLOT_DATA_PAGE_DIR, recursive = TRUE)
dir.create(PLOT_DATA_PAGE_DIR)
file.create(file.path(PLOT_DATA_PAGE_DIR, "_index.md"))

if (dir.exists(PLOT_MAP_HTML_DIR)) unlink(PLOT_MAP_HTML_DIR, recursive = TRUE)
dir.create(PLOT_MAP_HTML_DIR)

if (dir.exists(PLOT_DATATABLE_HTML_DIR)) unlink(PLOT_DATATABLE_HTML_DIR, recursive = TRUE)
dir.create(PLOT_DATATABLE_HTML_DIR)


# Prep tree-level data
trees_vis = trees_clean |>
  # Bring in plot-level data
  left_join(plotproj, by = "plot_id") |>
  # If there is no DBH, use height for size plotting
  # TODO: Make this check whether the whole plot had height
  mutate(size = ifelse(is.na(dbh), height, dbh))


# --- Stem maps for each field plot

plot_foc = plotproj[100, ]

bound_foc = bounds |>
  filter(plot_id == plot_foc$plot_id)

trees_foc = trees_vis |>
  filter(plot_id == plot_foc$plot_id)

trees_prepped = st_as_sf(trees_foc, coords = c("tree_lon", "tree_lat"), crs = 4326) |>
  # largest trees on top
  arrange(-size)

pal = colorFactor("viridis", trees_prepped$sp_code)

rescale_size = function(x, min_size = 5, max_size = 20) {
  x = (x - min(x)) / (max(x) - min(x))
  x = x * (max_size - min_size) + min_size
  return(x)
}

m = leaflet() %>%
  addPolygons(data = bound_foc, group = "bounds") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery",
                   options = providerTileOptions(minZoom = 1, maxZoom = 20)) |>
  addCircleMarkers(data = trees_prepped,
                   stroke = FALSE,
                   fillOpacity = 1,
                   color = pal(trees_prepped$sp_code),
                   radius = rescale_size(trees_prepped$dbh, 5, 20)) |>
  hideGroup("Imagery") |>
  #groupOptions("bounds", zoomLevels = 13:20) |>
  addLayersControl(overlayGroups = c("Imagery"),
                   options = layersControlOptions(collapsed = FALSE)) |>
  addLegend(pal = pal, values = trees_prepped$sp_code, title = "Species", opacity = 1)
m

# Make background transparent
backg <- htmltools::tags$style(".leaflet-container { background: rgba(200,200,200,1) }")
m = prependContent(m, backg)
m


# -- Save map HTML to website repo

# Make a temp dir to save the table package to before pulling out the table HTML
tempdir = tempdir()
mapdir = file.path(tempdir, "map-staging")
if (dir.exists(mapdir)) unlink(mapdir, recursive = TRUE)
dir.create(mapdir)
mappath = file.path(mapdir, "map.html")

htmlwidgets::saveWidget(m, mappath, selfcontained = FALSE, background = "transparent")

# Extract the datatable HTML to save to the website repo
map_html = readLines(mappath) |>
  # Update the paths to the header files
  str_replace_all("map_files", "/leaflet-header-files")

# Concatenate lines to a single string with newlines
#dt_html = paste0(dt_html, collapse = "\n")

# Write table HTML to the website repo

writeLines(map_html, file.path(PLOT_MAP_HTML_DIR, paste0(plotfoc$plot_id, ".html")))

map_html_path = paste0("/field-plot-maps/", plotfoc$plot_id, ".html")



# TODO: allow interpolation of zoom level beyond the level provided by the data source:
# https://gis.stackexchange.com/questions/332823/scaling-tiles-for-missing-zoom-levels-in-leaflet



# --- Tree-level data table prep


# Format numbers for display, e.g. rounding
d = plotproj |>
  mutate(plot_area_ha = round(plot_area_ha, 2),
         ba_ha = round(ba_ha, 0),
         min_ht = round(min_ht, 2),
         min_ht_ohvis = round(min_ht_ohvis, 2),
         min_dbh = round(min_dbh, 1))

plotfoc = d[1, ]

plotfoc_dt = plotfoc |>
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
  
formatJS = JS(
  "function(settings, json) {",
    "$('body').css({'font-family': 'Arial'});",
    "}"
)

# This is to remove the column header, but it conflicted with the initComplete call, and I was able
# to mostly deal with this by setting colnames to NULL and bSort to FALSE
headerCallbackJS = JS(
              "function(thead, data, start, end, display){",
              "  $(thead).remove();",
              "}")

dt = datatable(plotfoc_dt, rownames = FALSE, escape = TRUE,
               colnames = NULL,
               options = list(paging = FALSE, scrollY = "100%",
                              dom = 't',
                              bSort = FALSE,
                              autoWidth = TRUE,
                              columnDefs = list(list(width = '40%', targets = "Attribute")),
                              #headerCallback = headerCallbackJS,
                              initComplete = formatJS))
dt

dt$sizingPolicy$browser$padding = 0
dt$sizingPolicy$browser$fill = FALSE


# Make a temp dir to save the table package to before pulling out the table HTML
tempdir = tempdir()
tabledir = file.path(tempdir, "table-staging")
if (dir.exists(tabledir)) unlink(tabledir, recursive = TRUE)
dir.create(tabledir)
tablepath = file.path(tabledir, "table.html")

htmlwidgets::saveWidget(dt, tablepath, selfcontained = FALSE)

# Extract the datatable HTML to save to the website repo
dt_html = readLines(tablepath) |>
  # Update the paths to the header files
  str_replace_all("table_files", "/datatable-header-files") |>
  str_replace_all("table_files", "/datatable-header-files")

# Concatenate lines to a single string with newlines
#dt_html = paste0(dt_html, collapse = "\n")

# Write table HTML to the website repo

writeLines(dt_html, file.path(PLOT_DATATABLE_HTML_DIR, paste0(plotfoc$plot_id, ".html")))

datatable_html_path = paste0("/field-plot-datatables/", plotfoc$plot_id, ".html")

# --- Template the plot page

library(jinjar)

template_file = fs::path(file.path("sandbox", "field-ref-catalog", "templates", "field-ref-plot.md"))

rendered = jinjar::render(template_file,
                          plot_id = plotfoc$plot_id,
                          map_html_path = map_html_path,
                          datatable_html_path = datatable_html_path)
rendered

# Write rendered markdown page
# # To this repo's sandbox
# write_path = file.path("sandbox", "field-ref-catalog", "rendered-pages",
#                        paste0(plotfoc$plot_id, ".md"))
# To the Hugo site


write_path = file.path(PLOT_DATA_PAGE_DIR, paste0(plotfoc$plot_id, ".md"))


writeLines(rendered, write_path)














## OLD: extract datatable headers

# # List table supporting files and copy to website repo static folder

# table_files_rel = list.files(file.path(tabledir, "table_files"), include.dirs = TRUE)
# table_files_abs = file.path(tabledir, "table_files", table_files_rel)

# file.copy(table_files_abs,
#           FIELDREF_PLOT_DATA_TABLE_SUPPORTING_FILES_DIR, recursive = TRUE)

# # Extract the table HTML header that links in these files, and sub in the new paths

# table_html = readLines(tablepath)
# first_line = grep("<style>", table_html)[1]
# last_line = grep("</style>", table_html)[1]

