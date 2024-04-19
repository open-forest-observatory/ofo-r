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
PLOT_DATA_PAGE_DIR = "~/repos/ofo-website-3/content/data/field-ref-plot/"

BASE_OFO_URL = "https://openforestobservatory.netlify.app"




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


# -- Make a HTML data table of plot attributes

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
d = plotproj |>
  mutate(plot_id = paste0('<a href="', BASE_OFO_URL, "data/field/plot/", plot_id, ".html", '">', plot_id, "</a>"))

# Select relevant columns to display
d = d |>
  select("ID" = plot_id,
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
d

formatJS = JS("function(settings, json) {",
    "$('body').css({'font-family': 'Arial'});",
    "}")

dt = datatable(d, rownames = FALSE, escape = FALSE, options = list(paging = FALSE, scrollY = "100%",
                                                   initComplete = formatJS)) |>
  formatStyle(names(d), lineHeight = '100%',
                        padding = '4px 15px 4px 15px')



dt

htmlwidgets::saveWidget(dt, file.path(OVERVIEW_DATA_DIR, "field-plot-data-table.html"))


#TODO: make 0 BA be NA (for FOCAL plots)


dt = datatable(d, rownames = FALSE, escape = FALSE)
dt

# TODO: constrain what columns are not escaped




# Make leaflet map of all field plots
m = leaflet() %>%
  addMarkers(data = plot_centroids, popup = ~plot_id) |>
  addPolygons(data = bounds, group = "bounds") |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
  groupOptions("bounds", zoomLevels = 13:20) |>
  addLayersControl(baseGroups = c("Topo", "Imagery"),
                   options = layersControlOptions(collapsed = FALSE))
m

htmlwidgets::saveWidget(m, file.path(OVERVIEW_DATA_DIR, "field-plot-map.html"))


# ---- Create individual plot pages (tree-level detail)

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

###!!!! Export map here


# TODO: allow interpolation of zoom level beyond the level provided by the data source:
# https://gis.stackexchange.com/questions/332823/scaling-tiles-for-missing-zoom-levels-in-leaflet



# --- Template the plot page

library(jinjar)

template_file = fs::path(file.path("sandbox", "field-ref-catalog", "templates", "field-ref-plot.md"))

# Format numbers for display, e.g. rounding
d = plotproj |>
  mutate(plot_area_ha = round(plot_area_ha, 2),
         ba_ha = round(ba_ha, 0),
         min_ht_ohvis = round(min_ht_ohvis, 2),
         min_dbh = round(min_dbh, 1))

plotfoc = d[1, ]

plotfoc = plotfoc |>
  # Select just what's needed for a datatable
  select("Plot ID" = plot_id,
        "Project ID" = project_id,
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
        "Contributor plot ID" = contributor_plot_id) |>
  # Pivot longer
  mutate(across(everything(), as.character)) |>
  tidyr::pivot_longer(cols = everything(), names_to = "Attribute", values_to = "Value")

formatJS = JS("function(settings, json) {",
    "$('body').css({'font-family': 'Arial'});",
    "}")

headerCallbackJS = JS(
              "function(thead, data, start, end, display){",
              "  $(thead).remove();",
              "}")


dt = datatable(plotfoc, rownames = FALSE, escape = FALSE,
               options = list(paging = FALSE, scrollY = "100%",
                              dom = 't',
                              autoWidth = TRUE,
                              columnDefs = list(list(width = '50%', targets = "_all")),
                              initComplete = formatJS,
                              headerCallback = headerCallbackJS))
dt

# Make a temp dir to save the table to
tempdir = tempdir()
tabledir = file.path(tempdir, "table-staging")
dir.create(tabledir)
tablepath = file.path(tabledir, "table.html")
tablepath

htmlwidgets::saveWidget(dt, tablepath, selfcontained = FALSE)

# Next: For the first plot, put the table files in static,
# Then from the table HTML, extract the text from between the body tags, then put it in the markdown
# page via a jinjar template





dt = datatable(d, rownames = FALSE, escape = FALSE) |>
  formatStyle(names(d), lineHeight = '100%',
                        padding = '4px 15px 4px 15px')


rendered = jinjar::render(template_file, another_param = "dd")
rendered

# Write rendered markdown page
# To this repo's sandbox
write_path = file.path("sandbox", "field-ref-catalog", "rendered-pages",
                               paste0(plotfoc$plot_id, ".md"))
# To the Hugo site
write_path = file.path(PLOT_DATA_PAGE_DIR, plotfoc$plot_id, "index.md")


writeLines(rendered, write_path)
