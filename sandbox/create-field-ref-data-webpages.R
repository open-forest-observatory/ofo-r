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
  filter(!is.na(species)) |>
  mutate(species = as.character(species)) |>
  group_by(plot_id, species) |>
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

# Prepare species code data for joining
species_codes = species_codes |>
  mutate(code_numeric = as.character(code_numeric)) |>
  mutate(sp_code = ifelse(!is.na(code_usda), code_usda, code_supp))

top_species = top_species |>
  left_join(species_codes, by = join_by(species == code_numeric))

# Merge species USDA code and proportion into one column
# sp_w_prop_alt is a nicer but wider version of sp_w_prop
top_species = top_species |>
  mutate(sp_w_prop = paste0(prop_trees_sp, "-", sp_code),
         sp_w_prop_alt = paste0(sp_code, " (", prop_trees_sp, "%)")) |>
  group_by(plot_id) |>
  summarize(top_species = paste(sp_w_prop, collapse = ","),
            top_one_species = first(sp_w_prop_alt))


# -- General prep of plot-level data

# If plot-level min_dbh_ohvis is missing, use min_dbh
plots = plots |>
  mutate(min_ht_ohvis = ifelse(is.na(min_ht_ohvis), min_ht, min_ht_ohvis))


# -- Make a HTML data table of plot attributes

# Prep plot bounds data for merging
bounds_nosp = st_drop_geometry(bounds) |>
  select(plot_id, area_ha_sf)

# To the plot data, join project data, top-species data, tree-summary data, plot bounds area
plotproj = left_join(plots, projects, by = "project_id") |>
  left_join(top_species, by = "plot_id") |>
  left_join(tree_summ, by = "plot_id") |>
  left_join(bounds_nosp, by = "plot_id")

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
  
# Select relevant columns to display
d = plotproj |>
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

dt = datatable(d, rownames = FALSE, options = list(paging = FALSE, scrollY = "800px"))
dt

htmlwidgets::saveWidget(dt, file.path(OVERVIEW_DATA_DIR, "field-plot-data-table.html"))









# Compute plot centroids
plot_centroids = sf::st_centroid(bounds)

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