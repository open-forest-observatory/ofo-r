# Purpose: Create the Hugo markdown pages for all field reference sites, as well as a dataset index
# page

library(dplyr)
library(googlesheets4)
library(leaflet)
library(stringr)
library(sf)
library(htmlwidgets)

datadir = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt", n = 1)


# Load field ref data (tabular)
projects = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "field-projects")
plots = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "field-plots")
subplots = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "field-subplots")
trees = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "field-trees")
species_codes = read_sheet("1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4", sheet = "species-codes")


# --- Data cleaning and prep

# -- Compiling and cleaning plot boundaries

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

# Standardize boundary plot ID formatting
bounds = bounds |>
  mutate(plot_id = str_pad(plot_id, 4, pad = "0", side = "left"))

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

# --- Processing

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

htmlwidgets::saveWidget(m, file.path(datadir, "temp", "field-plot-map.html"))
