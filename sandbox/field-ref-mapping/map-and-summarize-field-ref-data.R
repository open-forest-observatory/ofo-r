# Purpose: Map the locations of field reference data plots, and make a table summarizing the data

devtools::load_all()
library(tidyverse)
library(sf)
library(readxl)

datadir = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt")

projects = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-projects")
plots = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-plots")
trees = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-trees")

# TODO: check warnings on file read

# --- Load all plot boundaries into single object ---

boundary_files = list.files(file.path(datadir, "field-plot-boundaries"), full.names = TRUE, pattern = ".gpkg$")

boundaries = lapply(boundary_files, st_read)

# Make all columns character so they can be rbinded together
boundaries = lapply(boundaries, function(x) {
  mutate(x, across(-geom, as.character))
})

boundaries = bind_rows(boundaries)

boundaries$plot_id = case_when(
  !is.na(boundaries$ofo_plot_id) ~ as.character(boundaries$ofo_plot_id),
  !is.na(boundaries$plot_id) ~ as.character(boundaries$plot_id),
  !is.na(boundaries$plot_id_ofo) ~ as.character(boundaries$plot_id_ofo),
  TRUE ~ NA
)

boundaries = boundaries |>
  select(plot_id) |>
  mutate(plot_id = as.numeric(plot_id))

# --- Join the plot attributes to the boundaries ---

boundaries = left_join(boundaries, plots, by = "plot_id")

centers = st_as_sf(plots, coords = c("plot_lon", "plot_lat"), crs = 4326)

trees = st_as_sf(trees, coords = c("tree_lon", "tree_lat"), crs = 4326)

st_write(centers, file.path(datadir, "temp", "field-plot-centers.gpkg"))
st_write(boundaries, file.path(datadir, "temp", "field-plot-boundaries.gpkg"))
st_write(trees, file.path(datadir, "temp", "field-trees.gpkg"))
