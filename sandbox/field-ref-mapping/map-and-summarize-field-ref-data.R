# Purpose: Map the locations of field reference data plots, and make a table summarizing the data
# TODO: Need to make sure there are field plot bounds for all plots with tabular data and vice-versa

devtools::load_all()
library(tidyverse)
library(sf)
library(readxl)

datadir = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt")

projects = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-projects")
plots = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-plots")
trees = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "field-trees")
species = read_excel(file.path(datadir, "field-reference-data.xlsx"), sheet = "species-codes")

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

st_write(centers, file.path(datadir, "field-plot-summaries", "field-plot-centers.gpkg"), delete_dsn = TRUE)
st_write(boundaries, file.path(datadir, "field-plot-summaries", "field-plot-boundaries_v2.gpkg"), delete_dsn = TRUE)
st_write(trees, file.path(datadir, "field-plot-summaries", "field-trees.gpkg"), delete_dsn = TRUE)



## Summarize the trees at each plot to get structure and comp stats
trees_tabular = trees
st_geometry(trees_tabular) = NULL

# If DBH not measured, estimate from height
trees_tabular = trees_tabular |>
  filter((is.na(dbh) | dbh < 500) & (is.na(height) | height < 100)) |>
  mutate(dbh_measured = dbh) |>
  mutate(dbh_allometric = 2.85 + 1.82 * height,
         dbh = ifelse(is.na(dbh_measured), dbh_allometric, dbh_measured))

plot_summ = trees_tabular |>
  filter(!is.na(dbh) & dbh > 25.4) |>
  group_by(plot_id) |>
  summarize(dbh_mean = mean(dbh) |> round(1),
            dbh_sd = sd(dbh) |> round(1),
            dbh_cv = (sd(dbh) / mean(dbh)) |> round(2),
            n_trees = n(),
            height_measured = (sum(!is.na(height)) / n()) > 0.9)

trees_tabular = left_join(plot_summ, trees_tabular, by = "plot_id") |>
  rename(n_trees_plot = n_trees)


top_species = trees_tabular |>
  filter(!is.na(dbh) & dbh > 25.4) |>
  filter(!is.na(species)) |>
  mutate(species = as.character(species)) |>
  group_by(plot_id, species) |>
  summarize(n_trees_sp = n(),
            n_trees_plot = median(n_trees_plot)) |>
  mutate(prop_trees_sp = n_trees_sp / n_trees_plot) |>
  mutate(prop_trees_sp = round(prop_trees_sp, 2) * 100) |>
  group_by(plot_id) |>
  arrange(plot_id, desc(n_trees_sp)) |>
  slice_head(n = 3)

species = species |>
  mutate(code_numeric = as.character(code_numeric)) |>
  mutate(sp_code = ifelse(!is.na(code_usda), code_usda, code_supp))

top_species = top_species |>
  left_join(species, by = join_by(species == code_numeric))

# Merge species USDA code and proportion

top_species = top_species |>
  mutate(sp_w_prop = paste0(prop_trees_sp, "-", sp_code)) |>
  group_by(plot_id) |>
  summarize(top_species = paste(sp_w_prop, collapse = ","))

plot_summ = left_join(plot_summ, top_species, by = "plot_id")

# Bring in plot-level attributes: project, contributor plot id, plot area, year

plots_foc = plots |>
  mutate(survey_year = str_sub(survey_date, 1, 4),
         plot_area = round(plot_area / 10000, 2))

projects_foc = projects |>
  select(project_id, project_name = name)

plots_foc = left_join(plots_foc, projects_foc, by = "project_id")

plot_summ = left_join(plot_summ, plots_foc, by = "plot_id")

plot_summ = plot_summ |>
  mutate(tph = round(n_trees / plot_area)) |>
  select(plot_id, project_name, contributor_plot_id, survey_year, plot_area, height_measured, includes_snags, min_dbh, min_ht, min_ht_ohvis, num_ohvis_trees_excluded, tph, dbh_mean, dbh_sd, dbh_cv, forest_type, top_species)



# Write summarized plot data to file
write_csv(plot_summ, file.path(datadir, "field-plot-summaries", "field-plot-summaries.csv"))
