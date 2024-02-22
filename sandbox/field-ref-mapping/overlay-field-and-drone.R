# Purpose: Identify field plots that are covered by drone imagery

devtools::load_all()
library(tidyverse)
library(sf)

datadir_field = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt")
datadir_imagery = readLines("sandbox/data-dirs/derek-map-imagery-laptop.txt")




# Read in and prep field plot data

# Tabular
field_summ = read_csv(file.path(datadir_field, "field-plot-summaries", "field-plot-summaries.csv"))

# Manually exclude OSU Johnston plots which are not accurate yet, and some others that are not
# exhaustive plots or don't include height
field_summ = field_summ |>
  mutate(field_plot_id = plot_id) |>
  filter(tph < 5000) |>
  filter(!(contributor_plot_id %in% c("Chips_1_ABCO"))) |> # not exhaustive  # TODO: add back: 20230709-0199,20230709-0201,20230710-0205,20230710-0206,20230710-0207
  filter(height_measured)

# Spatial
field_bounds = st_read(file.path(datadir_field, "field-plot-summaries", "field-plot-boundaries_v2.gpkg")) |>
  select(field_plot_id = plot_id)



# Reach in and prep drone plot data

imagery_polys = st_read(file.path(datadir_imagery, "dataset-polys", "dataset-polys_v2.gpkg"))

imagery_polys = imagery_polys |>
  rename(drone_dataset_id = dataset_id)

# Consolidate the imagery polygons to 120m nadir imagery, one polygon per location

imagery_polys = imagery_polys |>
  filter(altitude > 95 & altitude < 145 & pitch < 8)

parts <- st_cast(st_union(imagery_polys), "POLYGON")

clust <- unlist(st_intersects(imagery_polys, parts))

imagery_polys_flat <- cbind(imagery_polys, clust) |>
  group_by(clust) |>
  summarize(drone_dataset_id = paste(drone_dataset_id, collapse = ","),
            altitude = paste(altitude, collapse = ","),
            pitch = paste(pitch, collapse = ","),
            year = paste(year_annotated, collapse = ","))



# ---- Determine whether field plots are covered by drone imagery ----

field_drone_idx = st_within(field_bounds |> st_transform(st_crs(imagery_polys_flat)), imagery_polys_flat, sparse = FALSE)

# Get the first drone dataset_id that covers each field plot (should be only one anyway)
field_drone_idx = apply(field_drone_idx, 1, function(x) which(x)[1])

field_drone = bind_cols(field_bounds, imagery_polys_flat[field_drone_idx, ] |> st_drop_geometry())


# Get the IDs of the field plots that are covered by drone imagery
plots_w_imagery = field_drone$field_plot_id[!is.na(field_drone$drone_dataset_id)]
# Manually remove one plot that is just at the edge of imagery
plots_w_imagery = plots_w_imagery[!plots_w_imagery %in% c(114)]
# Manually remove the lamping plots
plots_w_imagery = plots_w_imagery[!plots_w_imagery %in% c(82:87)]
# Manually add the SSI plot because we know it's good, we just didn't have the imagery in the right
# place before
plots_w_imagery = c(plots_w_imagery, 52)


# Get the details of the plots that are covered by drone imagery
field_summ_w_imagery = field_summ |>
  filter(field_plot_id %in% plots_w_imagery)


# Plot the plots along DBH, TPH, and plot area axes

ggplot(field_summ, aes(y = dbh_mean, x = tph, size = plot_area, color = project_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = dbh_mean - dbh_cv * 3, ymax = dbh_mean + dbh_cv * 3), width = 0.0, size = 0.5) +
  geom_point(data = field_summ_w_imagery, color = "black", pch = 1) +
  scale_size_continuous(range = c(1.5, 6)) +
  theme_bw(15) +
  labs(size = "Plot area (ha)", x = "Trees per hectare", y = "Mean DBH (cm)")


# Repeat but with only the plots that are covered by drone imagery

ggplot(field_summ_w_imagery, aes(y = dbh_mean, x = tph, size = plot_area, color = project_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = dbh_mean - dbh_cv * 3, ymax = dbh_mean + dbh_cv * 3), width = 0.0, size = 0.5) +
  scale_size_continuous(range = c(1.5, 6)) +
  theme_bw(15) +
  scale_color_viridis_d(option = "magma", end = 0.9) +
  labs(size = "Plot area (ha)", x = "Trees per hectare", y = "Mean DBH (cm)")


## Get a list of all the plots with imagery, along with their relevant details

# Get a shapefile of the plot centers
