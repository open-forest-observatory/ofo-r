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



# Read in and prep drone plot data

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
# # Manually remove the lamping plots
# plots_w_imagery = plots_w_imagery[!plots_w_imagery %in% c(82:87)]
# Manually add the SSI plot because we know it's good, we just didn't have the imagery in the right
# place before
plots_w_imagery = c(plots_w_imagery, 52)


# Get the details of the plots that are covered by drone imagery
field_summ_w_imagery = field_summ |>
  filter(field_plot_id %in% plots_w_imagery)


# Plot the plots along DBH, TPH, and plot area axes

fig = ggplot(field_summ, aes(y = dbh_mean, x = tph, size = plot_area, color = project_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = dbh_mean - dbh_cv * 3, ymax = dbh_mean + dbh_cv * 3), width = 0.0, size = 0.5) +
  geom_point(data = field_summ_w_imagery, color = "black", pch = 1) +
  scale_size_continuous(range = c(1.5, 6)) +
  theme_bw(15) +
  scale_color_viridis_d(option = "magma", begin = 0.3, end = 0.9) +
  labs(size = "Plot area (ha)", x = "Trees per hectare", y = "Mean DBH (cm)")
fig
png(file.path(datadir_field, "field-plot-summaries", "field-plots_structure-scatterplot.png"), width = 12, height = 8, units = "in", res = 300)
fig
dev.off()


# Repeat but with only the plots that are covered by drone imagery

fig = ggplot(field_summ_w_imagery, aes(y = dbh_mean, x = tph, size = plot_area, color = project_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = dbh_mean - dbh_cv * 3, ymax = dbh_mean + dbh_cv * 3), width = 0.0, size = 0.5) +
  scale_size_continuous(range = c(1.5, 6)) +
  theme_bw(15) +
  scale_color_viridis_d(option = "magma", end = 0.9) +
  labs(size = "Plot area (ha)", x = "Trees per hectare", y = "Mean DBH (cm)")
fig

png(file.path(datadir_field, "field-plot-summaries", "field-plots_w-ht_w-imagery_structure-scatterplot.png"), width = 12, height = 8, units = "in", res = 300)
fig
dev.off()


## Get a list of all the plots with imagery, along with their relevant details (including drone imagery params)

field_plots_w_imagery_save = field_summ_w_imagery |>
  select(field_plot_id, project_name, survey_year, plot_area, min_dbh, min_ht, min_ht_ohvis, num_ohvis_trees_excluded,  tph, dbh_mean, dbh_cv, forest_type, top_species, contributor_field_plot_id = contributor_plot_id)

# Bring in the drone imagery details
field_drone_selected = field_drone |>
  select(field_plot_id, drone_dataset_id, drone_imagery_year = year)
field_drone_selected = st_drop_geometry(field_drone_selected)

field_plots_w_imagery_save = left_join(field_plots_w_imagery_save, field_drone_selected, by = "field_plot_id")

# Condense the comma-separated list of imagery years into only unique years
field_plots_w_imagery_save = field_plots_w_imagery_save |>
  mutate(drone_imagery_year = str_split(drone_imagery_year, ",") |> map_chr(~ str_c(unique(.x), collapse = ",")))

# Write it
write_csv(field_plots_w_imagery_save, file.path(datadir_field, "field-plot-summaries", "field-plots_w-ht_w-imagery.csv"))

# Get a shapefile of the plot centroids
field_centers = st_centroid(field_bounds)

field_plot_locs = inner_join(field_centers, field_summ_w_imagery, by = "field_plot_id")

st_write(field_plot_locs, file.path(datadir_field, "field-plot-summaries", "field-plots _w-ht_w-imagery.gpkg"), delete_dsn = TRUE)



# Make a map figure of the field plots with imagery

ofo_locations <- 
  field_plot_locs |>
  sf::st_transform(3857)

ofo_extent <-
  sf::st_bbox(ofo_locations) |>
  sf::st_as_sfc() |>
  sf::st_as_sf() |>
  sf::st_buffer(dist = 200000) |>
  sf::st_bbox() |>
  sf::st_as_sfc() |>
  sf::st_as_sf()

states <-
  rnaturalearth::ne_states(country = "United States of America", 
                            returnclass = "sf") |>
  sf::st_transform(3857) |>
  sf::st_intersection(ofo_extent)
  
  
map = ggplot() +
  geom_sf(data = states) +
  geom_sf(data = ofo_locations, aes(color = project_name)) +
  scale_color_viridis_d(option = "magma", end = 0.9) +
  theme_bw(15)
map

png(file.path(datadir_field, "field-plot-summaries", "field-plots_w-ht_w-imagery_map.png"), width = 12, height = 8, units = "in", res = 300)
map
dev.off()
