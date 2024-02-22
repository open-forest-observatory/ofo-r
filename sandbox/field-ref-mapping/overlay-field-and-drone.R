# Purpose: Identify field plots that are covered by drone imagery

devtools::load_all()
library(tidyverse)
library(sf)
library(readxl)

datadir = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt")
datadir_imagery = readLines("sandbox/data-dirs/derek-map-imagery-laptop.txt")




# Write summarized plot data to file
write_csv(plot_summ, file.path(datadir, "field-plot-summaries", "field-plot-summaries.csv"))





# Manually exclude Johnston plots which are not accurate yet, and some others that are not
# exhaustive plots or don't include height
plot_summ = plot_summ |>
  filter(tph < 5000) |>
  filter(!(contributor_plot_id %in% c("Chips_1_ABCO"))) |> # not exhaustive
  filter(height_measured)










st_write(centers, file.path(datadir, "field-plot-summaries", "field-plot-centers.gpkg"), delete_dsn = TRUE)
st_write(boundaries, file.path(datadir, "field-plot-summaries", "field-plot-boundaries_v2.gpkg"), delete_dsn = TRUE)
st_write(trees, file.path(datadir, "field-plot-summaries", "field-trees.gpkg"), delete_dsn = TRUE)




# ---- Determine whether covered by drone imagery ----

# Load the drone imagery polygons
imagery_polys = st_read(file.path(datadir_imagery, "dataset-polys", "dataset-polys_v1.gpkg"))

# Consolidate the imagery polygons to 120m nadir imagery, one polygon per location

imagery_polys = imagery_polys |>
  filter(altitude > 95 & altitude < 145 & pitch < 8)



#### RESUME HERE

parts <- st_cast(st_union(imagery_polys),"POLYGON")

clust <- unlist(st_intersects(imagery_polys, parts))

flattened <- cbind(imagery_polys, clust) |>
  group_by(clust) |>
  summarize(dataset_id = paste(dataset_id, collapse = ","),
            altitude = paste(altitude, collapse = ","),
            pitch = paste(pitch, collapse = ","))

st_write(flattened, file.path(datadir, "temp", "imagery-polys-flattened.gpkg"), delete_dsn = TRUE)
st_write(imagery_polys, file.path(datadir, "temp", "imagery-polys-unflattened.gpkg"), delete_dsn = TRUE)



## NOTE: need to get imagery year









# Plot the plots along DBH, TPH, and plot area axes

ggplot(plot_summ, aes(y = dbh_mean, x = tph, size = plot_area, color = project_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = dbh_mean - dbh_cv * 3, ymax = dbh_mean + dbh_cv * 3), width = 0.0, size = 0.5) +
  scale_size_continuous(range = c(1.5, 6)) +
  theme_bw(15) +
  labs(size = "Plot area (ha)", x = "Trees per hectare", y = "DBH (cm)")

