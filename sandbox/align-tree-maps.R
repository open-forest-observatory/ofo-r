# Purpose: Demonstrate function for identifying the optimal x, y, z shift to align two (simulated)
# tree maps

# Setup

library(spatstat)
library(ggplot2)
library(dplyr)
set.seed(1)

# --- Visualization functions
# Visualize a hypothetical tree map
vis1 = function(trees) {
  p = ggplot(trees, aes(x, y, size = z, color = z)) +
    geom_point() +
    scale_size_continuous(range = c(0.5, 3)) +
    scale_color_viridis_c(end = 0.85) +
    theme_bw() +
    coord_cartesian(xlim = c(-200, 200), ylim = c(-200, 200))

  print(p)
}

# Visualize two hypothetica tree maps (predicted and observed) overlaid. obs_foc: zoom in to focus
# on the observed trees, with some buffer. Assumes the observed tree map is 50 x 50 and that both
# tree maps are centered at 0,0.
vis2 = function(pred, obs, obs_foc = FALSE) {
  pred = pred |> mutate(layer = "predicted")
  obs = obs |> mutate(layer = "observed")

  trees = bind_rows(pred, obs)

  lim = ifelse(obs_foc, 100, 200)

  p = ggplot(trees, aes(x, y, size = z, color = layer)) +
    geom_point() +
    scale_size_continuous(range = c(0.5, 3)) +
    theme_bw() +
    coord_cartesian(xlim = c(-lim, lim), ylim = c(-lim, lim))

  print(p)
}


# ---- Simulate a "predicted" and "observed" tree map

# Generate random, slightly clustered points in x-y space, over an area wider than would reasonably
# have a field stem map (to represent the drone-based tree predictions). Interpret the x-y coords as
# meters. These params make a 16 ha stem map with 240 trees/ha.
pred = rMatClust(kappa = 0.005, scale = 25, mu = 5, win = as.owin(c(-200, 200, -200, 200)))
pred = as.data.frame(pred)
# Add random heights from 5 to 50 m, and sort so smallest appear on top
pred$z = runif(nrow(pred), 5, 50)
pred = pred[order(-pred$z), ]

# Visualize the map
vis1(pred)

# Take a spatial subset of these points, to represent the "observed" tree map (e.g. a 100 m x 100 m
# field plot)
obs = pred |>
  filter(between(x, -50, 50) & between(y, -50, 50))

# Visualize the map
vis1(obs)

# Vis the two overlaid
vis2(pred, obs)

# Remove 25% of the trees from each map, to simulate false positives and false negatives
pred = pred |>
  sample_frac(0.75)
obs = obs |>
  sample_frac(0.75)

# Add some noise to the predicted tree heights
pred$z = pred$z + runif(nrow(pred), -5, 5)

# Shift the "observe" tree map slightly to help visualize the slight differences and the alignment challenge
obs_offset = obs |>
  mutate(x = x + 2, y = y + 2)

vis2(pred, obs_offset, obs_foc = TRUE)

# Shift obs a known amount that we will attempt to recover
obs = obs |>
  mutate(x = x + 12, y = y - 23)

vis2(pred, obs, obs_foc = TRUE)


obj_mean_dist_to_closest(pred, obs)

best_shift = find_best_shift(pred, obs, method = "optim")
print(best_shift)
