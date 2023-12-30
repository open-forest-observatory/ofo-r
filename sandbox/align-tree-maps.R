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
sim = simulate_tree_maps(trees_per_ha = 250, trees_per_clust = 5, cluster_radius = 25,
                   horiz_jitter = 1,
                   vert_jitter = 5, # max of 5
                   false_pos = 0.25,
                   false_neg = 0.25,
                   drop_understory = FALSE, # TODO
                   drop_small_thresh = 5) # TODO

vis2(sim$pred, sim$obs)

obj_mean_dist_to_closest(sim$pred, sim$obs)

tictoc::tic()
best_shift = find_best_shift(sim$pred, sim$obs)
tictoc::toc()

print(best_shift)


# Try different random tree maps and see if it works every time