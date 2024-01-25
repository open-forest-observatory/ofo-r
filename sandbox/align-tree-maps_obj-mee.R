# Purpose: Prepare functions for tree map alignment using an objective function using MEE_based
# alignemnt

# Setup
devtools::load_all()

library(tidyverse)

# ---- Simulate a "predicted" and "observed" tree map
sim = simulate_tree_maps(trees_per_ha = 300, trees_per_clust = 5, cluster_radius = 5,
                         obs_extent = 90,
                         horiz_jitter = 3,
                         vert_jitter = 5, # max of 5
                         false_pos = 0.25,
                         false_neg = 0.50,
                         drop_observed_understory = TRUE,
                         shift_x = -9.25,
                         shift_y = 15.5)

vis2(sim$pred, sim$obs)

obs = sim$obs
pred = sim$pred

# Prep observed map

