# Purpose: Prepare functions for tree map alignment using an objective function based on MEE-based
# matching

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


# Prep predicted and observed tree maps for comparison
obs = prep_obs_map(obs = sim$obs, obs_bound = sim$obs_bound, edge_buffer = 5)
pred = prep_pred_map(pred = sim$pred, obs_bound = sim$obs_bound, edge_buffer = 5)

# Match predicted and observed trees, following logic in MEE paper

obs_matched = match_obs_to_pred_mee(obs, pred,
                                search_distance_fun_intercept = 1,
                                search_distance_fun_slope = 0.1,
                                search_height_proportion = 0.5)


match_stats = compute_match_stats(pred, obs_matched)
match_stats
