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


# Prep predicted and observed tree maps for comparison
obs = prep_obs_map(obs = sim$obs, obs_bound = sim$obs_bound, edge_buffer = 5)
pred = prep_pred_map(pred = sim$pred, obs_bound = sim$obs_bound, edge_buffer = 5)

# Match predicted and observed trees

obs_matched = match_obs_to_pred(obs, pred,
                                search_distance_fun_intercept = 1,
                                search_distance_fun_slope = 0.1,
                                search_height_proportion = 0.5)



### Compute match stats

obs_simple = obs_matched |>
  dplyr::select(observed_tree_id,
                matched_predicted_tree_id,
                observed_tree_height = z,
                observed_tree_core_area = core_area) |>
  mutate(observed_tree_core_area = as.vector(observed_tree_core_area)) |>
  mutate(matched_predicted_tree_id = as.numeric(matched_predicted_tree_id)) #TODO: fix this type inconsistency further upstream. Where is ID getting stored as a character?

pred_simple <- pred |>
  dplyr::select(predicted_tree_id,
                predicted_tree_height = z,
                predicted_tree_core_area = core_area) |>
  mutate(predicted_tree_core_area = as.vector(predicted_tree_core_area))

# For each predicted tree, get the attributes of the observed tree it was matched to. Same for
# observed trees matched to predicted trees (two separate tables). It is necessary to have these two
# separate tables because when assessing sensitivity (recall), we need to check how many of the
# observed trees *within the internal negative buffered area* match to predicted trees *regardless
# of whether they're in the internal negative buffered area*, and similarly for precision, we need
# to check how many of the predicted trees *within the internal negative buffered area* match to
# observed trees *regardless of whether they're in the internal negative buffered area*, so the sets
# of trees used for each is different. Before joining observed and predicted trees, remove geometry
# (convert to regular data frame) because coordinates are not necessary, and two spatial data frames
# with different geometry cannot be left_joined as it is ambiguous which one to take the coordinates
# from
sf::st_geometry(obs_simple) <- NULL
sf::st_geometry(pred_simple) <- NULL

pred_obs_match <- left_join(pred_simple,
                            obs_simple,
                            by = c("predicted_tree_id" = "matched_predicted_tree_id")
)

obs_pred_match <- right_join(pred_simple,
                             obs_simple,
                             by = c("predicted_tree_id" = "matched_predicted_tree_id")
)


# Sum the tree counts (number of predicted trees, number of predicted trees matched, number of
# observed trees, and number of observed trees matched) across height classes

over10_match = count_total_and_matched_trees(obs_pred_match, pred_obs_match, min_height = 10)

## RESUME HERE. Can prob remove most of height accuracy metrics.

# Get height accuracy metrics based on a table of heights of the observed trees and the heights of the matched predicted trees
height_metrics_over10 <- height_accuracy_metrics(trees_matched = trees_matched %>% filter(observed_tree_internal_area == TRUE), min_height = 10)
height_metrics_over20 <- height_accuracy_metrics(trees_matched = trees_matched %>% filter(observed_tree_internal_area == TRUE), min_height = 20)

# Bind the two height categories and compute height MAE for both
height_stats <- bind_rows(height_metrics_over10, height_metrics_over20) %>%
  mutate(height_mae_percent = height_mae / height_mean_observed)

# Compute sensitivity, precision, f_score for individual tree detection
match_stats <- match_stats %>%
  mutate(sensitivity = n_observed_matched_predicted / n_observed,
          precision = n_predicted_matched_observed / n_predicted) %>%
  mutate(f_score = 2 * sensitivity * precision / (sensitivity + precision))

# Combine individual tree detection and height accuracy
match_stats <- full_join(match_stats, height_stats, by = "height_cat")
match_stats$canopy_position <- canopy_position
