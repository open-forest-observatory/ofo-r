# Purpose: Demonstrate function for identifying the optimal x, y, z shift to align two (simulated)
# tree maps

# Setup
devtools::load_all()

library(tidyverse)

# ---- Simulate a "predicted" and "observed" tree map
sim = simulate_tree_maps(trees_per_ha = 300, trees_per_clust = 5, cluster_radius = 5,
                         obs_extent = 60,
                         pred_extent = 300,
                         horiz_jitter = 3,
                         vert_jitter = 5, # max of 5
                         false_pos = 0.25,
                         false_neg = 0.50,
                         drop_observed_understory = TRUE,
                         shift_x = -9.25,
                         shift_y = 15.5)

print(nrow(sim$obs))
vis2(sim$pred, sim$obs)

obj_mean_dist_to_closest(sim$pred, sim$obs)

search_result = find_best_shift(sim$pred, sim$obs)

print(search_result)


# Try different random tree maps and see how often the shift is recovered
set.seed(123)
result = calc_alignment_success_rate(n_tries = 4, # set to at least 100 except for testing
                                     parallel = FALSE,
                                     trees_per_ha = 300,
                                     trees_per_clust = 5,
                                     cluster_radius = 5,
                                     obs_extent = 60,
                                     horiz_jitter = 3,
                                     vert_jitter = 5, # max of 5
                                     height_bias = 0,
                                     false_pos = 0.25,
                                     false_neg = 0.50,
                                     drop_observed_understory = TRUE)
result


# Across a range of stem map parameters, calculate the recovery rates

# trees_per_ha = c(150, 300)
# cluster_radius = c(3, 5, 7, 9)
# obs_extent = c(30, 60, 90)
# vert_jitter = c(1, 3)
# horiz_jitter = c(1)
# false_pos = c(0.6)
# false_neg = c(0.2, 0.6)
# drop_observed_understory = c(TRUE, FALSE)

trees_per_ha = c(100, 300, 500)
cluster_radius = c(3, 5, 7, 9)
obs_extent = c(30, 60, 90)
vert_jitter = c(1, 3, 5)
horiz_jitter = c(1, 3, 5, 7)
false_pos = c(0.2, 0.4, 0.6)
false_neg = c(0.2, 0.4, 0.6)
drop_observed_understory = c(TRUE)

stem_params = tidyr::expand_grid(trees_per_ha,
                          cluster_radius,
                          obs_extent,
                          vert_jitter,
                          horiz_jitter,
                          false_pos,
                          false_neg,
                          drop_observed_understory)
tictoc::tic()
future::plan(future::multisession, workers = parallelly::availableCores())
result = furrr::future_pmap(stem_params, calc_alignment_success_rate, parallel = TRUE, n_tries = 50,
                    method = "grid", return_summary = TRUE)
future::plan(future::sequential)
tictoc::toc()

result = dplyr::bind_rows(result)
result


alignment_results = dplyr::bind_cols(stem_params, result)

write.csv(alignment_results, "/ofo-share/scratch-derek/algnment-results_grid_dist-to-nearest.csv", row.names = FALSE)


d = alignment_results |>
  filter(n_trials > 40,
         vert_jitter == 3,
         cluster_radius == 5) |>
  mutate(trees_per_ha = as.factor(trees_per_ha))

ggplot(d, aes(x = horiz_jitter, y = recovery_rate, linetype = as.factor(obs_extent), color = trees_per_ha)) +
  facet_grid(false_pos ~ false_neg) +
  geom_point() +
  geom_line()



