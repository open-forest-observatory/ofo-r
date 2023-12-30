## TODO: how much better is the best match vs the  mean of the alternatives (but only those where there is complete overlap between predicted and observed)? This could be a way to quantify the confidence in the alignment.
#### TODO: Make sure that the requested range of shifts is still completley within the predicted stem map.

# Functions for determining the optimal alignment between a predicted (e.g. drone-based) and
# observed (e.g. field-based) tree map.


## Create a simulated "predicted" and "observed" tree map
# Generate random, customizably clustered points in x-y space, over an area wider than would
# reasonably have a field stem map (to represent the drone-based tree predictions). Interpret the
# x-y coords as meters.
simulate_tree_maps = function(trees_per_ha = 250, trees_per_clust = 5, cluster_radius = 25,
                              pred_extent = 400,
                              obs_extent = 100,
                              horiz_jitter = 1,
                              vert_jitter = 5, # max of 5
                              false_pos = 0.25,
                              false_neg = 0.25,
                              drop_understory = TRUE,
                              height_bias = 0,
                              shift_x = -9.25,
                              shift_y = 15.5) {



  cluster_dens = trees_per_ha / trees_per_clust / 10000 # Converts from trees/ha to trees/m^2

  pred = spatstat.random::rMatClust(kappa = cluster_dens, scale = cluster_radius, mu = trees_per_clust,
                             win = spatstat.geom::as.owin(c(-pred_extent / 2,
                                                       pred_extent / 2,
                                                       -pred_extent / 2,
                                                       pred_extent / 2)))
  pred = as.data.frame(pred)

  # Assign the trees heights ranged 5 to 50 m, skewed towards the lower end
  heights = rnorm(10000, 5, 20)
  heights = heights[dplyr::between(heights,5,50)]
  pred$z = sample(heights, nrow(pred), replace = TRUE)
  # Order by decreasing height, so when plotting the smaller trees are on top
  pred = pred[order(-pred$z), ]

  # Take a spatial subset of these points, to represent the "observed" tree map (e.g. field
  # reference plot)
  obs = pred |>
    filter(dplyr::between(x, -obs_extent/2, obs_extent/2) & between(y, -obs_extent/2, obs_extent/2))

  # Randomly remove a fraction of the trees from each map, to simulate false positives and false negatives
  pred = pred |>
    dplyr::sample_frac(1 - false_neg)
  obs = obs |>
    dplyr::sample_frac(1 - false_pos)

  # Add some noise to the predicted tree x-y coords
  pred$x = pred$x + runif(nrow(pred), -horiz_jitter, horiz_jitter)
  pred$y = pred$y + runif(nrow(pred), -horiz_jitter, horiz_jitter)

  # If specified, remove understory trees
  # TODO: make this flexible to work with tree datasets that have only DBH and not height
  if (drop_understory) {
    pred = drop_understory_trees(pred)
    obs = drop_understory_trees(obs)
  }
  
  # Add some noise and bias to the predicted tree heights
  pred$z = pred$z + runif(nrow(pred), -vert_jitter, vert_jitter) + height_bias

  # If specified, remove small trees from the observed map, so we only base the alignment on the
  # large observed trees (matching to any size small trees)
  # TODO: This made it worse and does not accommodate dataset with only DBH so needs to be refined or
  # removed. 
  # obs = obs[obs$z > drop_small_thresh, ]

  # Shift obs a known amount that we will attempt to recover
  obs = obs |>
    dplyr::mutate(x = x + shift_x, y = y + shift_y)

  return(list(pred = pred, obs = obs))

}


# For each observed point, get the closest predicted point in x, y, z space
# (note: this is not a true distance, but is fine for our purposes)
get_closest_obs = function(obs, pred) {
  # Get the closest predicted point to each observed point, in x, y, z space
  closest = pred |>
    dplyr::group_by(x, y) |>
    dplyr::mutate(dist = sqrt((x - first(obs$x))^2 + (y - first(obs$y))^2 + (z - first(obs$z))^2)) |>
    dplyr::filter(dist == min(dist)) |>
    dplyr::ungroup()

  # Join back to the observed points
  obs = obs |>
    dplyr::left_join(closest, by = c("x", "y"))

  return(obs)
}

# Objective function: the mean x,y,z distance between each observed tree and its nearest predicted
# tree
obj_mean_dist_to_closest = function(pred, obs) {
  # For each predicted point, get the closest observed point in x, y, z space

  # Crop the predicted points to the x-y extent of the observed points +- 25%, so we don't waste
  # time computing distances to points that are definitely not the closest

  xmin = min(obs$x)
  ymin = min(obs$y)
  xmax = max(obs$x)
  ymax = max(obs$y)
  xrange = xmax - xmin
  yrange = ymax - ymin

  pred_crop = pred |>
    dplyr::filter(dplyr::between(x, xmin - xrange / 4, xmax + xrange / 4) &
                    dplyr::between(y, ymin - yrange / 4, ymax + yrange / 4))

  # For each observed point, get the distance to every predicted point
  # TODO: Is it OK that multiple observed points may be matched to the same predicted point? Maybe
  # keep it simple for the sake of speed, if it can recover the correct shift
  dists = sqrt(outer(obs$x, pred_crop$x, "-")^2 +
                 outer(obs$y, pred_crop$y, "-")^2 +
                 outer(obs$z, pred_crop$z, "-")^2)

  # For each observed point, get the distance to the nearest predicted point
  min_dists = apply(dists, 1, min)

  # Summarize the alignment as the mean distance to the nearest predicted point
  objective = mean(min_dists)

  # Any values over 10 don't make sense
  # objective = pmin(objective, 10)

  return(objective)
}

# Test a given x-y shift and compute the objective function for it, using the objective
# function supplied to it
# transform_params: contains (in order): x_shift, y_shift
eval_shift = function(transform_params, pred, obs, objective_fn) {

  obs_shifted = obs |>
    dplyr::mutate(x = obs$x + transform_params[[1]], y = obs$y + transform_params[[2]])

  objective = objective_fn(pred, obs_shifted)

  return(objective)
}

# Function to find the best shift, using a grid search
# base_shift_x and base_shift_y: the x and y shifts to center the grid search around (e.g., if a
# previous iteration of the search identified a set of coarse shifts)
find_best_shift_grid = function(pred, obs, objective_fn,
                                search_window = 50,
                                search_increment = 2,
                                base_shift_x = 0,
                                base_shift_y = 0,
                                return_full_grid = FALSE) {

  # Define the shifts to test
  shifts_x = seq(-search_window + base_shift_x, search_window + base_shift_x, search_increment)
  shifts_y = seq(-search_window + base_shift_y, search_window + base_shift_y, search_increment)
  shifts = expand.grid(shift_x = shifts_x, shift_y = shifts_y)
  # NOTE: need to make sure that the order of params in "shifts" matches what's expected by
  # eval_shift

  transform_params = split(shifts, 1:nrow(shifts))

  future::plan(future::multicore, workers = parallelly::availableCores())
  shifts$objective = furrr::future_map_dbl(transform_params, eval_shift, pred, obs, objective_fn)
  future::plan(future::sequential)

  # Get the shift that minimized the objective function
  best_shift = shifts |>
    dplyr::filter(objective == min(objective))

  # Return it, along with the full grid if requested
  if (return_full_grid) {
    return(list(best_shift = best_shift, shifts = shifts))
  } else {
    return(list(best_shift = best_shift))
  }
}

# Find the overall best shift using the specified method.
# Only applies to grid search
find_best_shift = function(pred, obs, objective_fn = obj_mean_dist_to_closest, method = "grid") {

  if (method == "grid") {
    # Find the best shift using a grid search, starting wide and coarse
    best1 = find_best_shift_grid(pred, obs, objective_fn,
                                 search_window = 50,
                                 search_increment = 2)

    # Centered around the best coarse shift, do a finer grid search, starting with the best shift
    # from the coarse iteration
    best2 = find_best_shift_grid(pred, obs, objective_fn,
                                 search_window = 3,
                                 search_increment = 0.25,
                                 base_shift_x = best1$best_shift$shift_x,
                                 base_shift_y = best1$best_shift$shift_y)

    # Return the best shift
    return(best2$best_shift)

  } else if (method == "optim") {
    # Find the best shift using an optimization algorithm

    optim(par = c(0, 0),
          fn = eval_shift,
          gr = NULL,
          pred = pred,
          obs = obs,
          objective_fn = obj_mean_dist_to_closest,
          method = "Nelder-Mead")

  } else {
    stop("Invalid method passed to find_best_shift()")
  }
}

drop_understory_trees = function(trees) {
  
  # Drop all trees that are within 1+0.1h m horiz distance of a taller tree
  trees$search_rad = 1 + 0.1 * trees$z
  
  # For each tree, get the distance to every other tree and then check if within the search radius
  # of any
  dists = sqrt(outer(trees$x, trees$x, "-")^2 +
                 outer(trees$y, trees$y, "-")^2)
  
  trees$understory = FALSE
  
  for(i in seq_len(nrow(trees))) {
    
    # which trees have the focal tree within their radius?
    within_radius = which(dists[i,] < trees$search_rad)
    
    # which of those are taller than the focal tree?
    taller = which(trees$z[within_radius] > trees$z[i])
    
    if(length(taller) > 0) {
      trees[i, "understory"] = TRUE
    }
  }
  
  trees = trees[!trees$understory, ]
  
  return(trees)
  
}


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
