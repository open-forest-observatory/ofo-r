# For each observed point, get the closest predicted point in x, y, z space
# (note: this is not a true distance, but is fine for our purposes)
get_closest_obs = function(obs, pred) {
  # Get the closest predicted point to each observed point, in x, y, z space
  closest = pred |>
    group_by(x, y) |>
    mutate(dist = sqrt((x - first(obs$x))^2 + (y - first(obs$y))^2 + (z - first(obs$z))^2)) |>
    filter(dist == min(dist)) |>
    ungroup()

  # Join back to the observed points
  obs = obs |>
    left_join(closest, by = c("x", "y"))

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
    dplyr::filter(dplyr::between(x, xmin - xrange / 4, xmax + xrange / 4) & between(y, ymin - yrange / 4, ymax + yrange / 4))

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
eval_shift = function(params_to_optimize, pred, obs, objective_fn) {
  obs_shifted = obs |>
    dplyr::mutate(x = x + params_to_optimize[1], y = y + params_to_optimize[2])

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

  # Try each shift and compute the objective function
  for (i in seq_len(nrow(shifts))) {
    shift = shifts[i, ]

    obs_shifted = obs |>
      mutate(x = x + shift$shift_x, y = y + shift$shift_y)

    shifts[i, "objective"] = obj_mean_dist_to_closest(pred, obs_shifted)
  }
  
  # Get the shift that minimized the objective function
  best_shift = shifts |>
    filter(objective == min(objective))
   
  # Return it, along with the full grid if requested  
  if (return_full_grid) {
    return(list(best_shift = best_shift, shifts = shifts))
  } else {
    return(list(best_shift = best_shift))
  }
}

# Find the overall best shift using the specified method.
# Only applies to grid search
find_best_shift = function(pred, obs, objective_fn, search_type = "grid2") {

  if(search_type == "grid2") {
    # Find the best shift using a grid search, starting wide and coarse
    best1 = find_best_shift_grid(pred, obs, objective_fn,
                                      search_window = 50,
                                      search_increment = 2)

    # Centered around the best coarse shift, do a finer grid search, starting with the best shift from the coarse
    # iteration
    best2 = find_best_shift_grid(pred, obs, objective_fn,
                                      search_window = 3,
                                      search_increment = 0.25,
                                      base_shift_x = best1$best_shift$shift_x,
                                      base_shift_y = best1$best_shift$shift_y)

    # Return the best shift
    return(best2$best_shift)
  } else if (search_type == "optim") {
    # Find the best shift using an optimization algorithm

    optim(
      par = c(0, 0),
      fn = shift_and_eval,
      gr = NULL,
      pred = pred,
      obs = obs,
      objective_fn = obj_mean_dist_to_closest,
      method = "BFGS")
    
  } else {
    stop("Invalid search_type passed to find_best_shift()")
  }
}