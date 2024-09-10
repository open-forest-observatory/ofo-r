library(rdist)
## TODO: how much better is the best match vs the  mean of the alternatives (but only those where there is complete overlap between predicted and observed)? This could be a way to quantify the confidence in the alignment.
#### TODO: Make sure that the requested range of shifts is still completley within the predicted stem map.

# Functions for determining the optimal alignment between a predicted (e.g. drone-based) and
# observed (e.g. field-based) tree map.


## Create a simulated "predicted" and "observed" tree map
# Generate random, customizably clustered points in x-y space, over an area wider than would
# reasonably have a field stem map (to represent the drone-based tree predictions). Interpret the
# x-y coords as meters.
simulate_tree_maps = function(trees_per_ha = 250, trees_per_clust = 5, cluster_radius = 25,
                              pred_extent = 300,
                              obs_extent = 100,
                              horiz_jitter = 1,
                              vert_jitter = 5, # max of 5
                              false_pos = 0.25,
                              false_neg = 0.25,
                              drop_observed_understory = TRUE,
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
  heights = heights[heights > 5 & heights < 50]
  pred$z = sample(heights, nrow(pred), replace = TRUE)
  # Order by decreasing height, so when plotting the smaller trees are on top
  pred = pred[order(-pred$z), ]

  # Take a spatial subset of these points, to represent the "observed" tree map (e.g. field
  # reference plot)
  obs = pred[pred$x > -obs_extent / 2 &
               pred$x < obs_extent / 2 &
               pred$y > -obs_extent / 2 &
               pred$y < obs_extent / 2, ]

  # Randomly remove a fraction of the trees from each map, to simulate false positives and false negatives
  pred = pred |>
    dplyr::sample_frac(1 - false_neg)
  obs = obs |>
    dplyr::sample_frac(1 - false_pos)

  # Add some noise to the predicted tree x-y coords
  pred$x = pred$x + runif(nrow(pred), -horiz_jitter, horiz_jitter)
  pred$y = pred$y + runif(nrow(pred), -horiz_jitter, horiz_jitter)

  # Remove understory trees from the predicted dataset (to simulate drone not seeing them)
  pred = drop_understory_trees(pred)

  # Add some noise and bias to the predicted tree heights
  pred$z = pred$z + runif(nrow(pred), -vert_jitter, vert_jitter) + height_bias

  # If there are no trees in the observed dataset, skip some operations on it and return an empty
  # data frame for obs, along with the (non-empty) predicted tree data
  if (nrow(obs) == 0) {
    return(list(pred = pred, obs = obs))
  }

  # If specified, remove understory trees from observed dataset (to see if it improves alignment)
  if (drop_observed_understory) {
    obs = drop_understory_trees(obs)
  }

  # If specified, remove small trees from the observed map, so we only base the alignment on the
  # large observed trees (matching to any size small trees)
  # TODO: This made it worse and does not accommodate dataset with only DBH so needs to be refined or
  # removed.
  # obs = obs[obs$z > drop_small_thresh, ]

  # Shift obs a known amount that we will attempt to recover
  obs$x = obs$x + shift_x
  obs$y = obs$y + shift_y

  # Compute the bounds of the observed tree map and return as 'sf' object
  obs_sf = sf::st_as_sf(obs, coords = c("x", "y"))
  obs_bbox = sf::st_bbox(obs_sf)
  obs_bound = sf::st_as_sfc(obs_bbox) |> sf::st_as_sf()

  return(list(pred = pred, obs = obs, obs_bound = obs_bound))

}

# Crop the predicted points to the x-y extent of the observed points +- 25%, so we don't waste time
# computing distances to points that are definitely not the closest
crop_pred_to_obs = function(pred, obs) {

  xmin = min(obs$x)
  ymin = min(obs$y)
  xmax = max(obs$x)
  ymax = max(obs$y)
  xrange = xmax - xmin
  yrange = ymax - ymin

  pred_crop = pred |>
    dplyr::filter(dplyr::between(x, xmin - xrange / 4, xmax + xrange / 4) &
                    dplyr::between(y, ymin - yrange / 4, ymax + yrange / 4))
}


# Objective function: the mean x,y,z distance between each observed tree and its nearest predicted
# tree. Note that obs_bound is not used by this objective function logic, but it needs to be included for compatibility with objective functions that do require it.
#' @export
obj_mean_dist_to_closest = function(pred, obs, ...) {

  # Crop the predicted points to the x-y extent of the observed points +- 25%, so we don't waste
  # time if the user provided a predicted tree map that is much larger than the observed tree map
  pred_crop = crop_pred_to_obs(pred, obs)

  ## For each predicted point, get the closest observed point in x, y, z space
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

  return(objective)
}


obj_mee_matching = function(pred, obs, obs_bound) {

  pred_crop = crop_pred_to_obs(pred, obs)

  # Prep predicted and observed tree maps for comparison
  obs_prepped = prep_obs_map(obs, obs_bound = obs_bound, edge_buffer = 5)
  pred_prepped = prep_pred_map(pred_crop, obs_bound = obs_bound, edge_buffer = 5)

  # Match predicted and observed trees, following logic in MEE paper, and compute the match stats

  obs_matched = match_obs_to_pred_mee(obs_prepped, pred_prepped,
                                      search_distance_fun_intercept = 1,
                                      search_distance_fun_slope = 0.1,
                                      search_height_proportion = 0.5)

  match_stats = compute_match_stats(pred_prepped, obs_matched)
  f_score = match_stats$f_score

  # If no trees match, return the worst possible score
  if(is.nan(f_score) | is.na(f_score)) f_score = 0

  # Compute the objective function value as 1 - f_score
  objective = 1 - f_score

  return(objective)
}



# Test a given x-y shift and compute the objective function for it, using the objective
# function supplied to it
# transform_params: contains (in order): x_shift, y_shift
eval_shift = function(transform_params, pred, obs, obs_bound, objective_fn, visualize = FALSE) {

  obs_shifted = obs |>
    dplyr::mutate(x = obs$x + transform_params[[1]], y = obs$y + transform_params[[2]])

  # Shift the obs bound by the same amount the obs tree map is being shifted
  shift_df = data.frame(x = transform_params[[1]], y = transform_params[[2]])
  shift_geom = sf::st_as_sf(shift_df, coords = c("x", "y"))
  obs_bound_shifted = obs_bound + shift_geom
  obs_bound_shifted = sf::st_as_sf(obs_bound_shifted, crs = sf::st_crs(obs_bound))

  objective = objective_fn(pred, obs_shifted, obs_bound_shifted)

  if (visualize){
    vis2(pred, obs, shift=transform_params)
  }

  return(objective)
}

# Function to find the best shift, using a grid search
# base_shift_x and base_shift_y: the x and y shifts to center the grid search around (e.g., if a
# previous iteration of the search identified a set of coarse shifts)
find_best_shift_grid = function(pred, obs,
                                obs_bounds = NULL,
                                objective_fn,
                                search_window = 50,
                                search_increment = 2,
                                base_shift_x = 0,
                                base_shift_y = 0,
                                return_full_grid = FALSE,
                                parallel = TRUE) {

  # Make sure the observed tree map is within the predicted tree map
  # Approach: make predicted and observed tree maps into spatial 'sf' objects, get the bounds of
  # each as convex hulls, and then check if the observed tree map is fully within the bounds of the
  # predicted. Note the the bounds here are different than the manually provided bounds associated with the specific stem map

  obs_w_base_shift = obs
  obs_w_base_shift$x = obs$x + base_shift_x
  obs_w_base_shift$y = obs$y + base_shift_y

  if (is.null(obs_bounds)) {
    # If no obs_bound provided, compute one from the tree points
    obs_bounds = obs_w_base_shift |>
      sf::st_as_sf(coords = c("x", "y")) |>
      sf::st_union() |>
      sf::st_convex_hull() |>
      sf::st_as_sf()
  } else {
    # If we're using provided bounds, shift them by the same amount as the trial shift being applied
    # to the observed stem map, then make sure it's in the same CRS

    shift_df = data.frame(x = base_shift_x, y = base_shift_y)
    shift_geom = sf::st_as_sf(shift_df, coords = c("x", "y"))
    obs_bounds_shifted = obs_bounds + shift_geom
    obs_bounds_shifted = sf::st_as_sf(obs_bounds_shifted, crs = sf::st_crs(obs_bounds))

    # Ensure in the same CRS, if relevant
    if (!is.na(sf::st_crs(obs_bounds))) {
      obs_bounds = sf::st_transform(obs_bounds, sf::st_crs(pred_bounds))
    }
  }

  pred_bounds = pred |>
    sf::st_as_sf(coords = c("x", "y")) |>
    sf::st_union() |>
    sf::st_convex_hull()

  obs_inside_pred = sf::st_within(obs_bounds,
                                  pred_bounds,
                                  sparse = FALSE)

  if (!obs_inside_pred[1, 1]) {
    stop("The observed tree map (including its base shift of ", base_shift_x, ",", base_shift_y,
         ")is not fully within the bounds of the predicted tree map.")
  }

  # Make sure the search window does not result in a shift that would put the observed tree map
  # outside the bounds of the predicted tree map. Approach: make predicted and observed tree maps
  # into spatial 'sf' objects, get the bounds of each as convex hulls, buffer the observed tree map
  # by the search window, and then check if the buffered observed tree map is fully within the
  # bounds of the predicted.

  obs_search_bounds = obs_bounds |>
    sf::st_buffer(search_window)

  search_inside_pred = sf::st_within(obs_search_bounds,
                                     pred_bounds,
                                     sparse = FALSE)

  if (!search_inside_pred[1, 1]) {

    # Get the maximum search window that fits, and reduce the search window to that distance
    obs_bounds_line = sf::st_cast(obs_bounds, "MULTILINESTRING")
    pred_bounds_line = sf::st_cast(pred_bounds, "MULTILINESTRING")
    max_search_window = sf::st_distance(obs_bounds_line, pred_bounds_line)

    warning("The specified search window of ", search_window, " is too large. Accounting for the base shift of (",
            base_shift_x, ",", base_shift_y,
            ") it would result in a search outside the bounds of the predicted tree map. Reducing the search window to the maximum possible of ",
            round(max_search_window, 2), ".")

    search_window = max_search_window
  }

  # TODO !! Perform predicted tree map prep (attribute with in/outside plot bounds, ensure correct
  # projection, assign tree IDs, determine whether in internal buffer, etc)


  # Define the shifts to test
  shifts_x = seq(-search_window + base_shift_x, search_window + base_shift_x, search_increment)
  shifts_y = seq(-search_window + base_shift_y, search_window + base_shift_y, search_increment)
  shifts = expand.grid(shift_x = shifts_x, shift_y = shifts_y)
  # NOTE: need to make sure that the order of params in "shifts" matches what's expected by
  # eval_shift

  transform_params = split(shifts, seq_len(nrow(shifts)))

  if (parallel) {
    future::plan(future::multicore, workers = parallelly::availableCores())
    shifts$objective = furrr::future_map_dbl(transform_params, eval_shift, pred, obs, obs_bound = obs_bounds, objective_fn, .options = furrr::furrr_options(seed = TRUE))
    future::plan(future::sequential)
  } else {
    shifts$objective = purrr::map_dbl(transform_params, eval_shift, pred, obs, obs_bound = obs_bounds, objective_fn)
  }


  # Get the shift that minimized the objective function
  best_shift = shifts[shifts$objective == min(shifts$objective), ]

  # Return it, along with the full grid if requested
  if (return_full_grid) {
    return(list(best_shift = best_shift, shifts = shifts))
  } else {
    return(list(best_shift = best_shift))
  }
}

# Find the overall best shift using the specified method.
# Currently only works for grid search
#' @export
find_best_shift = function(pred, obs,
                           obs_bounds = NULL,
                           objective_fn = obj_mean_dist_to_closest,
                           method = "grid",
                           parallel = TRUE) {

  if (method == "grid") {

    # Keep track of execution time
    tictoc::tic.clear()
    tictoc::tic(quiet = TRUE)

    # Find the best shift using a grid search, starting wide and coarse
    result_coarse = find_best_shift_grid(pred = pred, obs = obs,
                                         obs_bounds = obs_bounds,
                                         objective_fn = objective_fn,
                                         search_window = 50,
                                         search_increment = 2,
                                         return_full_grid = TRUE,
                                         parallel = parallel)

    # Centered around the best coarse shift, do a finer grid search, starting with the best shift
    # from the coarse iteration
    result_fine = find_best_shift_grid(pred = pred, obs = obs,
                                       obs_bounds = obs_bounds,
                                       objective_fn = objective_fn,
                                       search_window = 3,
                                       search_increment = 0.25,
                                       base_shift_x = result_coarse$best_shift$shift_x,
                                       base_shift_y = result_coarse$best_shift$shift_y,
                                       parallel = parallel)


    # Determine how much better the best shift (of the second iteration) is than the mean of the
    # alternatives from the first iteration

    shifts = result_coarse$shifts

    # Get the mean obj value of the alternatives from the first iteration
    median_obj = median(shifts$objective, na.rm = TRUE)


    # Get the number of shifts tried
    n_shifts_coarse = sum(!is.na(shifts$objective))
    # Get the number of trees in the observed dataset
    n_trees_obs = nrow(obs)
    # Get the time taken
    toc = tictoc::toc()
    time_taken = toc$toc - toc$tic

    # Return the best shift and the ancillary data
    result = data.frame(shift_x = result_fine$best_shift$shift_x,
                        shift_y = result_fine$best_shift$shift_y,
                        shift_obj = result_fine$best_shift$objective,
                        median_obj = median_obj,
                        n_tested_coarse = n_shifts_coarse,
                        n_trees_obs = n_trees_obs,
                        time_taken = time_taken)

    row.names(result) = NULL

    # Return the best shift
    return(result)

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
    stop("Invalid method ", method, " passed to find_best_shift()")
  }
}


# Find the overall best shift using PDAL's iterative closest point implementation
find_best_shift_icp = function(pred, obs) {

  # Keep track of execution time
  tictoc::tic.clear()
  tictoc::tic(quiet = TRUE)

  # Business logic

  coords_pred = pred |> dplyr::select(X = x, Y = y, Z = z)
  pred_las = lidR::LAS(coords_pred)

  obs = sim$obs
  coords_obs = obs |> dplyr::select(X = x, Y = y, Z = z)
  obs_unaligned_las = lidR::LAS(coords_obs)

  # Write to temp dir
  pred_file = tempfile(fileext = ".las")
  obs_unaligned_file = tempfile(fileext = ".las")
  obs_aligned_file = tempfile(fileext = ".las")
  lidR::writeLAS(pred_las, pred_file)
  lidR::writeLAS(obs_unaligned_las, obs_unaligned_file)

  # Create PDAL pipeline for filter.icp algorithm
  pipeline = c(
    "[",
    paste0('"', pred_file, '"', ","),
    paste0('"', obs_unaligned_file, '"', ","),
    "{",
    '"type":"filters.icp"',
    # '"method":"rigid"',
    "},",
    paste0('"', obs_aligned_file, '"'),
    "]"
  )

  pipeline_file = tempfile(fileext = ".json")
  writeLines(pipeline, pipeline_file)

  cmd_call = paste0("pdal pipeline ", pipeline_file)
  cmd_call
  system(cmd_call)

  obs_aligned_las = lidR::readLAS(obs_aligned_file)

  # Get the xyz coords
  obs_aligned_sf = sf::st_as_sf(obs_aligned_las)
  obs_aligned_coords = sf::st_coordinates(obs_aligned_sf)

  obs_aligned_coords_conformed = obs_aligned_coords |>
    dplyr::as_tibble() |>
    dplyr::select(x = X, y = Y, z = Z)

  obs_unaligned_sf = obs_unaligned_las |>
    sf::st_as_sf()

  obs_unaligned_coords_conformed = obs_unaligned_sf |>
    sf::st_coordinates() |>
    dplyr::as_tibble() |>
    dplyr::select(x = X, y = Y, z = Z)

  shifts = obs_unaligned_coords_conformed - obs_aligned_coords_conformed

  # Get the number of trees in the observed dataset
  n_trees_obs = nrow(obs)
  # Get the time taken
  toc = tictoc::toc()
  time_taken = toc$toc - toc$tic

  # Return the best shift and the ancillary data
  result = data.frame(
    shift_x = mean(shifts$x),
    shift_y = mean(shifts$y),
    n_trees_obs = n_trees_obs,
    time_taken = time_taken
  )

  row.names(result) = NULL

  # Return the best shift
  return(result)
}

transform_points_by_homography = function(xy_mat, homography) {
  #' Transforms a set of points by a homography.
  #' xy_mat: a (n_points, 2) matrix of data to
  #' homography: a (3, 3) matrix following the conventions of a 2d homography
  #'
  #' Returns: the (n_points, 2) transformed points
  homogenous_xy_mat = cbind(xy_mat, rep(1, nrow(xy_mat)))
  transformed_xy_mat = t(homography %*% t(homogenous_xy_mat))
  transformed_xy_mat = transformed_xy_mat[, 1:2]
  return(transformed_xy_mat)
}

# Implementation of the following MATLAB function
# https://gitlab.com/fgi_nls/public/2d-registration/-/blame/main/fit_euclidean_transformation.m?ref_type=heads#L231
compute_feature_descriptors = function(xy_mat, R_local) {
  xy_mat = as.matrix(xy_mat)[, 1:2]
  # Convert the pairwise distances between each row
  pdist = dist(xy_mat, method = "euclidean")
  # Convert from a dist object into a matrix
  pdist = as.matrix(pdist)
  # Set the diagnal entries to the the max value. This ensures that the closest matching point
  # for a given point is not itself.
  diag(pdist) = max(pdist)
  # Finding the closest neighboring point for each point
  # Invert the sign so we can use max.col
  indices_of_closest_points = max.col(-pdist)

  # Coordinates of the closest neighboring point for each of the point
  closest_points = xy_mat[indices_of_closest_points]
  # For each point, compute the normalized characteristic directions that
  # are locally used as the 1st axis direction.

  char_dirs = closest_points - xy_mat
  distances = sqrt(char_dirs[, 1]^2 + char_dirs[, 2]^2)
  char_dirs = char_dirs / distances

  # Directions perpendicular to the characteristic directions. These are
  # used as the local 2nd axis direction.
  char_dirs_perp = cbind(-1 * char_dirs[, 2], char_dirs[, 1])


  # For each point, we transform the coordinates of the other points
  # into the local coordinate frame of the given point We denote the
  # coordinate along the 1st axis direction by v and the coordinate along
  # the 2nd axis direction by w. In the matrices below, the element at row
  # i and column j means the coordinate of the jth object in the local
  # coordinate frame of the ith object.

  # Define intermediate variables
  x_values = xy_mat[, 1]
  y_values = xy_mat[, 2]

  n_points = nrow(xy_mat)
  x_tiled = matrix(rep(t(x_values), n_points), ncol = n_points)
  y_tiled = matrix(rep(t(y_values), n_points), ncol = n_points)
  x_minus_x_transposed = x_tiled - t(x_tiled)
  y_minus_y_transposed = y_tiled - t(y_tiled)


  v_mat = (
    (x_minus_x_transposed * char_dirs[, 1])
    + (y_minus_y_transposed * char_dirs[, 2])
  )
  w_mat = (
    (x_minus_x_transposed * char_dirs_perp[, 1])
    + (y_minus_y_transposed * char_dirs_perp[, 2])
  )

  # Use atan2 to compute the angles with respect to the characteristic
  # directions. The angles are in the range [-pi, pi]
  # TODO check if this atan2 has the same conventions as expected
  angle_mat = atan2(w_mat, v_mat)

  # For each object, we then need to find the closest object in each of the
  # four quadrants corresponding to angles (0, pi/2), (pi/2, pi), (-pi, -pi/2),
  # (-pi/2, 0).
  eps = 10^(-6) # used to exclude the closest object from the 1st/4th quadrant

  # Bounds for the quadrants
  lower_bounds = c(eps, pi / 2, -pi, -pi / 2)
  upper_bounds = c(pi / 2, pi, -pi / 2, -eps)

  # Pre-allocating the feature descriptor matrix
  feat_desc_mat = matrix(data = 0, nrow = n_points, ncol = 8)

  for (i_quadrant in 1:4) {
    lower_bound = lower_bounds[i_quadrant]
    upper_bound = upper_bounds[i_quadrant]
    quadrant_i_mat = angle_mat > lower_bound & angle_mat <= upper_bound

    # Construct a matrix marking objects not belonging to the current
    # quadrant with large values
    not_quadrant_i_mat = matrix(0, nrow = n_points, ncol = n_points)
    not_quadrant_i_mat[quadrant_i_mat == FALSE] = max(pdist) + eps

    # For each object, find the closest object within the current quadrant
    summed_matrices = pdist + not_quadrant_i_mat
    idx_closest_i = as.matrix(apply(summed_matrices, 2, which.min))
    min_dist_i = as.matrix(summed_matrices[cbind(1:n_points, idx_closest_i)])
    # For each object, determine the angle of the closest object in the
    # current quadrant w.r.t. the characteristic direction.
    angle_closest_quad_i = as.matrix(
      angle_mat[cbind(1:n_points, idx_closest_i)]
    )
    # Normalizing the distances and angles
    min_dist_i_norm = min_dist_i / R_local
    angle_quad_i_norm = (angle_closest_quad_i - lower_bound) / (pi / 2)
    # If the min distance in a given quadrant is larger than R_local, we set
    # the normalized distance and angle to -1.
    min_dist_i_norm[min_dist_i > R_local] = -1
    angle_quad_i_norm[min_dist_i > R_local] = -1
    # Storing the normalized min distance and the angle to the pre-allocated
    # feature descriptor matrix
    feat_desc_mat[, i_quadrant] = min_dist_i_norm
    feat_desc_mat[, i_quadrant + 4] = angle_quad_i_norm
  }

  char_theta = atan2(char_dirs[, 1], char_dirs[, 2])
  return(list(feat_desc_mat = feat_desc_mat, char_theta = char_theta))
}

# function [from_idx2_to_closest_idx1, from_idx2_to_min_dist1, n_of_matches] ...
get_closest_pairs_after_transformation = function(xy_mat1, xy_mat2, R_mat, t_vect, r_thres) {
  # Helper function for obtaining a mapping from each object in dataset 2 to the
  # index of the closest object in dataset 1 after transforming the object
  # locations in dataset 1 as R_mat*xy_mat1' + t_vect.
  # The function also evaluates the number of matching pairs by finding the
  # the number of closest object pairs, for which the pair-wise distance is below
  # r_thres.
  #
  # Args:
  #   xy_mat1 = (x,y)-coordinates of objects (e.g. trees) found from point
  #             cloud 1 as a N1-by-2 matrix. (N1=number of rows, 2= number of
  #             columns)
  #   xy_mat2 = (x,y)-coordinates of objects (e.g. trees) found from point
  #             cloud 2 as N2-by-2 matrix
  #   R_mat = 2-by-2 rotation matrix of the transformation
  #   t_vect = 2-by-1 column vector corresponding to the translation vector
  #           of the transformation
  #   r_thres = distance threshold for deciding if the closest object in the
  #           other point cloud is a match
  # Returns:
  #   from_idx2_to_closest_idx1 = N2-by-1 vector containing the indices of the closest
  #           object in dataset 1 for each object in dataset 2.
  #   from_idx2_to_min_dist1 = N2-by-1 vector containing the distances to the closest
  #           object in dataset 1 for each object in dataset 2.
  #   n_of_matches = number of matching object pairs based on r_thres.
  xy_mat1 = as.matrix(xy_mat1)
  xy_mat2 = as.matrix(xy_mat2)
  colnames(xy_mat2) = NULL
  xy_mat1_t = t(xy_mat1)
  rotated_xy1 = t(R_mat %*% xy_mat1_t)
  t_vect = as.vector(as.numeric(t_vect))
  xy_mat1_transformed = t(t(rotated_xy1) + t_vect) # N-by-2
  # Pair-wise locations between each object in dataset 1 and 2. Each row is
  # one object in dataset 2 and each column in one object in dataset 1.
  transformed_loc_p_dist = cdist(xy_mat2, xy_mat1_transformed)
  # Finding the closest object in transformed point cloud 1 for each object
  # in point cloud 2
  from_idx2_to_closest_idx1 = apply(transformed_loc_p_dist, 1, which.min)
  from_idx2_to_min_dist1 = transformed_loc_p_dist[
    cbind(1:nrow(transformed_loc_p_dist), from_idx2_to_closest_idx1)
  ]

  # Computing the number of matches, i.e., object pairs whose distance is
  # below the set criterion
  less_than_thresh = from_idx2_to_min_dist1 < r_thres
  n_of_matches = sum(less_than_thresh)

  return(
    list(
      from_idx2_to_closest_idx1 = from_idx2_to_closest_idx1,
      from_idx2_to_min_dist1 = from_idx2_to_min_dist1,
      n_of_matches = n_of_matches
    )
  )
}


# Implementation of the following MATLAB function
# https://gitlab.com/fgi_nls/public/2d-registration/-/blob/main/fit_euclidean_transformation.m?ref_type=heads
find_best_shift_hyyppa = function(pred, obs, R_local = 10, k = 20, r_thresh = 1.0, max_iters = 200) {
  # Rename the variables for consistency with the reference code and remove unneeded columns
  xy_mat1 = pred[, c("x", "y")]
  xy_mat2 = obs[, c("x", "y")]

  # Centering the coordinates before matching to improve numerical stability
  xy_mat1_mean = colMeans(xy_mat1[, c("x", "y")])
  xy_mat2_mean = colMeans(xy_mat2[, c("x", "y")])
  # The double transpose seems to be the easiest way to broadcast subtraction
  # given the recycling rules
  xy_mat1 = t(t(xy_mat1) - xy_mat1_mean)
  xy_mat2 = t(t(xy_mat2) - xy_mat2_mean)

  # Number of objects detected from each point cloud
  N_objects_vect = c(nrow(xy_mat1), nrow(xy_mat2))

  if (N_objects_vect[2] < k) {
    k = N_objects_vect[2]
    msg = paste0(
      "Number of objects in dataset 2 (and thus the potential number of matches) ",
      "is smaller than given parameter k. k was set to ",
      N_objects_vect[2]
    )
    # TODO make this a warning
    print(msg)
  }

  # First, we construct the feature descriptor for each object in each of
  # the point clouds.
  feature_info1 = compute_feature_descriptors(xy_mat1, R_local)
  feature_info2 = compute_feature_descriptors(xy_mat2, R_local)
  # Extract out the components of the result
  feat_desc_mat1 = feature_info1$feat_desc_mat
  char_thetas1 = feature_info1$char_theta
  feat_desc_mat2 = feature_info2$feat_desc_mat
  char_thetas2 = feature_info2$char_theta

  # Compute a matrix of pairwise distances between feature
  # descriptors of the two point clouds -> rows correspond to objects detected
  # from the point cloud 2 and columns correspond to objects detected from
  # the point cloud 1.
  feat_desc_pdist = cdist(feat_desc_mat2, feat_desc_mat1)

  # Find the index per row with the lowest value
  nn_indices = apply(feat_desc_pdist, 1, which.min)
  lowest_dists = feat_desc_pdist[cbind(1:N_objects_vect[2], nn_indices)]

  # Now find the second nearest index for each point in the first point cloud
  # Mask out the first highest value.
  # Note this must be done after extracting the distances
  feat_desc_pdist[cbind(1:N_objects_vect[2], nn_indices)] = NA
  # Find the closest remaining value
  second_nearest_inds = apply(feat_desc_pdist, 1, which.min)
  second_lowest_dists = feat_desc_pdist[
    cbind(1:N_objects_vect[2], second_nearest_inds)
  ]

  # Sorting the nearest neighbor distance ratios so that we can rank the
  # tentative matches from most reliable to least reliable
  NNDR_vect = lowest_dists / second_lowest_dists
  sorted_indices = sort(NNDR_vect, index.return = TRUE, decreasing = FALSE)$ix

  # Then, we consider the k most promising tentative matches
  max_n_of_matches = 0 # maximum number of matching objects
  ## Initializing the parameters for the transformation of the best match.
  t_best = c(0, 0)
  theta_best = 0
  idx_matches1_best = c() # indices of matching objects in point cloud 1
  idx_matches2_best = c() # indices of matching objects in point cloud 2

  if (length(sorted_indices) < k) {
    k = length(sorted_indices)
    warning("Not enough valid correspondences, setting k to ", k)
  }

  # Indices for each element in the second set
  indices2 = 1:N_objects_vect[2]
  for (i_iter in 1:k) {
    # indices of objects corresponding to the current tentative match
    idx_object_2 = sorted_indices[i_iter]
    idx_object_1 = nn_indices[idx_object_2]

    # Characteristic directions corresponding to these objects
    char_theta1 = char_thetas1[idx_object_1]
    char_theta2 = char_thetas2[idx_object_2]

    # TODO figure out if this is the right convention
    delta_theta = char_theta2 - char_theta1
    # The corresponding rotation matrix, in column major order
    R_mat = matrix(
      as.numeric(
        c(cos(delta_theta), sin(delta_theta), -sin(delta_theta), cos(delta_theta))
      ),
      nrow = 2,
      ncol = 2
    )

    # Computing the translation vector based on the object locations in the two point
    # clouds
    xy_1 = xy_mat1[idx_object_1, c("x", "y")] # row vect.
    xy_2 = xy_mat2[idx_object_2, c("x", "y")] # row vect.
    # TODO ensure that matrix-vector multiplication works as expected
    xy1_t_vec = as.vector(as.numeric(xy_1))
    rotated_xy1 = R_mat %*% xy1_t_vec
    t_vect = (xy_2 - rotated_xy1) # column vector

    # For each object in point cloud 2, we find the closest object in the
    # point cloud 1 after using the estimated transformation. We also compute
    # the number of matches, i.e., object pairs whose distance is below the
    # set criterion
    res = get_closest_pairs_after_transformation(
      xy_mat1, xy_mat2, R_mat, t_vect, r_thresh
    )
    from_idx2_to_closest_idx1 = res[[1]]
    from_idx2_to_min_dist1 = res[[2]]
    n_of_matches = res[[3]]

    if (n_of_matches > max_n_of_matches) {
      max_n_of_matches = n_of_matches

      idx_matches2_best = indices2[from_idx2_to_min_dist1 < r_thresh]
      idx_matches1_best = from_idx2_to_closest_idx1[idx_matches2_best]

      t_best = t_vect
      theta_best = delta_theta
    }
  }
  # Compose the steps to recover the final transform since the computation occured on shifted data
  # It's going to be something like subtract mean of xy_1, then apply the R|t matrix, then add the mean of xy2
  subtract_xy1_mean = matrix(
    as.numeric(
      c(1, 0, -xy_mat1_mean[[1]], 0, 1, -xy_mat1_mean[[2]], 0, 0, 1)
    ),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  transform_by_R_t = matrix(
    as.numeric(
      c(
        cos(theta_best), -sin(theta_best), t_best[1],
        sin(theta_best), cos(theta_best), t_best[2],
        0, 0, 1
      )
    ),
    nrow = 3,
    ncol = 3,
    byrow = TRUE,
  )
  add_xy2_mean = matrix(
    as.numeric(c(1, 0, xy_mat2_mean[[1]], 0, 1, xy_mat2_mean[[2]], 0, 0, 1)),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )

  # Find the 3x3 matrix representing the transformation
  composite_transform = add_xy2_mean %*% transform_by_R_t %*% subtract_xy1_mean

  # Extract the elements of the matrix into a flat data frame
  result = data.frame(
    x_shift = composite_transform[1, 3],
    y_shift = composite_transform[2, 3],
    scale = sqrt(det(composite_transform[1:2, 1:2])), # TODO Check that this is correct
    rotation = atan2(composite_transform[2, 1], composite_transform[1, 1])
  )

  return(result)
}

big_testing_function = function(map_params, registration_methods, registration_arguments = NULL) {
  # Get the names of all arguments of the function and their default values
  simulate_tree_maps_default_args = formals(simulate_tree_maps)

  # Build the list of all provided values while using defaults for unspecified values
  all_map_params = list()
  for (name in names(simulate_tree_maps_default_args)) {
    if (name %in% names(map_params)) {
      all_map_params[name] = map_params[name]
    } else {
      all_map_params[name] = simulate_tree_maps_default_args[name]
    }
    if (!is.vector(all_map_params[[name]])) {
      all_map_params[name] = c(all_map_params[[name]])
    }
  }

  param_configurations = expand.grid(all_map_params)

  for (i in 1:nrow(param_configurations)) {
    row = param_configurations[i, ]
    res = do.call(simulate_tree_maps, row)
    print(res)
  }

  # Takes in
  ## parameter ranges (or lists) for a variety of attributes of the map
  ## potentially-multiple algorithms to test on the same datasets
  ## convergence criteria or a convergence metric

  # Create a cross product or sampling thereof from the map parameter space
  ## If there aren't enough rows, redraw that configuration
  # Run each of the algorithms on each of the maps
  # Compute the metric for each run
  # Plot the results, sliced by different metrics
  ## Probably do a scatter plot for each repetition of a given configuration
  ## Or average across all other params except for the one you are slicing by
  ## Or just return data frame
}

# Generate one pair of random tree maps (pred and observed) with observed shifted a known amt, test
# alignment, and return the result
make_map_and_align = function(method, ...) {

  # Simulate a predicted and observed tree map with a known offset
  sim = simulate_tree_maps(shift_x = 12, shift_y = 21, ...)

  # If there are too few trees in the simulated observed map, skip
  if(nrow(sim$obs) < 10) return(NULL)

  # Find the optimal shift, unparallelized because it is more efficient to parallelize over multiple
  # runs of the current function
  result = find_best_shift(sim$pred, sim$obs,
                           obs_bounds = sim$obs_bound,
                           method = method, parallel = FALSE)

  # Determine if the x-y shift was successfully recovered, using a hard-coded tolerance of +- 3 m in both dimensions
  result$shift_recovered = result$shift_x < -12 + 3 &
    result$shift_x > -12 - 3 &
    result$shift_y < -21 + 3 &
    result$shift_y > -21 - 3

  return(result)
}

# Try aligning multiple times, each with a new random tree map generated with the same parameters
# (to get percent of time we are able to recover the true shift)


### need to deal with fact that one trial may have < 10 trees but if the other does, it still gets included and 2 trials are recorded
calc_alignment_success_rate = function(n_tries = 4,
                                       method = "grid",
                                       parallel = TRUE,
                                       return_summary = TRUE,
                                       ...) {

  if (parallel) future::plan(future::multicore, workers = parallelly::availableCores())
  # This looks convoluted (and it is), but it is the cleanest way I could find to pass the "..."
  # parameter (essentially R's **kwargs) to 'make_map_and_align' and also run it for
  # multiple random iterations in parallel
  shifts = furrr::future_map(1:n_tries,
                             function(x, method, ...) make_map_and_align(method = method, ...), #nolint
                             method = method,
                             ...,
                             .options = furrr::furrr_options(seed = TRUE))
  future::plan(future::sequential)

  shifts = dplyr::bind_rows(shifts)

  # If requested, return a summary across all iterations, as a single row
  if (return_summary) {

    # remove rows with NAs (no data)
    if (nrow(shifts) == 0) return(data.frame(n_trials = 0))
    shifts = shifts[!is.na(shifts$n_trees_obs), ]

    n_trials = nrow(shifts) # can be different from 'n_tries' because some tries may have < 10 trees in obs map and thus be excluded
    n_gridcells_per_trial = mean(shifts$n_tested_coarse) |> round(2)
    mean_n_trees_obs = mean(shifts$n_trees_obs) |> round(1)
    mean_secs_per_trial = mean(shifts$time_taken) |> round(2)
    recovery_rate = mean(shifts$shift_recovered) |> round(4)

    summary = data.frame(n_gridcells_per_trial,
                         mean_n_trees_obs,
                         mean_secs_per_trial,
                         recovery_rate,
                         n_trials = n_trials)

    return(summary)

  } else {
    return(shifts)
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

  for (i in seq_len(nrow(trees))) {

    # which trees have the focal tree within their radius?
    within_radius = which(dists[i, ] < trees$search_rad)

    # which of those are taller than the focal tree?
    taller = which(trees$z[within_radius] > trees$z[i])

    if (length(taller) > 0) {
      trees[i, "understory"] = TRUE
    }
  }

  trees = trees[!trees$understory, ]

  return(trees)

}


# --- Visualization functions
# Visualize a hypothetical tree map
vis1 = function(trees) {
  p = ggplot2::ggplot(trees, ggplot2::aes(x, y, size = z, color = z)) +
    ggplot2::geom_point() +
    ggplot2::scale_size_continuous(range = c(0.5, 3)) +
    ggplot2::scale_color_viridis_c(end = 0.85) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = c(-200, 200), ylim = c(-200, 200))

  print(p)
}

# Visualize two hypothetica tree maps (predicted and observed) overlaid. obs_foc: zoom in to focus
# on the observed trees, with some buffer. Assumes the observed tree map is 50 x 50 and that both
# tree maps are centered at 0,0.
#' @export
vis2 = function(pred, obs, shift=c(0, 0), obs_foc = FALSE, coords_arbitrary = FALSE, zoom_to_obs = FALSE, obs_buffer = 75) {
  pred = pred |> dplyr::mutate(layer = "predicted")
  obs = obs |> dplyr::mutate(layer = "observed")

  # Shift the values. Will not change the data with the default shift
  pred = pred |> dplyr::mutate(x = x + shift[1], y = y + shift[2])

  trees = dplyr::bind_rows(pred, obs)

  lim = ifelse(obs_foc, 100, 200)

  if(coords_arbitrary) {
    lim = NA
  }

  lim_xmin = -lim
  lim_xmax = lim
  lim_ymin = -lim
  lim_ymax = lim

  # Should we constrain the coords to the observed trees plus some buffer?
  if (zoom_to_obs) {

    # get the coord extremes
    xmin = min(obs$x)
    ymin = min(obs$y)
    xmax = max(obs$x)
    ymax = max(obs$y)

    # get the range
    xrange = xmax - xmin
    yrange = ymax - ymin

    # set the limits as the observed lims +- a buffer
    lim_xmin = xmin - obs_buffer
    lim_xmax = xmax + obs_buffer
    lim_ymin = ymin - obs_buffer
    lim_ymax = ymax + obs_buffer

  }

  p = ggplot2::ggplot(trees, ggplot2::aes(x, y, size = z, color = layer)) +
    ggplot2::geom_point() +
    ggplot2::scale_size_continuous(range = c(0.5, 3)) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = c(lim_xmin, lim_xmax), ylim = c(lim_ymin, lim_ymax))

  print(p)
}
