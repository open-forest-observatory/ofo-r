CANDIDATE_HEIGHT_COLNAMES = c("Height", "height", "z")

# Function to look for one or more columns matching a provided set of names, select the first
# matching one, and rename it to a provided standard name
standardize_colname = function(d, candidate_colnames, new_colname) {
  # Which of the candidate height column names is present in the data frame?
  candidate_colnames_idxs = which(names(d) %in% candidate_colnames)
  candidate_colnames = names(d)[candidate_colnames_idxs]

  # If there are no height cols, return an error
  if (length(candidate_colnames) == 0) {
    stop("No height column found in the data frame")
  }

  # If more than one height col found, select the first and issue a warning
  if (length(candidate_colnames) > 1) {
    warning(
      "Multiple height columns found in the data frame (",
      paste(candidate_colnames, collapse = ", "),
      "); using the first one"
    )
  }

  src_height_colname = candidate_colnames[1]

  d = d |>
    dplyr::rename(!!new_colname := src_height_colname)

  return(d)
}

# Take a lon/lat coordinates dataframe and convert to the local UTM zone EPSG code
lonlat_to_utm_epsg = function(lonlat) {
  utm = (floor((lonlat[, 1] + 180) / 6) %% 60) + 1
  utms = ifelse(lonlat[, 2] > 0, utm + 32600, utm + 32700)

  utms_unique = unique(utms)

  if (length(utms_unique) > 2) {
    stop("The geometry spans 3 or more UTM zones")
  } else if (length(utms_unique) > 1) {
    if (abs(diff(utms_unique)) > 1) {
      stop("The geometry spans 2 non-adjacent UTM zones.")
    }
  }

  return(utms_unique[1])
}

# Reproject a sf object into the CRS representing its local UTM zone
#' @export
transform_to_local_utm = function(sf) {
  geo = sf::st_transform(sf, 4326)
  geo_noz = sf::st_zm(geo, drop = TRUE)
  lonlat = sf::st_centroid(geo_noz) |> sf::st_coordinates()
  utm = lonlat_to_utm_epsg(lonlat)

  sf_transf = sf::st_transform(sf, utm)

  return(sf_transf)
}

#' @export
prep_obs_map = function(obs, obs_bound, edge_buffer) {
  # Convert the maps to sf objects (without a real CRS) if they are not already. This is to enable
  # tree map comparison in an arbitrary "local" crs, and in this case we assume all other files are in
  # the same local CRS
  if (!inherits(obs, "sf")) {
    obs = sf::st_as_sf(obs, coords = c("x", "y"))
  } else {
    # Ensure the CRS is the local UTM zone
    obs = transform_to_local_utm(obs)
    obs_bound = st_transform(obs_bound, st_crs(obs))
  }

  # Prep an internally buffered region (to accommodate edge uncertainty)
  obs_bound_core = sf::st_buffer(obs_bound, -edge_buffer)

  # Assign an incremental unique ID to each observed tree
  obs$obs_tree_id = 1:nrow(obs)

  # Assign the trees an attribute that designates whether they are within the internally-buffered region
  obs$obs_tree_core_area = sf::st_intersects(obs, obs_bound_core, sparse = FALSE) |> as.vector()

  # Assign a more descriptive name to the height column, tolerating a variety of names for height
  obs = standardize_colname(obs, CANDIDATE_HEIGHT_COLNAMES, "obs_tree_height")

  # Ensure all trees have heights; if < 1% are missing heights, drop them, otherwise throw an error
  n_missing_height = sum(is.na(obs$obs_tree_height))
  if (n_missing_height > 0) {
    pct_missing_height = ((n_missing_height / nrow(obs)) |> round(3)) * 100
    if (pct_missing_height > 1) {
      stop("More than 1% of observed trees are missing heights")
    } else {
      warning(
        "There are ",
        n_missing_height,
        " observed trees missing heights (",
        pct_missing_height,
        "%); dropping them"
      )
      obs = obs |> dplyr::filter(!is.na(obs_tree_height))
    }
  }

  # Add an (empty for now) attribute that will store which predicted tree the observed tree is matched to
  obs$matched_pred_tree_id = NA

  return(obs)
}

#' @export
prep_pred_map = function(pred, obs_bound, edge_buffer) {
  # Convert the maps to sf objects (without a real CRS) if they are not already. This is to enable
  # tree map comparison in an arbitrary "local" crs, and in this case we assume all other files are in
  # the same local CRS
  if (!inherits(pred, "sf")) {
    pred = sf::st_as_sf(pred, coords = c("x", "y"))
  } else {
    # Ensure the CRS is the local UTM zone
    pred = transform_to_local_utm(pred)
    obs_bound = st_transform(obs_bound, st_crs(pred))
  }

  # Prep an internally buffered region (to accommodate edge uncertainty)
  obs_bound_core = obs_bound |> sf::st_buffer(-edge_buffer)

  # Crop the predicted map to the observed map
  pred = sf::st_intersection(pred, obs_bound)

  if(nrow(pred) == 0) return(pred)

  # Assign an incremental unique ID to each predicted tree
  pred$pred_tree_id = 1:nrow(pred)

  # Assign the trees an attribute that designates whether they are within the internally-buffered region
  pred$pred_tree_core_area = sf::st_intersects(pred, obs_bound_core, sparse = FALSE) |> as.vector()

  # Assign a more descriptive name to the height column and core area column
  pred = standardize_colname(pred, CANDIDATE_HEIGHT_COLNAMES, "pred_tree_height")

  # Ensure all trees have heights; if < 1% are missing heights, drop them, otherwise throw an error
  n_missing_height = sum(is.na(pred$pred_tree_height))
  if (n_missing_height > 0) {
    pct_missing_height = ((n_missing_height / nrow(pred)) |> round(3)) * 100
    if (pct_missing_height > 1) {
      stop("More than 1% of observed trees are missing heights")
    } else {
      warning(
        "There are ",
        n_missing_height,
        " predicted trees missing heights (",
        pct_missing_height,
        "%); dropping them"
      )
      pred = pred |> dplyr::filter(!is.na(pred_tree_height))
    }
  }

  return(pred)
}

#' @export
match_obs_to_pred_mee = function(obs, pred, search_distance_fun_intercept, search_distance_fun_slope, search_height_proportion) {
  if (st_crs(obs) != st_crs(pred)) {
    stop("The observed and predicted tree maps are not in the same UTM zone.")
  }

  dist_mat <- sf::st_distance(obs, pred) |> drop_units_if_present() # To remove units if present

  colnames(dist_mat) = pred$pred_tree_id
  rownames(dist_mat) = obs$obs_tree_id

  dist_graph = as.data.frame(as.table(dist_mat)) |>
    dplyr::mutate(
      Var1 = as.numeric(as.character(Var1)),
      Var2 = as.numeric(as.character(Var2))
    ) |>
    dplyr::rename(
      "obs_tree_id" = "Var1",
      "pred_tree_id" = "Var2",
      "dist" = "Freq"
    )

  # Filter to only trees within the maximum matching distance
  tallest_obs_tree = max(obs$obs_tree_height)
  max_dist_window = search_distance_fun_intercept + search_distance_fun_slope * tallest_obs_tree
  dist_graph = dist_graph |>
    dplyr::filter(dist <= max_dist_window)

  # Pull in each tree's height into the distance graph
  pred_height = pred |>
    dplyr::select(pred_tree_id, pred_tree_height)
  sf::st_geometry(pred_height) = NULL

  obs_height = obs %>%
    dplyr::select(obs_tree_id, obs_tree_height)
  sf::st_geometry(obs_height) = NULL

  dist_graph = dplyr::left_join(dist_graph, obs_height, by = "obs_tree_id")
  dist_graph = dplyr::left_join(dist_graph, pred_height, by = "pred_tree_id")

  # Take every possible pairing of trees and filter to those within matching distance (horiz and vert)
  dist_graph = dist_graph |>
    dplyr::mutate(max_dist = search_distance_fun_intercept + search_distance_fun_slope * obs_tree_height) |>
    dplyr::filter(dist <= max_dist) |>
    dplyr::mutate(
      min_height = search_height_proportion * obs_tree_height,
      max_height = (1 + search_height_proportion) * obs_tree_height
    ) |>
    dplyr::filter(pred_tree_height >= min_height & pred_tree_height <= max_height) |>
    dplyr::arrange(dist)

  # Go through the list of possible pairings and find the best match for each observed tree. When
  # there are multiple potential matches, the shortest distance match is chosen, and the rest remain
  # eligible for matching

  pred_ids_matched = NULL
  obs_ids_matched = NULL

  for (i in seq_len(nrow(dist_graph))) {
    row = dist_graph[i, ]
    if (row$pred_tree_id %in% pred_ids_matched || row$obs_tree_id %in% obs_ids_matched) {
      # already matched, no longer eligible
      next()
    } else {
      # this is a new match; record it
      pred_ids_matched = c(pred_ids_matched, row$pred_tree_id)
      obs_ids_matched = c(obs_ids_matched, row$obs_tree_id)
      obs[obs$obs_tree_id == row$obs_tree_id, "matched_pred_tree_id"] = row$pred_tree_id
    }
  }

  return(obs)
}

# Function that takes a table of observed trees with an indication of whether they were matched to a
# predicted tree, a table of predicted trees with an indication of whether they were matched to an
# observed tree, and a minimum tree height to consider for assessment, and returns the number of
# predicted trees, number of observed trees, the number of predicted matched to observed, and number
# of observed matched to predicted. Purpose is that this can be re-run for different minimum
# heights. It is necessary to pass these two separate tables because when assessing sensitivity
# (recall), we need to check how many of the observed trees *within the core area* match to
# predicted trees *regardless of whether they're in the core area*, and similarly for precision, we
# need to check how many of the predicted trees *within the core area* match to observed trees
# *regardless of whether they're in the core area*, so the sets of trees used for each is different.
count_total_and_matched_trees = function(obs_pred_match, pred_obs_match, min_height) {
  obs_pred_match_counts <- obs_pred_match |>
    dplyr::filter(
      obs_tree_height >= min_height,
      obs_tree_core_area == TRUE
    ) |>
    dplyr::summarize(
      n_obs_match_pred = sum(!is.na(pred_tree_id) & !is.na(obs_tree_id)),
      n_obs = dplyr::n()
    )

  pred_obs_match_counts <- pred_obs_match |>
    dplyr::filter(
      pred_tree_height >= min_height,
      pred_tree_core_area == TRUE
    ) |>
    dplyr::summarize(
      n_pred_match_obs = sum(!is.na(pred_tree_id) & !is.na(obs_tree_id)),
      n_pred = dplyr::n()
    )

  matched_counts = cbind(obs_pred_match_counts, pred_obs_match_counts)
  matched_counts$min_height = min_height

  return(matched_counts)
}


#' @export
compute_match_stats = function(pred, obs_matched, min_height = 10) {
  obs_simple = obs_matched |>
    dplyr::select(obs_tree_id, matched_pred_tree_id, obs_tree_height, obs_tree_core_area)

  pred_simple = pred |>
    dplyr::select(pred_tree_id, pred_tree_height, pred_tree_core_area)

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

  pred_obs_match <- dplyr::left_join(pred_simple,
    obs_simple,
    by = c("pred_tree_id" = "matched_pred_tree_id")
  )

  obs_pred_match <- dplyr::right_join(pred_simple,
    obs_simple,
    by = c("pred_tree_id" = "matched_pred_tree_id")
  )


  # Sum the tree counts (number of predicted trees, number of predicted trees matched, number of
  # observed trees, and number of observed trees matched) across height classes

  match_counts = count_total_and_matched_trees(obs_pred_match, pred_obs_match, min_height = min_height)

  # Compute recall, precision, f_score for individual tree detection
  match_stats <- match_counts |>
    dplyr::mutate(
      recall = n_obs_match_pred / n_obs,
      precision = n_pred_match_obs / n_pred
    ) |>
    dplyr::mutate(f_score = 2 * recall * precision / (recall + precision))

  return(match_stats)
}

# A wrapper for the above functions that performs the prep, matching, and stats computation
match_trees_compute_stats = function(obs,
                                     pred,
                                     obs_boundary,
                                     edge_buffer = 5,
                                     search_distance_fun_intercept = 1,
                                     search_distance_fun_slope = 0.1,
                                     search_height_proportion = 0.5,
                                     min_height = 10) {
  # Get the predicted and observed tree points into the standardized format needed for the accuracy
  # assessment
  obs_prepped = prep_obs_map(obs, obs_boundary, edge_buffer = edge_buffer)
  pred_prepped = prep_pred_map(pred, obs_boundary, edge_buffer = edge_buffer)

  # Match the predicted and observed trees
  obs_matched = match_obs_to_pred_mee(
    obs = obs_prepped,
    pred = pred_prepped,
    search_distance_fun_intercept = search_distance_fun_intercept,
    search_distance_fun_slope = search_distance_fun_slope,
    search_height_proportion = search_height_proportion
  )

  # Compute recall, precision, F-score, etc.
  match_stats = compute_match_stats(pred_prepped,
    obs_matched,
    min_height = min_height
  )

  return(match_stats)
}

#' Return the euclidian distance between the predicted and true shift
#'
#' @param predicted_shift The predicted shift with shift_x and shift_y attributes
#' @param map_config The true shifts with shift_x and shift_y attributes
#' @return A scalar distance
eval_res_dist = function(predicted_shift, map_config) {
  pred_shift_vec = c(predicted_shift$shift_x, predicted_shift$shift_y)
  # TODO figure out one convention so we don't have to keep negating this
  pred_shift_vec = -1 * pred_shift_vec

  map_shift = c(map_config$shift_x, map_config$shift_y)
  # Compute the diffrence between the real and predicted shifts
  diff = pred_shift_vec - map_shift
  # Compute the Euclidean distance from this difference
  dist = norm(as.matrix(diff), type = "2")
  return(dist)
}

# Determine whether the distance is less than the threshold value. Since lower is better
# for the other metric, the same is here, with 0 representing suffient alignment and 1
# representing a bad match
#'
#' @param predicted_shift The predicted shift with shift_x and shift_y attributes
#' @param map_config The true shifts with shift_x and shift_y attributes
#' @return An integer, either 0 for a match or 1 for no match
eval_res_threshold = function(predicted_shift, map_config, threshold = 3) {
  # Compute the distance
  dist = eval_res_dist(predicted_shift = predicted_shift, map_config = map_config)
  if (dist < threshold) {
    ind = 0
  } else {
    ind = 1
  }
  return(ind)
}
