prep_obs_map = function(obs, obs_bound, edge_buffer) {

  # TODO: Ensure predicted tree map is projected to same CRS as the observed tree map

  # Convert the maps to sf objects
  obs = sf::st_as_sf(obs, coords = c("x", "y"))

  # Prep an internally buffered region (to accommodate edge uncertainty)
  obs_bound_core = sf::st_buffer(obs_bound, -edge_buffer)
  obs_bound_core$core_area = TRUE

  # Assign an incremental unique ID to each observed tree
  obs$obs_tree_id = 1:nrow(obs)

  # Assign the trees an attribute that designates whether they are within the internally-buffered region
  obs$core_area = sf::st_intersects(obs, obs_bound_core, sparse = FALSE) |> as.vector()

  # Assign a more descriptive name to the height column and core area column
  obs = obs |>
    rename(obs_tree_height = z,
           obs_tree_core_area = core_area)

  # Add an (empty for now) attribute that will store which predicted tree the observed tree is matched to
  obs$matched_pred_tree_id = NA

  return(obs)

}

prep_pred_map = function(pred, obs_bound, edge_buffer) {

  # TODO: Ensure predicted tree map is projected to same CRS as the observed tree map

  # Convert the maps to sf objects
  pred = sf::st_as_sf(pred, coords = c("x", "y"))

  # Prep an internally buffered region (to accommodate edge uncertainty)
  obs_bound_core = obs_bound |> sf::st_buffer(-edge_buffer)
  obs_bound_core$core_area = TRUE

  # Crop the predicted map to the observed map
  pred = sf::st_intersection(pred, obs_bound)

  # Assign an incremental unique ID to each observed tree
  pred$pred_tree_id = 1:nrow(pred)

  # Assign the trees an attribute that designates whether they are within the internally-buffered region
  pred$core_area = sf::st_intersects(pred, obs_bound_core, sparse = FALSE) |> as.vector()

  # Assign a more descriptive name to the height column and core area column
  pred = pred |>
    rename(pred_tree_height = z,
           pred_tree_core_area = core_area)

  return(pred)

}

match_obs_to_pred_mee = function(obs, pred, search_distance_fun_intercept, search_distance_fun_slope, search_height_proportion) {

  dist_mat <- sf::st_distance(obs, pred)

  colnames(dist_mat) = pred$pred_tree_id
  rownames(dist_mat) = obs$obs_tree_id

  dist_graph = as.data.frame(as.table(dist_mat)) |>
    dplyr::mutate(Var1 = as.numeric(as.character(Var1)),
            Var2 = as.numeric(as.character(Var2))) |>
    dplyr::rename("obs_tree_id" = "Var1",
                  "pred_tree_id" = "Var2",
                  "dist" = "Freq")

  # Filter to only trees within the maximum matching distance
  tallest_obs_tree = max(obs$obs_tree_height)
  max_dist_window = search_distance_fun_intercept + search_distance_fun_slope * tallest_obs_tree
  dist_graph = dist_graph |>
    filter(dist <= max_dist_window)

  # Pull in each tree's height into the distance graph
  pred_height = pred |>
    dplyr::select(pred_tree_id, pred_tree_height)
  sf::st_geometry(pred_height) = NULL

  obs_height = obs %>%
    select(obs_tree_id, obs_tree_height)
  sf::st_geometry(obs_height) = NULL

  dist_graph = left_join(dist_graph, obs_height)
  dist_graph = left_join(dist_graph, pred_height)

  # Take every possible pairing of trees and filter to those within matching distance (horiz and vert)
  dist_graph = dist_graph |>
    dplyr::mutate(max_dist = search_distance_fun_intercept + search_distance_fun_slope * obs_tree_height) |>
    dplyr::filter(dist <= max_dist) |>
    dplyr::mutate(min_height = search_height_proportion * obs_tree_height,
                  max_height = (1 + search_height_proportion) * obs_tree_height) |>
    dplyr::filter(pred_tree_height >= min_height & pred_tree_height <= max_height) |>
    dplyr::arrange(dist)

  # Go through the list of possible pairings and find the best match for each observed tree. When
  # there are multiple potential matches, the shortest distance match is chosen, and the rest remain
  # eligible for matching

  pred_ids_matched = NULL
  obs_ids_matched = NULL

  for (i in seq_len(nrow(dist_graph))) {

    row = dist_graph[i,]
    if (row$pred_tree_id %in% pred_ids_matched || row$obs_tree_id %in% obs_ids_matched) {
      #already matched, no longer eligible
      next()
    } else {
      #this is a new match; record it
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
    filter(obs_tree_height >= min_height,
           obs_tree_core_area == TRUE) |>
    summarize(n_obs_match_pred = sum(!is.na(pred_tree_id) & !is.na(obs_tree_id)),
              n_obs = n())

  pred_obs_match_counts <- pred_obs_match |>
    filter(pred_tree_height >= min_height,
           pred_tree_core_area == TRUE) |>
    summarize(n_pred_match_obs = sum(!is.na(pred_tree_id) & !is.na(obs_tree_id)),
              n_pred = n()
    )

  matched_counts = cbind(obs_pred_match_counts, pred_obs_match_counts)
  matched_counts$min_height = min_height

  return(matched_counts)

}



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

  pred_obs_match <- left_join(pred_simple,
                              obs_simple,
                              by = c("pred_tree_id" = "matched_pred_tree_id"))

  obs_pred_match <- right_join(pred_simple,
                               obs_simple,
                               by = c("pred_tree_id" = "matched_pred_tree_id"))


  # Sum the tree counts (number of predicted trees, number of predicted trees matched, number of
  # observed trees, and number of observed trees matched) across height classes

  match_counts = count_total_and_matched_trees(obs_pred_match, pred_obs_match, min_height = min_height)

  # Compute recall, precision, f_score for individual tree detection
  match_stats <- match_counts |>
    mutate(recall = n_obs_match_pred / n_obs,
           precision = n_pred_match_obs / n_pred) |>
    mutate(f_score = 2 * recall * precision / (recall + precision))

  return(match_stats)
}
