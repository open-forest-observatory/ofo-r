prep_obs_map = function(obs, obs_bound, edge_buffer) {

  # TODO: Ensure predicted tree map is projected to same CRS as the observed tree map

  # Convert the maps to sf objects
  obs = sf::st_as_sf(obs, coords = c("x", "y"))

  # Prep an internally buffered region (to accommodate edge uncertainty)
  obs_bound_internal = obs_bound |> sf::st_buffer(-edge_buffer)
  obs_bound_internal$internal_area = TRUE

  # Assign an incremental unique ID to each observed tree
  obs$observed_tree_id = 1:nrow(obs)

  # Assign the trees an attribute that designates whether they are within the internally-buffered region
  obs$internal_area = sf::st_intersects(obs, obs_bound_internal, sparse = FALSE) |> as.vector()

  # Add an (empty for now) attribute that will store which predicted tree the observed tree is matched to
  obs$final_predicted_tree_match_id = NA

  return(obs)

}

prep_pred_map = function(pred, obs_bound, edge_buffer) {

  # TODO: Ensure predicted tree map is projected to same CRS as the observed tree map

  # Convert the maps to sf objects
  pred = sf::st_as_sf(pred, coords = c("x", "y"))

  # Prep an internally buffered region (to accommodate edge uncertainty)
  obs_bound_internal = obs_bound |> sf::st_buffer(-edge_buffer)
  obs_bound_internal$internal_area = TRUE

  # Crop the predicted map to the observed map
  pred = sf::st_intersection(pred, obs_bound)

  # Assign an incremental unique ID to each observed tree
  pred$predicted_tree_id = 1:nrow(pred)

  # Assign the trees an attribute that designates whether they are within the internally-buffered region
  pred$internal_area = sf::st_intersects(pred, obs_bound_internal, sparse = FALSE) |> as.vector()

  return(pred)

}

match_obs_to_pred = function(obs, pred, search_distance_fun_intercept, search_distance_fun_slope, search_height_proportion) {

  dist_mat <- sf::st_distance(obs, pred)

  colnames(dist_mat) = pred$predicted_tree_id
  rownames(dist_mat) = obs$observed_tree_id

  dist_graph = as.data.frame(as.table(dist_mat)) |>
    dplyr::mutate(Var1 = as.numeric(as.character(Var1)),
            Var2 = as.numeric(as.character(Var2))) %>%
    dplyr::rename("observed_tree_id" = "Var1",
            "predicted_tree_id" = "Var2",
            "dist" = "Freq") %>%
    dplyr::mutate(dist = as.numeric(dist))

  # Filter to only trees within the maximum matching distance
  tallest_obs_tree = max(obs$z)
  max_dist_window = search_distance_fun_intercept + search_distance_fun_slope * tallest_obs_tree
  dist_graph = dist_graph |>
    filter(dist <= max_dist_window)

  # pull in each tree's height
  pred_height = pred |>
    dplyr::select(predicted_tree_id, predicted_height = z)
  sf::st_geometry(pred_height) = NULL

  obs_height = obs %>%
    select(observed_tree_id, observed_height = z)
  sf::st_geometry(obs_height) = NULL

  dist_graph = left_join(dist_graph,obs_height)
  dist_graph = left_join(dist_graph,pred_height)

  # Take every possible pairing of trees, filter to those within matching distance (horiz and vert)
  dist_graph = dist_graph |>
    dplyr::mutate(max_dist = search_distance_fun_intercept + search_distance_fun_slope * observed_height) |>
    dplyr::filter(dist <= max_dist) |>
    dplyr::mutate(min_height = search_height_proportion * observed_height,
                  max_height = (1 + search_height_proportion) * observed_height) |>
    dplyr::filter(predicted_height >= min_height & predicted_height <= max_height) |>
    dplyr::arrange(dist)

  # Go through the list of possible pairings and find the best match for each observed tree. When
  # there are multiple potential matches, the shortest distance match is chosen, and the rest remain
  # eligible for matching

  predicted_ids_matched = NULL
  observed_ids_matched = NULL

  for (i in 1:nrow(dist_graph)) {

    row = dist_graph[i,]
    if (row$predicted_tree_id %in% predicted_ids_matched || row$observed_tree_id %in% observed_ids_matched) {
      #already matched, no longer eligible
      next()
    } else {
      #this is a new match; record it
      predicted_ids_matched = c(predicted_ids_matched, row$predicted_tree_id)
      observed_ids_matched = c(observed_ids_matched, row$observed_tree_id)
      obs[obs$observed_tree_id == row$observed_tree_id, "final_predicted_tree_match_id"] = row$predicted_tree_id
    }

  }

  return(obs)

}