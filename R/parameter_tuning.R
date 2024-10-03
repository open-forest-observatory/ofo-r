create_parameter_grid = function(parameter_list, n_random_samples = NULL) {
  param_grid = expand.grid(parameter_list)
  if (!is.null(n_random_samples) && n_random_samples < nrow(param_grid)) {
    param_grid = param_grid[sample(nrow(param_grid), n_random_samples), ]
  }
  return(param_grid)
}


compute_metric_per_trial = function(results_df, eval_function, params_df = NULL) {
  # List of metrics from each run
  metric_values = list()
  # Iterate over all trials and compute the metric for each
  for (i in seq_len(nrow(results_df))) {
    # Get the shift predicted by the registration method
    results = results_df[i, ]
    # Get the map parameters defining the initial shift
    if (is.null(params_df)) {
      params = NULL
    } else {
      params = params_df[i, ]
    }
    # Compute a metric evaluating how well the predicted value matched the real one
    metric = eval_function(results, params)
    # Record this result
    metric_values[[i]] = metric
  }
  return(metric_values)
}

visualize_metric_vs_1_parameter = function(
    results_df,
    parameter_df,
    attributes_to_visualize,
    eval_function,
    smoothing_bandwidth = 5) {
  # Compute the metric values for each row in the results
  metric_values = compute_metric_per_trial(
    results_df = results_df,
    eval_function = eval_function,
    params_df = parameter_df
  )

  for (attribute in attributes_to_visualize) {
    # Create a data frame containing the attribute in question and the metric values
    df = data.frame(
      x = as.numeric(parameter_df[, attribute]),
      y = as.numeric(metric_values)
    )
    smooth_trend_line = as.data.frame(
      ksmooth(df$x, df$y, kernel = "normal", bandwidth = smoothing_bandwidth, n.points = 100)
    )

    p = ggplot2::ggplot(smooth_trend_line, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(data = df) +
      ggplot2::labs(x = attribute, y = "metric value")
    print(p)
  }
}

test_tree_registration = function(
    map_params,
    registration_methods,
    eval_function,
    per_method_registration_arguments = NULL,
    registration_method_names = NULL,
    n_random_samples = NULL) {
  # Get the names of all arguments of the function and their default values
  simulate_tree_maps_default_args = formals(simulate_tree_maps)

  # Build the list of all provided values while using defaults for unspecified values
  # Create an empty list
  all_map_params = list()
  # Iterate over the function arguments
  for (name in names(simulate_tree_maps_default_args)) {
    if (name %in% names(map_params)) {
      # If the name is in the named list of parameters use that value
      all_map_params[name] = map_params[name]
    } else {
      # Else, use the default value
      all_map_params[name] = simulate_tree_maps_default_args[name]
    }
    # If the value isn't a vector already make it a one-length vector containing that value
    if (!is.vector(all_map_params[[name]])) {
      all_map_params[name] = c(all_map_params[[name]])
    }
  }
  # Create the cross product of all parameter configurations
  all_map_param_configurations = create_parameter_grid(all_map_params, n_random_samples = n_random_samples)
  # Randomize the order so it's more likely that we hit errors early if they exist
  all_map_param_configurations = all_map_param_configurations[
    sample(nrow(all_map_param_configurations)),
  ]

  # Create names for each method if not provided
  if (is.null(registration_method_names)) {
    registration_method_names = as.character(seq_along(registration_methods))
  }
  # Raise error if the list of names isn't the same length as the list of methods
  # This should only happen if a list is provided but it is spurious
  if (length(registration_method_names) != length(registration_methods)) {
    stop("Number of methods and method names do not match")
  }

  # Initialize the list of lists for the results
  # The sub-lists for each registration method are indexed by the name of the method
  per_method_results = list()
  for (method_name in registration_method_names) {
    per_method_results[[method_name]] = vector(mode = "list", length = nrow(all_map_param_configurations))
  }

  # Iterate over the parameter configurations and conduct the registration for each one
  for (param_ind in seq_len(nrow(all_map_param_configurations))) {
    message(paste0("iteration: ", param_ind, " / ", nrow(all_map_param_configurations)))
    # Get one set of map parameters
    map_params = all_map_param_configurations[param_ind, ]
    # Create a simulated map based on these parameters
    simulated_map = do.call(simulate_tree_maps, map_params)

    # Iterate over the different registration methods
    for (reg_ind in seq_along(registration_methods)) {
      # Extract the method and method name
      registration_method = registration_methods[[reg_ind]]
      registration_method_name = registration_method_names[[reg_ind]]
      registration_arguments = per_method_registration_arguments[[reg_ind]]

      # Take the important arguments from the simulated map
      all_registration_args = simulated_map[cbind("pred", "obs", "obs_bounds")]
      if (!is.null(per_method_registration_arguments)) {
        all_registration_args = c(all_registration_args, registration_arguments)
      }
      all_registration_args = all_registration_args[!duplicated(names(all_registration_args))]

      # EXPENSIVE LINE
      # This is where registration actually occurs
      predicted_shift = do.call(registration_method, all_registration_args)
      # Save the result in the output list for the appropriate method
      per_method_results[[registration_method_name]][[param_ind]] = predicted_shift
    }
  }

  # RBind each list of lists into a dataframe
  for (registration_method_name in registration_method_names) {
    per_method_results[[registration_method_name]] = do.call(
      rbind, per_method_results[[registration_method_name]]
    )
  }

  # Return both the results and the parameters used to generate them
  return(
    list(
      per_method_results = per_method_results,
      all_map_param_configurations = all_map_param_configurations
    )
  )
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
