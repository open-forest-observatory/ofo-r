create_parameter_grid = function(parameter_list, n_random_samples = NULL) {
  #' Create a grid search of parameters and optionally take a random subset
  #'
  #' @param parameter_list A named list of parameters. Each element is either a list of values or a single value
  #' @param n_random_samples How many samples to draw from the cross product list. If NULL, all elements will be taken
  #'
  #' @return A dataframe where each row contains one value from each parameter drawn from the list

  # Create the cross product of all elements in the list. The column names will be taken from the
  # names in the list
  param_grid = expand.grid(parameter_list)

  # Take a random sampling of rows if requested and the number is lower than the number of rows
  if (!is.null(n_random_samples) && n_random_samples < nrow(param_grid)) {
    param_grid = param_grid[sample(nrow(param_grid), n_random_samples), ]
  }
  return(param_grid)
}


compute_metric_per_trial = function(results_df, eval_function, params_df = NULL) {
  #' Compute a scalar metric from each trial of an experiment.
  #'
  #' @param results_df A table of results, one row per trial
  #' @param eval_function A function to evaluate each row, using the result and (optionally-null) parameters
  #' @param params_df An optionally-null dataframe of paramters, one row per trial
  #'
  #' @return The list of metric values, one per row
  #' The eval_function is run on the corresponding pairs of rows from results_df and params_df

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
    smoothing_bandwidth = 5,
    title = "") {
  #' Visualize one metric versus one or more paramter values
  #'
  #' @param results_df The results of trials, one per row
  #' @param paramter_df The paramters used to generate the results, one row per trial
  #' @param attributes_to_visualize Which paramters to visualize the metrics versus
  #' @param eval_function How to evalute the quality of the result
  #' @param smoothing_bandwidth Width of the kernel used to smooth the trend line. May be a scalar or one value per attribute being visualized.
  #' @param title Title of the plot


  # Compute the metric values for each row in the results
  metric_values = compute_metric_per_trial(
    results_df = results_df,
    eval_function = eval_function,
    params_df = parameter_df
  )

  # If the bandwidth is a single value, duplicate it to the number of attributes being visualized
  if (!is.vector(smoothing_bandwidth) || length(smoothing_bandwidth) == 1) {
    smoothing_bandwidth = rep(smoothing_bandwidth, length(attributes_to_visualize))
  }

  # Visualize each attribute
  for (i in seq_along(attributes_to_visualize)) {
    # Create a data frame containing the attribute in question and the metric values
    attribute = attributes_to_visualize[i]
    df = data.frame(
      x = as.numeric(parameter_df[, attribute]),
      y = as.numeric(metric_values)
    )
    # Create a smooth trend line using a smoothing kernel
    smooth_trend_line = as.data.frame(
      ksmooth(
        df$x,
        df$y,
        kernel = "normal",
        bandwidth = smoothing_bandwidth[[i]],
        n.points = 100
      )
    )

    # Plot both the scatter plot and the smooth trend line
    p = ggplot2::ggplot(smooth_trend_line, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(data = df) +
      ggplot2::labs(x = attribute, y = "metric value") +
      ggplot2::ggtitle(title)
    print(p)
  }
}

test_tree_registration = function(
    map_params,
    registration_methods,
    per_method_registration_arguments = NULL,
    registration_method_names = NULL,
    n_random_samples = NULL) {
  #' Test the different tree registration algorithms in a variety of situations
  #'
  #' @param map_params Named list. The names correspond to arguments to simulate_tree_maps. The values
  #' correspond to potential values of this argument to be tried
  #' @param registration_methods List of registration algorithms
  #' @param per_method_registration_arguments A list of named lists, one for each registration method.
  #' The arguments in each list will be passed to the corresponding function
  #' @param registration_method_names The human-readable names for each method
  #' @param n_random_samples The number of random samples to draw from the grid of paramters
  #'
  #' @returns Named list with the following fields
  #'   per_method_results: Named list, where each name is from registration_method_names. The value is a data
  #'     frame of results from the experiments using that method.
  #'   all_map_param_configurations: Dataframe of paramters used in experiments, one row per trial

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

  # Concatenate each row into a single dataframe
  for (registration_method_name in registration_method_names) {
    per_method_results[[registration_method_name]] = as.data.frame(
      dplyr::bind_rows(per_method_results[[registration_method_name]])
    )
  }

  # Return both the results and the parameters used to generate them
  return(
    list(
      per_method_results = per_method_results,
      all_map_param_configurations = all_map_param_configurations
    )
  )
}
