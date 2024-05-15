# Functions to prepare for a photogrammetry run

# Update a value in a YAML for a specific key. If they key is nested below others, use "__" between
# the levels
# TODO: determine if this can be replaced with modifyList()
change_yaml_value = function(cfg, key, value) {

  # If the key is nested (specified by "__"), split it and recursively call this function
  if (stringr::str_detect(key, stringr::fixed("__"))) {
    key_parts = stringr::str_split(key, stringr::fixed("__"))[[1]]
    first_key = key_parts[1]
    remaining_keys = paste(key_parts[-1], collapse = "__")
    cfg_part = change_yaml_value(cfg[[first_key]], remaining_keys, value)
    cfg[[first_key]] = cfg_part
  } else {
    cfg[[key]] = value
  }

  return(cfg)
}

# If x is a list of one element that contains a list, extract the inner list. This nesting can happen
# when reading a list col from a data frame.
unnest_list = function(x) {
  if(is.list(x) && length(x) == 1) {
    return(x[[1]])
  } else {
    x = x[[1]]
  }

  return(x)
}


# Take a base metashape config (as a nested list) and a set of replacements (for one scenario), and
# write out a derived yaml file
make_derived_yaml = function(cfg_base, replacements, derived_yaml_dir) {

  cfg_filename = paste0(replacements$config_filename, ".yml")

  # remove the filename since that doesn't go into the yaml
  replacements = replacements |> dplyr::select(-"config_filename")

  cfg_derived = cfg_base

  # Replace the values in the base config with the values from the scenario
  for (key in names(replacements)) {
    replacement_value = replacements[[key]]
    replacement_value = unnest_list(replacement_value)
    cfg_derived = change_yaml_value(cfg_derived, key, replacement_value)
  }

  # Write back out to yaml
  out_filepath = file.path(derived_yaml_dir, cfg_filename)
  yaml::write_yaml(cfg_derived, out_filepath)

  # Return the filepath it was written to so the calling function knows
  return(out_filepath)
}

# Take a base yaml file and a set of scenarios (value replacements for specific keys) and write them
# all
make_derived_configs = function(base_yaml_filepath,
                                scenarios, derived_yaml_out_folder = "",
                                automate_metashape_path = "",
                                n_shell_splits = 1) {

  # if not specified, write derived yamls in same dir as the base
  if(derived_yaml_out_folder == "") {
    derived_yaml_out_folder = dirname(base_yaml_filepath)
  }

  if(!dir.exists(derived_yaml_out_folder)) {
    dir.create(derived_yaml_out_folder, recursive = TRUE)
  }

  config_files_created = NULL

  for (i in 1:nrow(scenarios)) {

    scenario = scenarios[i, ]

    cfg_base = yaml::read_yaml(base_yaml_filepath)
    filename = make_derived_yaml(cfg_base, scenario, derived_yaml_dir = derived_yaml_out_folder)

    config_files_created = c(config_files_created,filename)

  }

  if(automate_metashape_path != "") {
    ## make a shell script to run all the config files
    shell_lines = paste0("python3 ", file.path(automate_metashape_path, "python", "metashape_workflow.py"),
                         " ", config_files_created)

    writeLines(shell_lines,
               con = paste0(derived_yaml_out_folder,"/config_batch.sh"), sep="\n")

    # Optionally generate n_shell_splits shell scripts, each with a random subset of the config
    # files, sampling completely and without replacement
    if (n_shell_splits > 1) {

      # Randomize the order, in case really large projects are grouped together, we don't want them
      # all loaded on the same instance
      shell_lines = sample(shell_lines, length(shell_lines), replace = FALSE)

      shell_chunks = chunk_up(shell_lines, n = 8)

      for (i in 1:n_shell_splits) {
        writeLines(shell_chunks[[i]],
                   con = paste0(derived_yaml_out_folder,"/config_batch_",i-1,".sh"), sep="\n")
      }

    }

  }

}

# Convenience function to split a vector x into n chunks
chunk_up = function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))
