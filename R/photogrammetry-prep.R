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

# Take a base metashape config (as a nested list) and a set of replacements (for one scenario), and
# write out a derived yaml file
make_derived_yaml = function(cfg_base, replacements, derived_yaml_dir) {

  cfg_filename = paste0(replacements$config_filename, ".yml")

  # remove the filename since that doesn't go into the yaml
  replacements = replacements |> dplyr::select(-config_filename)

  cfg_derived = cfg_base

  # Replace the values in the base config with the values from the scenario
  for (key in names(replacements)) {
    replacement_value = replacements[[key]]
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
# 'base_yaml_filepath' must be an absolute filepath if the batch shell script creation is to work
make_derived_configs = function(base_yaml_filepath,
                                scenarios, derived_yaml_out_folder = "",
                                automate_metashape_path = "") {

  # if not specified, write derived yamls in same dir as the base
  if(derived_yaml_out_folder == "") {
    derived_yaml_out_folder = dirname(base_yaml_filepath)
  }

  config_files_created = NULL

  for(i in 1:nrow(scenarios)) {

    scenario = scenarios[i, ]

    cfg_base = yaml::read_yaml(base_yaml_filepath)
    filename = make_derived_yaml(cfg_base, scenario, derived_yaml_dir = derived_yaml_out_folder)

    config_files_created = c(config_files_created,filename)

  }

  if(metashape_path != "") {
    ## make a shell script to run all the config files
    shell_lines = paste0("python3 ", automate_metashape_path, "python", "metashape_workflow.py",
                         " ", config_files_created)

    writeLines(shell_lines,
               con = paste0(derived_yaml_out_folder,"/config_batch.sh"), sep="\n")
  }

}
