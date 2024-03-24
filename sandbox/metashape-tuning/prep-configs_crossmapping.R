devtools::load_all()
library(tidyverse)

datadir = readLines("sandbox/data-dirs/js2-itd-crossmapping.txt")

dataset_names = list.dirs(file.path(datadir, "drone-imagery-raw"), recursive = FALSE, full.names = FALSE)
all_dataset_paths = file.path(datadir, "drone-imagery-raw", dataset_names)

scenarios = data.frame("photo_path" = all_dataset_paths,
                       "config_filename" = dataset_names)

base_yaml_filepath = file.path(getwd(), "sandbox", "itd-crossmapping-photogrammetry-prep", "configs", "base.yml")


make_derived_configs(base_yaml_filepath, scenarios,
                     metashape_path = "/ofo-share/utils/automate-metashape/python/metashape_workflow.py")
