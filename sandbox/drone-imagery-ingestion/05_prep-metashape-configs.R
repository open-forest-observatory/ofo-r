# Purpose: Prepare the metashape config and shell files for processing all image sets

library(dplyr)

IMAGERY_PROJECT_NAME = "2022-early-regen"

DATASET_DIR = "/ofo-share/drone-imagery-organization/2z_sorted-notcleaned-combined"  
BASE_YAML_FILEPATH = "/ofo-share/repos-derek/ofo-r/sandbox/drone-imagery-ingestion/full-run-configs/base.yml"
DERIVED_YAML_OUTFOLDER = "/ofo-share/repos-derek/ofo-r/sandbox/drone-imagery-ingestion/full-run-configs"
AUTOMATE_METASHAPE_PATH = "/ofo-share/repos-derek/automate-metashapa/python/metashape_workflow.py"
METASHAPE_OUTPUT_PATH = "/ofo-share/drone-imagery-processed/01/metashape-outputs"
METASHAPE_PROJECT_PATH = "/ofo-share/drone-imagery-processed/01/metashape-projects"

dataset_dir = file.path(DATASET_DIR, IMAGERY_PROJECT_NAME)
derived_yaml_out_folder = file.path(DERIVED_YAML_OUTFOLDER, IMAGERY_PROJECT_NAME)

devtools::load_all()

# Processing

# Get the list of datasets
datasets = list.dirs(dataset_dir, recursive = FALSE)

scenarios = data.frame()
for (dataset in datasets) {

  subdirs = list.dirs(dataset, recursive = FALSE)
  config_filename = basename(dataset)

  scenario = data.frame(photo_path = I(list(subdirs)),
                   config_filename = config_filename)
  
  scenarios = bind_rows(scenarios, scenario)
}

scenarios$output_path = METASHAPE_OUTPUT_PATH
scenarios$project_path = METASHAPE_PROJECT_PATH

scenarios

make_derived_configs(base_yaml_filepath = BASE_YAML_FILEPATH,
                     scenarios,
                     derived_yaml_out_folder = derived_yaml_out_folder,
                     automate_metashape_path = AUTOMATE_METASHAPE_PATH)
