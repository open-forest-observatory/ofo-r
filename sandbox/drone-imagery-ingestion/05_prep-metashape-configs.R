# Purpose: Prepare the metashape config and shell files for processing all image sets

library(dplyr)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)

DATASET_DIR = "/ofo-share/drone-imagery-organization/3_sorted-notcleaned-combined" # Ultimately, will want to move this run on to 3_sorted_cleaned, but can run it on this upstream folder if we need data from the photogrammetry outputs (e.g. altitude AGL) in the interim
BASE_YAML_FILEPATH = "/ofo-share/repos-david/ofo-r/sandbox/drone-imagery-ingestion/full-run-configs/base.yml"
DERIVED_YAML_OUTFOLDER = "/ofo-share/scratch-david/2020-ucnrs-sfm/configs"
AUTOMATE_METASHAPE_PATH = "/ofo-share/repos-derek/automate-metashape"
METASHAPE_OUTPUT_PATH = "/ofo-share/drone-imagery-processed/01/metashape-outputs"
METASHAPE_PROJECT_PATH = "/ofo-share/drone-imagery-processed/01/metashape-projects"
N_SHELL_SPLITS = 4

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

make_derived_configs(
  base_yaml_filepath = BASE_YAML_FILEPATH,
  scenarios,
  derived_yaml_out_folder = derived_yaml_out_folder,
  automate_metashape_path = AUTOMATE_METASHAPE_PATH,
  n_shell_splits = N_SHELL_SPLITS
)
