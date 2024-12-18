# Purpose: There are many datasets that are composed of subdatasets (e.g., the E-W and N-S flights
# of an oblique grid mission). AT this stage, they have been identified an are stored in the top
# level of the folder `2_sorted-notcleaned` as, e.g., 000345-01 and 000345-02. Combine those into a
# single dataset "superfolder", with the subdataset folders beneath it.

library(tidyverse)
library(furrr)

# Handle difference in how the current directory is set between debugging and command line call
if (file.exists("sandbox/drone-imagery-ingestion/imagery_project_name.txt")) {
  IMAGERY_PROJECT_NAME_FILE = "sandbox/drone-imagery-ingestion/imagery_project_name.txt"
} else {
  IMAGERY_PROJECT_NAME_FILE = "imagery_project_name.txt"
}
IMAGERY_PROJECT_NAME = read_lines(IMAGERY_PROJECT_NAME_FILE)

SORTED_IMAGERY_FOLDER = "/ofo-share/drone-imagery-organization/2_sorted-notcleaned"
COMBINED_IMAGERY_FOLDER = "/ofo-share/drone-imagery-organization/3_sorted-notcleaned-combined"

sorted_imagery_folder = file.path(SORTED_IMAGERY_FOLDER, IMAGERY_PROJECT_NAME)
combined_imagery_folder = file.path(COMBINED_IMAGERY_FOLDER, IMAGERY_PROJECT_NAME)

# Processing

# Load the folder list

folders = list.dirs(sorted_imagery_folder, recursive = FALSE)

d = data.frame(folders)

d = d |>
  mutate(dataset_subdataset = basename(folders)) |>
  mutate(dataset = str_split(dataset_subdataset, "-") |> map(1)) |>
  mutate(in_path = file.path(sorted_imagery_folder, dataset_subdataset),
         out_path = file.path(combined_imagery_folder, dataset, dataset_subdataset))

make_hardlinks = function(d_row) {
  in_path = d_row$in_path
  out_path = d_row$out_path

  infiles = list.files(in_path, full.names = TRUE, recursive = TRUE)
  outfiles = str_replace(infiles, in_path, out_path)

  outdirs = unique(dirname(outfiles))
  dir.create(outdirs, recursive = TRUE)

  for(file in infiles) {
    file_out = str_replace(file, in_path, out_path)
    file.link(file, file_out)
  }
}

d_list = d |> split(1:nrow(d))

future::plan(future::multisession)
future_walk(d_list, make_hardlinks)
