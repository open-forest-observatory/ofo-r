# Purpose: There are many datasets that are composed of subdatasets (e.g., the E-W and N-S flights
# of an oblique grid mission). AT this stage, they have been identified an are stored in the top
# level of the folder `2_sorted-notcleaned` as, e.g., 000345-01 and 000345-02. Combine those into a
# single dataset "superfolder", with the subdataset folders beneath it.

library(tidyverse)
library(furrr)

IMAGERY_PROJECT_NAME = "2023-tahoe-aspen" # 2023-ny-ofo, 2022-early-regen

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
