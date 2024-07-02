# Purpose: Read a CSV file containing the EXIF data for all images in a dataset (multiple missions),
# *at the mission level* (i.e., combining both dates of a two-date mission; both orientations of a
# two-part grid mission) and extract/process into human-readable metadata at the image level and
# sub-mission level using the metadata extraction functions of the ofo package.

library(tidyverse)

devtools::document(); devtools::install(); library(ofo)

IMAGERY_PROJECT_NAME = "2019-focal"

BASEROW_DATA_PATH = "/ofo-share/drone-imagery-organization/ancillary/baserow-snapshots"
FOLDER_BASEROW_CROSSWALK_PATH = "/ofo-share/drone-imagery-organization/1c_exif-for-sorting/"
EXIF_PATH = "/ofo-share/drone-imagery-organization/3b_exif-unprocessed/"


# Derived constants
exif_filepath = file.path(EXIF_PATH, paste0("exif_", IMAGERY_PROJECT_NAME, ".csv"))
crosswalk_filepath = file.path(FOLDER_BASEROW_CROSSWALK_PATH, paste0(IMAGERY_PROJECT_NAME, "_crosswalk.csv"))


## Workflow

# Read in the EXIF
exif = prep_exif(exif_filepath, plot_flightpath = FALSE)

# Format columns
exif = exif |>
  mutate(mission_id = str_pad(mission_id, 6, pad = "0", side = "left"))

# Assign the "dataset_id" parameter that is used in the metadata extraction functions. This is done
# here as opposed to in the metadata extraction to keep those functions flexible as to how a dataset
# is defined (e.g. a "mission" or a "sub-mission"). Here we are defining a dataset as a
# "sub-mission".
exif$dataset_id = exif$submission_id

# Extract image-level metadata, which can occur across all missions at once becuase there are no
# hierarchical dependencies on mission-level data
metadata_perimage = extract_imagery_perimage_metadata(exif,
                                                      input_type = "dataframe")

# For sub-mission-level metadata, run sub-mission by sub-mission
submissions = unique(exif$submission_id)

# For parallelizing, make a list of subsets of the exif dataframe, one for each submission
exif_list <- lapply(submissions, function(submission) {
  exif_foc <- exif |>
    filter(submission_id == submission)
  return(exif_foc)
})

# Run dataset-level metadata extraction across each subset

future::plan("multisession")
res = furrr::future_map(exif_list,
                        extract_imagery_dataset_metadata,
                        input_type = "dataframe",
                        plot_flightpath = FALSE,
                        crop_to_contiguous = TRUE)

metadata_perdataset = bind_rows(res)






# Pull in the baserow (human-entered) metadata
baserow = read_csv(file.path(BASEROW_DATA_PATH, "export - datasets-imagery.csv"))
dataset_associations = read.csv(file.path(BASEROW_DATA_PATH, "export - dataset-associations - Grid.csv"))


# Load the crosswalk linking folder names to baserow rows. If there is more than one sub-mission for
# a mission, they may or may not have different entries in Baserow, and their manually extracted
# exif may or may not have differences.
crosswalk = read_csv(crosswalk_filepath)

# We need to create a set of attributes at the mission level and the sub-mission level. At the
# mission level, we can generate the *derived* attributes from the exif data, but the *manually
# provided* attributes from Baserow like base station location and drone model will need to be
# pulled from both matching baserow rows and concatenated