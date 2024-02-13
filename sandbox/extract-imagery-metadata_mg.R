# This script has two purposes: 1. Demonstrate how to run functions to extract metadata from imagery
# dataset EXIF data, and 2. Demonstrate how to write a new metadata extraction function.

# --- Setup ---

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "michellegarcia-metadata-mac.txt"))


# --- 1. Workflow for running metadata extraction ---

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
exif_file = exif_files[1]

# Run for that one EXIF file.
extract_metadata_dy(exif_file, plot_flightpath = TRUE)
# ^ If you want to inspect the definition of this function, it is at the bottom of
# 'R/imagery-metadata-extraction_dy.R'.

# Run extraction on all EXIF files
metadata = purrr::map_dfr(exif_files, extract_metadata_dy, plot_flightpath = TRUE)
metadata

# Write results to file (creating directory if it doesn't exist)
out_dir = file.path(datadir, "extracted-metadata", "dataset-level-tabular")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write.csv(metadata, file.path(out_dir, "dataset-metadata_dy.csv"), row.names = FALSE)


# --- 2. Example of how to use this sandbox script to write a metadata extraction function ---

# Select an EXIF file to test on, and prep the EXIF data by loading it as a geospatial data frame
# using the 'prep_exif' function. The 'prep_exif' function is defined in
# 'R/imagery-metadata-extraction_general.R'            # nolint
exif_file = exif_files[1]
exif = prep_exif(exif_file)
# Note that the 'prep_exif' function returns the EXIF data as a geospatial data frame (an 'sf'
# object) with point geometry. So any geospatial operations you attempt on it should use functions
# from the 'sf' package. If you are more familiar with 'terra' objects and would rather work with
# them, let Derek know and we can create an option to return 'terra::vect' objects.

# Between the BEGIN and END comments below, write code to extract the metadata attribute you're
# working on. When you're done, you can wrap it in a function definition, taking only one parameter,
# 'exif'. Here is an example of developing code to extract the number of images in an imagery
# dataset.

# BEGIN FUNCTION CODE

# Get the number of images in the dataset by counting the rows of the EXIF data frame
image_count = nrow(exif)

# END FUNCTION CODE


# Now here is an example of turning that code into a function

extract_image_count = function(exif) {

  # Get the number of images in the dataset by counting the rows of the EXIF data frame
  image_count = nrow(exif)

  return(image_count)

}


# Now you can test the function on the EXIF data

image_count = extract_image_count(exif)
image_count

# Once it is working right, you can move this function to your
# 'R/imagery-metadata-extraction_<initials>.R' file and then add a call to this function from within
# your 'extract_metadata_<initials>' function. Once it is in there, then you can run the top part of
# this script again, and when it extracts the metadata for each EXIF dataset, your additional
# metadata should be included.

R/imagery-metadata-extraction_mg.R
