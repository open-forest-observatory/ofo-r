# Load all the functions (and package dependencies) of this R package
devtools::load_all()
library(lubridate)

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "steven-metadata-laptop.txt"))

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
exif_file = exif_files[1]
exif = prep_exif(exif_file)

# Convert DateTimeOriginal to datetime objects
exif$DateTimeOriginal <- ymd_hms(gsub(":", " ", exif$DateTimeOriginal), tz = "UTC")

# Calculate start and end datetime_local
start_datetime_local <- min(exif$DateTimeOriginal)
end_datetime_local <- max(exif$DateTimeOriginal)

# Convert start and end datetime_local to the desired format (YYYYMMDD HHMMSS)
start_datetime_local_formatted <- format(start_datetime_local, "%Y%m%d %H%M%S")
end_datetime_local_formatted <- format(end_datetime_local, "%Y%m%d %H%M%S")

# Print the results
cat("Start of mission (local time):", start_datetime_local_formatted, "\n")
cat("End of mission (local time):", end_datetime_local_formatted, "\n")
