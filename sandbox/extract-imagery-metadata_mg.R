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


#START CODE FOR CENTROID LAT/LONG

GPSLocation = exif$GPSPosition

#splitting the list (was this step really necessary for my dataset? I'm not really sure this actually helped?)
coordinates <- strsplit(GPSLocation, split = ";")

# Separating the GPS from the whole exif file
almostdone <- strsplit(unlist(coordinates), split = ",")

#Creating a seperate dataframe with just the coordinates to call upon later
isolatedcoordinates <- as.data.frame(do.call(rbind, almostdone))

library(stringr)
#Need to download this package as it will help with the following functions
separator(df)

install.packages("tidyverse")
library(tidyverse)

df2 = separate(exif,
        col = GPSPosition,
        into = c("lat", "long"),
        sep = " " )
#Here this worked, instead of doing by number of characters, did it by seperating at
#the space in between the two (since the number of characters differed from row to row).

print(df2)
#printed out the new df that includes the "lat" and "long"

#when looking online, it says that the way to find the centroid lat/long is to
#just use the average of the lat and the average of the long

str(df2$lat)
str(df2$long)

#here checking what type of character the lat and long are within the
#dataset, if it is not numeric, will need to change it.

df2$lat = as.numeric(df2$lat)
df2$long = as.numeric(df2$long)

#had to convert to numeric numbers since it was thinking that the column was a
#character.

xcoordinate = mean(df2$lat, na.rm=TRUE)
ycoordinate = mean(df2$long, na.rm= TRUE)

#finding the mean of the lat and long, since this is all one mission,
#it should give us the centroid lat/long, at least from what I researched.

print(ycoordinate)
print(xcoordinate)

#END CODE FOR CENTOID CALCULATIONS



#START CODE FOR SOLAR NOON CALCULATIONS

#found the package suntools which will calculate the solar noon, this way we do not have to do
#any crazy calculations, it is all included in the function and only needs lat/long and the time.

install.packages("suntools")
library("suntools")

#Here we are seperating the date from the time, since we only need the date to run this function
datetime <- df2[1, "capture_datetime"]
datetime
datetimedf = separate(exif,
               col = capture_datetime,
               into = c("date", "time"),
               sep = " " )
datetimedf
date <- datetimedf[1, 143]$date
date
#now using the newly seperated date column and using the first row to get the date to be used in the function below

#used the centroid calculated in the above code chunk for the lat/long
solarnoon <- solarnoon(
  matrix(c(xcoordinate, ycoordinate), nrow = 1),
  as.POSIXct(date),
  POSIXct.out=TRUE
)

solarnoon
#This gives the solar noon as a fraction of the day as well as the datetime.

#END CODE FOR SOLARNOON CALCULATION







