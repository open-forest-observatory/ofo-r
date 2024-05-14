
devtools::load_all()
datadir = readLines(file.path("sandbox", "data-dirs", "michellegarcia-metadata-mac.txt"))
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)
exif_files
exif_file = exif_files[1]


#START CODE FOR CENTROID LAT/LONG
library(stringr)
library(tidyverse)

#Getting the coordinates listed in the exif file and for it to seperate into long/lat

coordinate = sf::st_coordinates(exif)

#assigning the points
points <- st_as_sf(exif, coord = c(coordinate), crs = 4326)

# Making it a multipoint
multipoint <- st_union(points)

# Calculate the centroid of the MULTIPOINT geometry
centroid <- st_centroid(multipoint)

# Print the centroid to get the coordinates
print(centroid)
#Ploting to make sure it comes out as a single point
plot(centroid)

#END CODE FOR CENTOID CALCULATIONS



#START CODE FOR SOLAR NOON CALCULATIONS

library("suntools")

#Here we are seperating the date from the time, since we only need the date to run this function
datetimedf = separate(exif, col = capture_datetime, into = c("date", "time"), sep = " " )
datetimedf
date <- datetimedf[1, 143]$date
date
#now using the newly seperated date column and using the first row to get the date to be used in the function below

#used the general coordinates and input into this function

sncalc <- solarnoon(matrix(c(coordinate), nrow = 1),as.POSIXct(date),POSIXct.out=TRUE)

sncalc
#This gives the solar noon as a fraction of the day as well as the datetime.

#END CODE FOR SOLARNOON CALCULATION


#START CODE FOR EARLIEST/LATEST TIME

etl <- min(exif$capture_datetime)
earliest_time_local <- gsub("[[:punct:]]", "", as.character(etl))
earliest_time_local

ltl<- max(exif$capture_datetime)
latest_time_local <- gsub("[[:punct:]]", "", as.character(ltl))
latest_time_local

#END CODE FOR EARLIEST/LATEST TIME


