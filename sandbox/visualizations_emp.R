#### Purpose: create web maps of image points (one web map per mission). These interactive maps will have options to color the points by time and altitude AGL/ATL, for visual inspection and outliar detection ####

## Author: Emily Marie Purvis. Date: 2-25-2024

## Output file: imagery-metadata-dev/extracted-metadata/visualizations/image-map_<dataset_id>.html

#### Load packages ####

library(leaflet)
library(leaflet.providers)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(stringr)

## the map base we want to use with the leaflet package is Jawg.terrain. In order to use Jawg Maps, you must register (https://www.jawg.io/lab). Once registered, your access token will be located here (https://www.jawg.io/lab/access-tokens) and you will access to all Jawg default maps (variants) and your own customized maps. See the leaflet providers github for more info (https://github.com/leaflet-extras/leaflet-providers/blob/master/README.md)

#### Set working directory ####

setwd ("C:\\Users\\emily\\Box\\imagery-metadata-dev\\")










#### load ofo-r package ####

setwd("C:\\Users\\emily\\OneDrive\\Desktop\\FOCAL\\ofo-r")

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "emp-metadata-laptop.txt"))

#### run image-level metadata extraction for the example exif files ####

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# Define which test EXIF file to run the functions on
#exif_file = exif_files[1]

# Run for test EXIF file.
#extract_metadata_emp(exif_file)

# Run extraction on all EXIF files

exif_file = exif_files[1]
exif = prep_exif(exif_file)

metadata1 <- extract_metadata_emp(exif_file)

datasetid1 <- extract_dataset_id(exif)

datatimelocal1 <- extract_datatime_local(exif)

aperture1 <- extract_aperture(exif)



#### Load data ####

## original data

exif_20220630_0041_original <- read.csv ("exif-examples\\exif_20220630-0041.csv")

exif_20220730_0079_original <- read.csv ("exif-examples\\exif_20220730-0079.csv")

exif_20230528_0008_original <- read.csv ("exif-examples\\exif_20230528-0008.csv")

exif_20230528_0009_original <- read.csv ("exif-examples\\exif_20230528-0009.csv")

exif_20230706_0152_original <- read.csv ("exif-examples\\exif_20230706-0152.csv")

exif_20230706_0153_original <- read.csv ("exif-examples\\exif_20230706-0153.csv")

## compiled image-level metadata. after loading each metadata file, add a row index ("X") identical to the row index in the original data files-- this will allow us to easily merge the two types of files in the next step.

exif_20220630_0041_metadata <- read.csv ("extracted-metadata\\image-level-metadata\\image-metadata_20220630-0041.csv")

exif_20220630_0041_metadata$X <- 1:nrow(exif_20220630_0041_metadata)

exif_20220730_0079_metadata <- read.csv ("extracted-metadata\\image-level-metadata\\image-metadata_20220730-0079.csv")

exif_20220730_0079_metadata$X <- 1:nrow(exif_20220730_0079_metadata)

exif_20230528_0008_metadata <- read.csv ("extracted-metadata\\image-level-metadata\\image-metadata_20230528-0008.csv")

exif_20230528_0008_metadata$X <- 1:nrow(exif_20230528_0008_metadata)

exif_20230528_0009_metadata <- read.csv ("extracted-metadata\\image-level-metadata\\image-metadata_20230528-0009.csv")

exif_20230528_0009_metadata$X <- 1:nrow(exif_20230528_0009_metadata)

exif_20230706_0152_metadata <- read.csv ("extracted-metadata\\image-level-metadata\\image-metadata_20230706-0152.csv")

exif_20230706_0152_metadata$X <- 1:nrow(exif_20230706_0152_metadata)

exif_20230706_0153_metadata <- read.csv ("extracted-metadata\\image-level-metadata\\image-metadata_20230706-0153.csv")

exif_20230706_0153_metadata$X <- 1:nrow(exif_20230706_0153_metadata)

## merge original data and image level metadata

exif_20220630_0041 <- full_join(exif_20220630_0041_original, exif_20220630_0041_metadata, by="X")

exif_20220730_0079 <- full_join(exif_20220730_0079_original, exif_20220730_0079_metadata, by="X")

exif_20230528_0008 <- full_join(exif_20230528_0008_original, exif_20230528_0008_metadata, by="X")

exif_20230528_0009 <- full_join(exif_20230528_0009_original, exif_20230528_0009_metadata, by="X")

exif_20230706_0152 <- full_join(exif_20230706_0152_original, exif_20230706_0152_metadata, by="X")

exif_20230706_0153 <- full_join(exif_20230706_0153_original, exif_20230706_0153_metadata, by="X")

#### exif_20220630-0041 interactive map ####

## create an intelligible "time of collection" value

# creating datetime column
exif_20220630_0041_datetime <- data.frame(datetime = ymd_hms(exif_20220630_0041$DateTimeOriginal))

# extracting time from the datetime column
# exif_20220630_0041_datetime$time <- format(as.POSIXct(
#  exif_20220630_0041_datetime$datetime),format = "%H:%M:%S")

# creating "duration in seconds from beginning of image to collection to time of [i] image" column
exif_20220630_0041_datetime$duration <- as.duration(exif_20220630_0041_datetime$datetime[739] %--% exif_20220630_0041_datetime$datetime)

# assigning X to this new dataframe to allow it to be merged into the pre-existing one
exif_20220630_0041_datetime$X <- 1:nrow(exif_20220630_0041_datetime)

# merging
exif_20220630_0041 <- full_join(exif_20220630_0041, exif_20220630_0041_datetime, by="X")

## define functions for continuous coloring

paletteNumASL <- colorNumeric('YlOrRd', domain = exif_20220630_0041$altitude_asl)

paletteNumDateTime <- colorNumeric('BuPu', domain = exif_20220630_0041$duration)

## the map

exif_20220630_0041_MAP <-
  # create leaflet function using data specific to this map
  leaflet(exif_20220630_0041) %>%
  # use the jawg.terrain basemap
  addTiles(
    'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%
  # set the center of the map to be the center ish of the flight footprint
  setView(
    lng=(exif_20220630_0041$lon[888]),
    lat=(exif_20220630_0041$lat[888]),
    zoom = 16
    ) %>%
  # add circles at each image location
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
    "Image filepath:", exif_20220630_0041$received_image_path, "<br>",
    "Collection date and time:", exif_20220630_0041$DateTimeOriginal, "<br>",
    "Drone altitude above sea level:", exif_20220630_0041$altitude_asl, " meters",        "<br>"),
    radius = 2,
    weight = 1,
    stroke = TRUE,
    fillOpacity = 1,
    color = "#008080"
    ) %>%
  # add secret invisible circles at each image location with a bigger radius so they're actually clickable
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
      "Image filepath:", exif_20220630_0041$received_image_path, "<br>",
      "Collection date and time:", exif_20220630_0041$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", exif_20220630_0041$altitude_asl, " meters",        "<br>"),
    radius = 5,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 0
  ) %>%
  # add an interactive layer for ASL
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
      "Image filepath:", exif_20220630_0041$received_image_path, "<br>",
      "Drone altitude above sea level:", exif_20220630_0041$altitude_asl, " meters",        "<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumASL(exif_20220630_0041$altitude_asl),
    group = "Altitude above sea level") %>%
  # add a legend for the ASL data
  addLegend(
    pal = paletteNumASL,
    values = exif_20220630_0041$altitude_asl,
    title = '<small>Altitude Above <br> Sea Level (meters)</small>',
    opacity = 1,
    position = "bottomright",
    group = "Altitude above sea level"
  ) %>%
  # add an interactive layer for collection time
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
      "Image filepath:", exif_20220630_0041$received_image_path, "<br>",
      "Collection date and time:", exif_20220630_0041$DateTimeOriginal, "<br>",
      "Collection time after image acquisition initiation",                                 exif_20220630_0041$duration,"<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumDateTime(exif_20220630_0041$duration),
    group = "Collection date and time") %>%
  # add a legend for the collection time data
  addLegend(
    pal = paletteNumDateTime,
    values = exif_20220630_0041$duration,
    title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
    opacity = 1,
    position = "bottomleft",
    group = "Collection date and time"
  ) %>%
  # add layer control to toggle the overlay groups on and off
  addLayersControl(
    overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE))

exif_20220630_0041_MAP







# save as html
install.packages("htmlwidgets")
install.packages("leaflet")
library(htmlwidgets)
saveWidget(exif_20220630_0041_MAP, file="C:\\Users\\emily\\OneDrive\\Desktop\\draft_visualization.html")




# add a legend for the base map
addLegend(
  colors = "#008080",
  labels = "image locations",
  title = "exif_20220630_0041",
  opacity = 1,
  position = "bottomright"
)





