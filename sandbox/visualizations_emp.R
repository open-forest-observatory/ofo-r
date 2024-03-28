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
library(htmlwidgets)

## the map base we want to use with the leaflet package is Jawg.terrain. In order to use Jawg Maps, you must register (https://www.jawg.io/lab). Once registered, your access token will be located here (https://www.jawg.io/lab/access-tokens) and you will access to all Jawg default maps (variants) and your own customized maps. See the leaflet providers github for more info (https://github.com/leaflet-extras/leaflet-providers/blob/master/README.md)

#### set working directory and load ofo-r package ####

setwd("C:\\Users\\emily\\Desktop\\FOCAL\\ofo-r")

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "emp-metadata-laptop.txt"))

#### run image-level metadata extraction for the example EXIF files ####

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# run extraction on test EXIF file

exif_file = exif_files[1]
exif = prep_exif(exif_file)

metadata <- data.frame(extract_altitude_asl(exif), extract_received_image_path(exif), extract_lat_lon(exif), extract_dataset_id(exif)) %>% rename (altitude_asl = extract_altitude_asl.exif., received_image_path = extract_received_image_path.exif., dataset_id = extract_dataset_id.exif.)


#### merge metadata to original EXIF file-- later in the map creation process we need the DateTimeOriginal column instead of the new one we created in the metadata ####

# first we need to create a column in the exif data.frame that can be used to match each row of data with the correct image metadata. let's choose the image path because we know each image has a unique file pathway

exif$received_image_path <- extract_received_image_path(exif)

# now let's merge the exif file with the metadata file

totalexif <- full_join(exif, metadata, by="received_image_path")

#### Now we're going to create an intelligible "time of collection" value that leaflet knows how to work with for the exif file ####

# creating new datetime column

totalexif$datetime <- ymd_hms(totalexif$DateTimeOriginal)

# creating "duration in seconds from beginning of image to collection to time of [i] image" column. the number in brackets is the index position of the image that was collected first.

totalexif$duration <- as.duration(totalexif$datetime[1] %--% totalexif$datetime)

#### let's create spatial centroid values-- we need these to tell leaflet where to center the maps ####

exif_centroid_lat = mean(totalexif$lat)
exif_centroid_lon = mean(totalexif$lon)

#### exif interactive map ####

## define functions for continuous coloring

paletteNumASL <- colorNumeric('YlOrRd', domain = totalexif$altitude_asl)

paletteNumDateTime <- colorNumeric('BuPu', domain = totalexif$duration)

## the map

exif_MAP <-
  # create leaflet function using data specific to this map
  leaflet(totalexif) %>%
  # use the jawg.terrain basemap
  addTiles(
    'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%
  # set the center of the map to be the center ish of the flight footprint
  setView(
    lng=(exif_centroid_lon),
    lat=(exif_centroid_lat),
    zoom = 16
  ) %>%
  # add circles at each image location
  addCircleMarkers(
    ~lon,
    ~lat,
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
      "Image filepath:", totalexif$received_image_path, "<br>",
      "Collection date and time:", totalexif$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif$altitude_asl, " meters",        "<br>"),
    radius = 5,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 0
  ) %>%
  # add an interactive layer for ASL
  addCircleMarkers(
    ~lon,
    ~lat,
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumASL(totalexif$altitude_asl),
    group = "Altitude above sea level") %>%
  # add a legend for the ASL data
  addLegend(
    pal = paletteNumASL,
    values = totalexif$altitude_asl,
    title = '<small>Altitude Above <br> Sea Level (meters)</small>',
    opacity = 1,
    position = "bottomright",
    group = "Altitude above sea level"
  ) %>%
  # add an interactive layer for collection time
  addCircleMarkers(
    ~lon,
    ~lat,
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumDateTime(totalexif$duration),
    group = "Collection date and time") %>%
  # add a legend for the collection time data
  addLegend(
    pal = paletteNumDateTime,
    values = totalexif$duration,
    title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
    opacity = 1,
    position = "bottomleft",
    group = "Collection date and time"
  ) %>%
  # add layer control to toggle the overlay groups on and off
  addLayersControl(
    overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE))

exif_MAP

# save as html

dataset_id = totalexif$dataset_id.x[1]

saveWidget(exif_MAP, file= paste0("C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\visualizations\\image-map_", dataset_id, ".html"))

#### the above code works great! now I'm going to turn this into a function. I'm going to keep the above code because I think it contains a lot of useful #notes on what each part of the function does ####

flightpath_visualization_emp = function (exif_file) {

  exif = prep_exif(exif_file)

  metadata <- data.frame(extract_altitude_asl(exif), extract_received_image_path(exif), extract_lat_lon(exif), extract_dataset_id(exif)) %>% rename (altitude_asl = extract_altitude_asl.exif., received_image_path = extract_received_image_path.exif., dataset_id = extract_dataset_id.exif.)

  exif$received_image_path <- extract_received_image_path(exif)

  totalexif <- full_join(exif, metadata, by="received_image_path")

  totalexif$datetime <- lubridate::ymd_hms(totalexif$DateTimeOriginal)

  totalexif$duration <- lubridate::as.duration(totalexif$datetime[1] %--% totalexif$datetime)

  exif_centroid_lat = mean(totalexif$lat)

  exif_centroid_lon = mean(totalexif$lon)

  paletteNumASL <- colorNumeric('YlOrRd', domain = totalexif$altitude_asl)

  paletteNumDateTime <- colorNumeric('BuPu', domain = totalexif$duration)

  exif_MAP <-

    leaflet(totalexif) %>%

    addTiles(
      'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%

    setView(
      lng=(exif_centroid_lon),
      lat=(exif_centroid_lat),
      zoom = 16
    ) %>%

    addCircleMarkers(
      ~lon,
      ~lat,
      radius = 2,
      weight = 1,
      stroke = TRUE,
      fillOpacity = 1,
      color = "#008080"
    ) %>%

    addCircleMarkers(
      ~lon,
      ~lat,
      popup = paste(
        "Image filepath:", totalexif$received_image_path, "<br>",
        "Collection date and time:", totalexif$DateTimeOriginal, "<br>",
        "Drone altitude above sea level:", totalexif$altitude_asl, " meters",        "<br>"),
      radius = 5,
      weight = 1,
      stroke = FALSE,
      fillOpacity = 0
    ) %>%

    addCircleMarkers(
      ~lon,
      ~lat,
      radius = 2,
      weight = 1,
      stroke = FALSE,
      fillOpacity = 1,
      fillColor = ~paletteNumASL(totalexif$altitude_asl),
      group = "Altitude above sea level") %>%

    addLegend(
      pal = paletteNumASL,
      values = totalexif$altitude_asl,
      title = '<small>Altitude Above <br> Sea Level (meters)</small>',
      opacity = 1,
      position = "bottomright",
      group = "Altitude above sea level"
    ) %>%

    addCircleMarkers(
      ~lon,
      ~lat,
      radius = 2,
      weight = 1,
      stroke = FALSE,
      fillOpacity = 1,
      fillColor = ~paletteNumDateTime(totalexif$duration),
      group = "Collection date and time") %>%

    addLegend(
      pal = paletteNumDateTime,
      values = totalexif$duration,
      title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
      opacity = 1,
      position = "bottomleft",
      group = "Collection date and time"
    ) %>%

    addLayersControl(
      overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE))

  dataset_id = totalexif$dataset_id.x[1]

  saveWidget(exif_MAP, file= paste0("C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\visualizations\\image-map_", dataset_id, ".html"))

  return(exif_MAP)
}

# now I'm going to run exifs 2-6 through the function to generate maps

exif_file = exif_files[2]
flightpath_visualization_emp(exif_file)

exif_file = exif_files[3]
flightpath_visualization_emp(exif_file)

exif_file = exif_files[4]
flightpath_visualization_emp(exif_file)

exif_file = exif_files[5]
flightpath_visualization_emp(exif_file)

exif_file = exif_files[6]
flightpath_visualization_emp(exif_file)
