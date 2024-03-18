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

setwd("C:\\Users\\emily\\OneDrive\\Desktop\\FOCAL\\ofo-r")

# Load all the functions (and package dependencies) of this R package
devtools::load_all()

# Define the root of the local data directory
datadir = readLines(file.path("sandbox", "data-dirs", "emp-metadata-laptop.txt"))

#### run image-level metadata extraction for the example EXIF files ####

# Get a list of the files containing the test EXIF data (one file per image dataset). These files
# have already been created and saved into the project data folder.
exif_files = list.files(file.path(datadir, "exif-examples"), pattern = "^exif.+\\.csv$", full.names = TRUE)

# run extraction on test EXIF file

exif_file1 = exif_files[1]
exif1 = prep_exif(exif_file1)

metadata1 <- extract_metadata_emp(exif_file1)

# Run extraction on all EXIF files

exif_file2 = exif_files[2]
exif2 = prep_exif(exif_file2)
metadata2 <- extract_metadata_emp(exif_file2)

exif_file3 = exif_files[3]
exif3 = prep_exif(exif_file3)
metadata3 <- extract_metadata_emp(exif_file3)

exif_file4 = exif_files[4]
exif4 = prep_exif(exif_file4)
metadata4 <- extract_metadata_emp(exif_file4)

exif_file5 = exif_files[5]
exif5 = prep_exif(exif_file5)
metadata5 <- extract_metadata_emp(exif_file5)

exif_file6 = exif_files[6]
exif6 = prep_exif(exif_file6)
metadata6 <- extract_metadata_emp(exif_file6)

#### merge metadata to original EXIF files-- later in the map creation process we need the DateTimeOriginal column instead of the new one we created in the metadata ####

# first we need to create a column in the exif data.frames that can be used to match each row of data with the correct image metadata. let's choose the image path because we know each image has a unique file pathway

exif1$received_image_path <- extract_received_image_path(exif1)

exif2$received_image_path <- extract_received_image_path(exif2)

exif3$received_image_path <- extract_received_image_path(exif3)

exif4$received_image_path <- extract_received_image_path(exif4)

exif5$received_image_path <- extract_received_image_path(exif5)

exif6$received_image_path <- extract_received_image_path(exif6)

# now let's merge the exif files with the metadata files

totalexif1 <- full_join(exif1, metadata1, by="received_image_path")

totalexif2 <- full_join(exif2, metadata2, by="received_image_path")

totalexif3 <- full_join(exif3, metadata3, by="received_image_path")

totalexif4 <- full_join(exif4, metadata4, by="received_image_path")

totalexif5 <- full_join(exif5, metadata5, by="received_image_path")

totalexif6 <- full_join(exif6, metadata6, by="received_image_path")

#### Now we're going to create an intelligible "time of collection" value that leaflet knows how to work with for each exif file ####

# creating new datetime column

totalexif1$datetime <- ymd_hms(totalexif1$DateTimeOriginal)

totalexif2$datetime <- ymd_hms(totalexif2$DateTimeOriginal)

totalexif3$datetime <- ymd_hms(totalexif3$DateTimeOriginal)

totalexif4$datetime <- ymd_hms(totalexif4$DateTimeOriginal)

totalexif5$datetime <- ymd_hms(totalexif5$DateTimeOriginal)

totalexif6$datetime <- ymd_hms(totalexif6$DateTimeOriginal)

# creating "duration in seconds from beginning of image to collection to time of [i] image" column. the number in brackets is the index position of the image that was collected first.

totalexif1$duration <- as.duration(totalexif1$datetime[1] %--% totalexif1$datetime)

totalexif2$duration <- as.duration(totalexif2$datetime[1] %--% totalexif2$datetime)

totalexif3$duration <- as.duration(totalexif3$datetime[1] %--% totalexif3$datetime)

totalexif4$duration <- as.duration(totalexif4$datetime[1] %--% totalexif4$datetime)

totalexif5$duration <- as.duration(totalexif5$datetime[1] %--% totalexif5$datetime)

totalexif6$duration <- as.duration(totalexif6$datetime[1] %--% totalexif6$datetime)

#### exif1 interactive map ####

## define functions for continuous coloring

paletteNumASL1 <- colorNumeric('YlOrRd', domain = totalexif1$altitude_asl)

paletteNumDateTime1 <- colorNumeric('BuPu', domain = totalexif1$duration)

## the map

exif1_MAP <-
  # create leaflet function using data specific to this map
  leaflet(totalexif1) %>%
  # use the jawg.terrain basemap
  addTiles(
    'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%
  # set the center of the map to be the center ish of the flight footprint
  setView(
    lng=(totalexif1$lon[888]),
    lat=(totalexif1$lat[888]),
    zoom = 16
    ) %>%
  # add circles at each image location
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
    "Image filepath:", totalexif1$received_image_path, "<br>",
    "Collection date and time:", totalexif1$DateTimeOriginal, "<br>",
    "Drone altitude above sea level:", totalexif1$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif1$received_image_path, "<br>",
      "Collection date and time:", totalexif1$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif1$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif1$received_image_path, "<br>",
      "Drone altitude above sea level:", totalexif1$altitude_asl, " meters",        "<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumASL1(totalexif1$altitude_asl),
    group = "Altitude above sea level") %>%
  # add a legend for the ASL data
  addLegend(
    pal = paletteNumASL1,
    values = totalexif1$altitude_asl,
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
      "Image filepath:", totalexif1$received_image_path, "<br>",
      "Collection date and time:", totalexif1$DateTimeOriginal, "<br>",
      "Collection time after image acquisition initiation",                                 totalexif1$duration,"<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumDateTime1(totalexif1$duration),
    group = "Collection date and time") %>%
  # add a legend for the collection time data
  addLegend(
    pal = paletteNumDateTime1,
    values = totalexif1$duration,
    title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
    opacity = 1,
    position = "bottomleft",
    group = "Collection date and time"
  ) %>%
  # add layer control to toggle the overlay groups on and off
  addLayersControl(
    overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE)) %>%
# add a legend for the base map
  addLegend(
    colors = "#008080",
    labels = "image locations",
    title = paste ("exif", totalexif1$dataset_id.x[1]),
    opacity = 1,
    position = "topleft"
  )

exif1_MAP

# save as html

saveWidget(exif1_MAP, file="C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\visualizations\\image-map_20220630-0041.html")

#### exif2 interactive map ####

## define functions for continuous coloring

paletteNumASL2 <- colorNumeric('YlOrRd', domain = totalexif2$altitude_asl)

paletteNumDateTime2 <- colorNumeric('BuPu', domain = totalexif2$duration)

## the map

exif2_MAP <-
  # create leaflet function using data specific to this map
  leaflet(totalexif2) %>%
  # use the jawg.terrain basemap
  addTiles(
    'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%
  # set the center of the map to be the center ish of the flight footprint
  setView(
    lng=(totalexif2$lon[2121]),
    lat=(totalexif2$lat[1470]),
    zoom = 15
  ) %>%
  # add circles at each image location
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
      "Image filepath:", totalexif2$received_image_path, "<br>",
      "Collection date and time:", totalexif2$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif2$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif2$received_image_path, "<br>",
      "Collection date and time:", totalexif2$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif2$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif2$received_image_path, "<br>",
      "Drone altitude above sea level:", totalexif2$altitude_asl, " meters",        "<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumASL2(totalexif2$altitude_asl),
    group = "Altitude above sea level") %>%
  # add a legend for the ASL data
  addLegend(
    pal = paletteNumASL2,
    values = totalexif2$altitude_asl,
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
      "Image filepath:", totalexif2$received_image_path, "<br>",
      "Collection date and time:", totalexif2$DateTimeOriginal, "<br>",
      "Collection time after image acquisition initiation",                                 totalexif2$duration,"<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumDateTime2(totalexif2$duration),
    group = "Collection date and time") %>%
  # add a legend for the collection time data
  addLegend(
    pal = paletteNumDateTime2,
    values = totalexif2$duration,
    title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
    opacity = 1,
    position = "bottomleft",
    group = "Collection date and time"
  ) %>%
  # add layer control to toggle the overlay groups on and off
  addLayersControl(
    overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE)) %>%
  # add a legend for the base map
  addLegend(
    colors = "#008080",
    labels = "image locations",
    title = paste ("exif", totalexif2$dataset_id.x[1]),
    opacity = 1,
    position = "topleft"
  )

exif2_MAP

# save as html

saveWidget(exif2_MAP, file="C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\visualizations\\image-map_20220730-0079.html")

#### exif3 interactive map ####

## define functions for continuous coloring

paletteNumASL3 <- colorNumeric('YlOrRd', domain = totalexif3$altitude_asl)

paletteNumDateTime3 <- colorNumeric('BuPu', domain = totalexif3$duration)

## the map

exif3_MAP <-
  # create leaflet function using data specific to this map
  leaflet(totalexif3) %>%
  # use the jawg.terrain basemap
  addTiles(
    'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%
  # set the center of the map to be the center ish of the flight footprint
  setView(
    lng=(totalexif3$lon[119]),
    lat=(totalexif3$lat[202]),
    zoom = 17
  ) %>%
  # add circles at each image location
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
      "Image filepath:", totalexif3$received_image_path, "<br>",
      "Collection date and time:", totalexif3$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif3$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif3$received_image_path, "<br>",
      "Collection date and time:", totalexif3$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif3$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif3$received_image_path, "<br>",
      "Drone altitude above sea level:", totalexif3$altitude_asl, " meters",        "<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumASL3(totalexif3$altitude_asl),
    group = "Altitude above sea level") %>%
  # add a legend for the ASL data
  addLegend(
    pal = paletteNumASL3,
    values = totalexif3$altitude_asl,
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
      "Image filepath:", totalexif3$received_image_path, "<br>",
      "Collection date and time:", totalexif3$DateTimeOriginal, "<br>",
      "Collection time after image acquisition initiation",                                 totalexif3$duration,"<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumDateTime3(totalexif3$duration),
    group = "Collection date and time") %>%
  # add a legend for the collection time data
  addLegend(
    pal = paletteNumDateTime3,
    values = totalexif3$duration,
    title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
    opacity = 1,
    position = "bottomleft",
    group = "Collection date and time"
  ) %>%
  # add layer control to toggle the overlay groups on and off
  addLayersControl(
    overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE)) %>%
  # add a legend for the base map
  addLegend(
    colors = "#008080",
    labels = "image locations",
    title = paste ("exif", totalexif3$dataset_id.x[1]),
    opacity = 1,
    position = "topleft"
  )

exif3_MAP

# save as html

saveWidget(exif3_MAP, file="C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\visualizations\\image-map_20230528-0008.html")

#### exif4 interactive map ####

## define functions for continuous coloring

paletteNumASL4 <- colorNumeric('YlOrRd', domain = totalexif4$altitude_asl)

paletteNumDateTime4 <- colorNumeric('BuPu', domain = totalexif4$duration)

## the map

exif4_MAP <-
  # create leaflet function using data specific to this map
  leaflet(totalexif4) %>%
  # use the jawg.terrain basemap
  addTiles(
    'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%
  # set the center of the map to be the center ish of the flight footprint
  setView(
    lng=(totalexif4$lon[66]),
    lat=(totalexif4$lat[54]),
    zoom = 17
  ) %>%
  # add circles at each image location
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
      "Image filepath:", totalexif4$received_image_path, "<br>",
      "Collection date and time:", totalexif4$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif4$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif4$received_image_path, "<br>",
      "Collection date and time:", totalexif4$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif4$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif4$received_image_path, "<br>",
      "Drone altitude above sea level:", totalexif4$altitude_asl, " meters",        "<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumASL4(totalexif4$altitude_asl),
    group = "Altitude above sea level") %>%
  # add a legend for the ASL data
  addLegend(
    pal = paletteNumASL4,
    values = totalexif4$altitude_asl,
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
      "Image filepath:", totalexif4$received_image_path, "<br>",
      "Collection date and time:", totalexif4$DateTimeOriginal, "<br>",
      "Collection time after image acquisition initiation",                                 totalexif4$duration,"<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumDateTime4(totalexif4$duration),
    group = "Collection date and time") %>%
  # add a legend for the collection time data
  addLegend(
    pal = paletteNumDateTime4,
    values = totalexif4$duration,
    title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
    opacity = 1,
    position = "bottomleft",
    group = "Collection date and time"
  ) %>%
  # add layer control to toggle the overlay groups on and off
  addLayersControl(
    overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE)) %>%
  # add a legend for the base map
  addLegend(
    colors = "#008080",
    labels = "image locations",
    title = paste ("exif", totalexif4$dataset_id.x[1]),
    opacity = 1,
    position = "topleft"
  )

exif4_MAP

# save as html

saveWidget(exif4_MAP, file="C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\visualizations\\image-map_20230528-0009.html")

#### exif5 interactive map ####

## define functions for continuous coloring

paletteNumASL5 <- colorNumeric('YlOrRd', domain = totalexif5$altitude_asl)

paletteNumDateTime5 <- colorNumeric('BuPu', domain = totalexif5$duration)

## the map

exif5_MAP <-
  # create leaflet function using data specific to this map
  leaflet(totalexif5) %>%
  # use the jawg.terrain basemap
  addTiles(
    'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%
  # set the center of the map to be the center ish of the flight footprint
  setView(
    lng=(totalexif5$lon[505]),
    lat=(totalexif5$lat[805]),
    zoom = 17
  ) %>%
  # add circles at each image location
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
      "Image filepath:", totalexif5$received_image_path, "<br>",
      "Collection date and time:", totalexif5$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif5$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif5$received_image_path, "<br>",
      "Collection date and time:", totalexif5$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif5$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif5$received_image_path, "<br>",
      "Drone altitude above sea level:", totalexif5$altitude_asl, " meters",        "<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumASL5(totalexif5$altitude_asl),
    group = "Altitude above sea level") %>%
  # add a legend for the ASL data
  addLegend(
    pal = paletteNumASL5,
    values = totalexif5$altitude_asl,
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
      "Image filepath:", totalexif5$received_image_path, "<br>",
      "Collection date and time:", totalexif5$DateTimeOriginal, "<br>",
      "Collection time after image acquisition initiation",                                 totalexif5$duration,"<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumDateTime5(totalexif5$duration),
    group = "Collection date and time") %>%
  # add a legend for the collection time data
  addLegend(
    pal = paletteNumDateTime5,
    values = totalexif5$duration,
    title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
    opacity = 1,
    position = "bottomleft",
    group = "Collection date and time"
  ) %>%
  # add layer control to toggle the overlay groups on and off
  addLayersControl(
    overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE)) %>%
  # add a legend for the base map
  addLegend(
    colors = "#008080",
    labels = "image locations",
    title = paste ("exif", totalexif5$dataset_id.x[1]),
    opacity = 1,
    position = "topleft"
  )

exif5_MAP

# save as html

saveWidget(exif5_MAP, file="C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\visualizations\\image-map_20230706-0152.html")

#### exif6 interactive map ####

## define functions for continuous coloring

paletteNumASL6 <- colorNumeric('YlOrRd', domain = totalexif6$altitude_asl)

paletteNumDateTime6 <- colorNumeric('BuPu', domain = totalexif6$duration)

## the map

exif6_MAP <-
  # create leaflet function using data specific to this map
  leaflet(totalexif6) %>%
  # use the jawg.terrain basemap
  addTiles(
    'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}{r}.png?access-token=cN5iUzB2bL1QMdeVwMJlflwXQweOoNTDOadGfoCbPf1rRM9FGPb4i1m4f2k5MaJP') %>%
  # set the center of the map to be the center ish of the flight footprint
  setView(
    lng=(totalexif6$lon[1570]),
    lat=(totalexif6$lat[45]),
    zoom = 16.5
  ) %>%
  # add circles at each image location
  addCircleMarkers(
    ~lon,
    ~lat,
    popup = paste(
      "Image filepath:", totalexif6$received_image_path, "<br>",
      "Collection date and time:", totalexif6$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif6$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif6$received_image_path, "<br>",
      "Collection date and time:", totalexif6$DateTimeOriginal, "<br>",
      "Drone altitude above sea level:", totalexif6$altitude_asl, " meters",        "<br>"),
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
      "Image filepath:", totalexif6$received_image_path, "<br>",
      "Drone altitude above sea level:", totalexif6$altitude_asl, " meters",        "<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumASL6(totalexif6$altitude_asl),
    group = "Altitude above sea level") %>%
  # add a legend for the ASL data
  addLegend(
    pal = paletteNumASL6,
    values = totalexif6$altitude_asl,
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
      "Image filepath:", totalexif6$received_image_path, "<br>",
      "Collection date and time:", totalexif6$DateTimeOriginal, "<br>",
      "Collection time after image acquisition initiation",                                 totalexif6$duration,"<br>"),
    radius = 2,
    weight = 1,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = ~paletteNumDateTime6(totalexif6$duration),
    group = "Collection date and time") %>%
  # add a legend for the collection time data
  addLegend(
    pal = paletteNumDateTime6,
    values = totalexif6$duration,
    title = '<small>Collection time after <br> image acquisition <br> initiation (seconds)</small>',
    opacity = 1,
    position = "bottomleft",
    group = "Collection date and time"
  ) %>%
  # add layer control to toggle the overlay groups on and off
  addLayersControl(
    overlayGroups=c("Altitude above sea level", "Collection date and time"), options=layersControlOptions(collapsed=FALSE)) %>%
  # add a legend for the base map
  addLegend(
    colors = "#008080",
    labels = "image locations",
    title = paste ("exif", totalexif6$dataset_id.x[1]),
    opacity = 1,
    position = "topleft"
  )

exif6_MAP

# save as html

saveWidget(exif6_MAP, file="C:\\Users\\emily\\Box\\imagery-metadata-dev\\extracted-metadata\\visualizations\\image-map_20230706-0153.html")
