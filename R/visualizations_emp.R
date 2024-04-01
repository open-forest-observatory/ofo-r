# This script defines image-level metadata extraction functions
# Written by Emily Marie Purvis, last updated April 1 2024

#### load necessary packages for running the function ####
library(devtools)
library(leaflet)
library(leaflet.providers)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(stringr)
library(htmlwidgets)

#### the visualization function ####

# important note #1: the map base this function uses is from Jawg.terrain. In order to use Jawg Maps, you must register (https://www.jawg.io/lab). Once registered, your access token will be located here (https://www.jawg.io/lab/access-tokens) and you will access to all Jawg default maps (variants) and your own customized maps. See the leaflet providers github for more info (https://github.com/leaflet-extras/leaflet-providers/blob/master/README.md)

# important note #2:

#' Returns an interactive map that shows the drone flightpath with locations of images. Altitude above sea level and collection time color schemes can be toggled on and off.
#'
#' @param exif_file the exif filepath (before being prepared to pass to the functions in the wrapper)
#'
#' @return a data.frame of dataset_id, datatime_local, lat, lon, rtk_fix, accuracy_x, accuracy_y, camera_pitch, camera_roll, camera_yaw, exposure, aperture, iso, white_balance, received_image_path, and altitude_asl
#'
#' @examples
#' flightpath_visualization_emp(exif_file)
#'
#' @export

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
