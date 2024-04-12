# This script defines image-level metadata extraction functions
# Written by Emily Marie Purvis, last updated April 1 2024

#### the visualization function ####

#' Returns an interactive map that shows the drone flightpath with locations of images. Altitude above sea level and collection time color schemes can be toggled on and off.
#'
#' @param exif_file the exif filepath (before being prepared to pass to the functions in the wrapper)
#'
#' @return an interactive map that shows the drone flightpath with locations of images. Altitude above sea level and collection time color schemes can be toggled on and off.
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

    addTiles() %>%

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

  return(exif_MAP)
}
