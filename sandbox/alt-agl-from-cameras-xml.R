# NOTE: This is a work in progress for getting lat/lon/alt from a metashape camera position XML and
# using it to get the height of the estimated cameras above ground.

make_4x4_transform <- function(rotation_str, translation_str, scale_str = "1") {
  # Convert strings to numeric vectors
  rotation_np = as.numeric(strsplit(rotation_str, " ")[[1]])
  translation_np = as.numeric(strsplit(translation_str, " ")[[1]])
  scale = as.numeric(scale_str)
  
  # Reshape rotation vector into a 3x3 matrix
  rotation_np = matrix(rotation_np, nrow = 3, byrow = TRUE)
  
  # Check if the determinant of the rotation matrix is close to 1
  if (abs(det(rotation_np) - 1) > 1e-8) {
    stop(paste("Improper rotation matrix with determinant", det(rotation_np)))
  }
  
  # Create a 4x4 identity matrix
  transform = diag(4)
  
  # Update the rotation and translation components of the transform matrix
  transform[1:3, 1:3] = rotation_np * scale
  transform[1:3, 4] = translation_np
  
  return(transform)
}



library(tidyverse)
library(elevatr)
library(xml2)
library(sf)
library(terra)

d = read_xml("/ofo-share/str-disp_drone-data-partial/imagery-processed/outputs/120m-01/Lassic-120m_20240213T0503_cameras.xml")

# Get chunk transform matrix

get_chunk_trans_mat = function(project_xml) {

  chunks = xml_find_all(project_xml, "chunk")
  chunk = chunks[1] # assume there's only one chunk
  tf = xml_find_all(chunk, "transform")
  tf = tf[1] # assume there's only one transform
  rot = xml_find_all(tf, "rotation") |> xml_text()
  trans = xml_find_all(tf, "translation") |> xml_text()
  scale = xml_find_all(tf, "scale") |> xml_text()

  chunk_tmat = make_4x4_transform(rot, trans, scale)

  return(chunk_tmat)
}


get_cams_pos_geo = function(project_xml) {

  # Get the chunk transform matrix
  chunk_tmat = get_chunk_trans_mat(project_xml)

  # Get each camera's position vector (rightmost 4x1 column of the camera 4x4 transform matrix) and
  # multiply by the chunk transform matrix to get the camera position in ECEF coordinates
  cams = xml_find_all(project_xml, ".//camera") |> as_list()

  cam_df_list = list()

  for (i in 1:length(cams)) {

    cam = cams[[i]]

    label = attributes(cam)$label

    trans = cam$transform[[1]]

    if (is.null(trans)) next() # cam pos not estimated

    trans = strsplit(trans, " ")[[1]] |> as.numeric()

    tmat = matrix(trans, nrow = 4, byrow = TRUE)

    pos = tmat[, 4]
    pos = matrix(pos, nrow = 4)

    cam_pos_ecef = chunk_tmat %*% pos

    cam_df_row = data.frame(
      label = label,
      x = cam_pos_ecef[1],
      y = cam_pos_ecef[2],
      z = cam_pos_ecef[3]
    )

    cam_df_list[[i]] = cam_df_row

  }


  # Make the list of one-row data frames into a single data frame
  cam_df = bind_rows(cam_df_list)

  # Convert this to a sf object with CRS EPSG:4978
  ecef = st_as_sf(cam_df, coords = c("x", "y", "z"), crs = 4978)
  geo = st_transform(ecef, 4326)

  return(geo)

}

cams_pos_geo = get_cams_pos_geo(d)


# Get number of total cameras and number of aligned cameras
n_cams_tot = xml_find_all(d, ".//camera") |> as_list() |> length()
n_cams_aligned = nrow(cams_pos_geo)
prop_cams_aligned = n_cams_aligned / n_cams_tot

# Read in DTM, compare alt of cameras to DTM

dtm = rast("/ofo-share/str-disp_drone-data-partial/imagery-processed/outputs/120m-01/Lassic-120m_20240213T0503_dtm-ptcloud.tif")

dtm_elev = terra::extract(dtm, cams_pos_geo |> st_transform(st_crs(dtm)))

cams_pos_geo$dtm_elev = dtm_elev[, 2]
cams_pos_geo$camera_elev = st_coordinates(cams_pos_geo)[, 3]
cams_pos_geo$camera_agl = cams_pos_geo$camera_elev - cams_pos_geo$dtm_elev

hist(cams_pos_geo$camera_agl)

mission_agl = median(cams_pos_geo$camera_agl, na.rm = TRUE)
