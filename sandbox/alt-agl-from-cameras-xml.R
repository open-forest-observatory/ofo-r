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

d = read_xml("/ofo-share/str-disp_drone-data-partial/imagery-processed/outputs/120m-01/Lassic-120m_20240213T0503_cameras.xml")

# TODO: get chunk transform matrix
chunks = xml_find_all(d, ".//chunk")
# Assume there's only one chunk
chunk = chunks[1]
tf = xml_find_all(chunk, "transform")
rot = xml_text(rot) = xml_find_all(tf, "rotation") |> xml_text()
trans = xml_find_all(tf, "translation") |> xml_text()
scale = xml_find_all(tf, "scale") |> xml_text()


chunk_tmat = make_4x4_transform(rot, trans, scale)

# Get each camera's position vector (rightmost 4x1 column of the camera 4x4 transform matrix)
cams = xml_find_all(d, ".//camera")
l = as_list(cams)

cam = l[[850]]

label = attributes(cam)$label

trans = cam$transform[[1]]
trans = strsplit(trans, " ")[[1]] |> as.numeric()

tmat = matrix(trans, nrow = 4, byrow = TRUE)

pos = tmat[, 4]
pos = matrix(pos, nrow = 4)

cam_pos_ecef = chunk_tmat %*% pos

# Convert this vector to a sf object with CRS EPSG:4978
ecef = st_point(cam_pos_ecef[1:3], dim = "XYZ") |> st_sfc(crs = 4978)
geo = st_transform(ecef, 4326)

# TODO: read in DEM, compare alt