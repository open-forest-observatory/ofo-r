# Functions to extract mission flight altitude from photogrammetry products

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

# Get chunk transform matrix
get_chunk_trans_mat = function(project_xml) {

  chunks = xml2::xml_find_all(project_xml, "chunk")
  chunk = chunks[1] # assume there's only one chunk
  tf = xml2::xml_find_all(chunk, "transform")
  tf = tf[1] # assume there's only one transform
  rot = xml2::xml_find_all(tf, "rotation") |> xml2::xml_text()
  trans = xml2::xml_find_all(tf, "translation") |> xml2::xml_text()
  scale = xml2::xml_find_all(tf, "scale") |> xml2::xml_text()

  chunk_tmat = make_4x4_transform(rot, trans, scale)

  return(chunk_tmat)
}

# Get camera positions in geographic coordinates
get_cams_pos_geo = function(project_xml) {

  # Get the chunk transform matrix
  chunk_tmat = get_chunk_trans_mat(project_xml)

  # Get each camera's position vector (rightmost 4x1 column of the camera 4x4 transform matrix) and
  # multiply by the chunk transform matrix to get the camera position in ECEF coordinates
  cams = xml2::xml_find_all(project_xml, ".//camera") |> xml2::as_list()

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

  # Make the list of one-row data frames into a single multi-row data frame
  cam_df = dplyr::bind_rows(cam_df_list)

  # Convert this to a sf object with CRS EPSG:4978
  ecef = sf::st_as_sf(cam_df, coords = c("x", "y", "z"), crs = 4978)
  # Project to geographic
  geo = sf::st_transform(ecef, 4326)

  return(geo)

}

# Extract camera positions and compare against DTM to get altitude AGL. Note that the photogrammetry
# run must include output of a cameras XML (named '{project_name}_cameras.xml') and a DTM (named
# '{project_name}_tem-ptcloud.tif')
#' @export
get_mission_agl = function(run_name,
                           photogrammetry_outputs_path) {
  cameras_xml_path = file.path(photogrammetry_outputs_path, paste0(run_name, "_cameras.xml"))
  dtm_path = file.path(photogrammetry_outputs_path, paste0(run_name, "_dtm-ptcloud.tif"))

  cameras_xml = xml2::read_xml(cameras_xml_path)

  cams_pos_geo = get_cams_pos_geo(cameras_xml)

  # Get number of total cameras and number of aligned cameras
  n_cams_tot = xml2::xml_find_all(cameras_xml, ".//camera") |> xml2::as_list() |> length()
  n_cams_aligned = nrow(cams_pos_geo)
  prop_cams_aligned = n_cams_aligned / n_cams_tot

  # Read in DTM, compare alt of cameras to DTM

  dtm = terra::rast(dtm_path)

  cams_pos_dtmproj = sf::st_transform(cams_pos_geo, sf::st_crs(dtm))
  dtm_elev = terra::extract(dtm, cams_pos_geo |> sf::st_transform(sf::st_crs(dtm)))

  cams_pos_geo$dtm_elev = dtm_elev[, 2]
  cams_pos_geo$camera_elev = sf::st_coordinates(cams_pos_geo)[, 3]
  cams_pos_geo$camera_agl = cams_pos_geo$camera_elev - cams_pos_geo$dtm_elev

  hist(cams_pos_geo$camera_agl, main = paste("Camera altitude AGL (m) for", run_name))

  mission_agl = median(cams_pos_geo$camera_agl, na.rm = TRUE)
  
  # Get the variation (coef of variation) in camera altitude AGL, to detect cases of, e.g. two
  # batteries at very different altitudes, fist excluding the upper and lower 10th percentile to
  # exclude outliers like takeoff photos or landscape shots
  agl = cams_pos_geo$camera_agl
  lwr = quantile(agl, probs = 0.1, na.rm = TRUE)
  upr = quantile(agl, probs = 0.9, na.rm = TRUE)
  agl_core = agl[agl > lwr & agl < upr]
  agl_cv = sd(agl_core, na.rm = TRUE) / mean(agl_core, na.rm = TRUE)

  ret = data.frame(
    mission = run_name,
    n_cams_tot = n_cams_tot,
    prop_aligned = prop_cams_aligned,
    agl_median = mission_agl,
    agl_cv = agl_cv

  )

  return(ret)

}
