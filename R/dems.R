#' Calculate Canopy Height Model (CHM) from a coregistered DSM and DTM
#'
#' This function calculates the Canopy Height Model (CHM) by subtracting the Digital Terrain Model
#' (DTM) from the Digital Surface Model (DSM).
#'
#' @param dsm A terra SpatRaster representing the Digital Surface Model.
#' @param dtm A terra SpatRaster representing the Digital Terrain Model.
#' @return A terra SpatRaster representing the Canopy Height Model.
#' @export
chm_from_coregistered_dsm_dtm = function(dsm, dtm) {
  chm = dsm - dtm
  chm[chm < 0] = 0
  return(chm)
}