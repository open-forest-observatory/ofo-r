#' Calculate Canopy Height Model (CHM) from a coregistered DSM and DTM
#'
#' This function calculates the Canopy Height Model (CHM) by subtracting the Digital Terrain Model
#' (DTM) from the Digital Surface Model (DSM).
#'
#' @param dsm A terra SpatRaster representing the Digital Surface Model.
#' @param dtm A terra SpatRaster representing the Digital Terrain Model.
#' @return A terra SpatRaster representing the Canopy Height Model.
#' @examples
#' chm_from_coregistered_dsm_dtm(dsm_raster, dtm_raster)
#' @export


chm_from_coregistered_dsm_dtm = function(dsm, dtm) {

  if (!identical(res(dsm), res(dtm))){
    # check if both raster files have the same spatial resolution
    dtm_resampled <- resample(dtm, dsm, method="bilinear")
  } else {

    dtm_resampled <- dtm
  }

  chm = dsm - dtm_resampled

  # can add smoothing step if need smoothing chm before removing zeros
  chm[chm < 0] = 0
  return(chm)
}
