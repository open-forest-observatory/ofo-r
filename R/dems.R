#' Calculate Canopy Height Model (CHM) from a coregistered DSM and DTM
#'
#' This function calculates the Canopy Height Model (CHM) by subtracting the Digital Terrain Model
#' (DTM) from the Digital Surface Model (DSM).
#'
#' @param dsm A terra SpatRaster representing the Digital Surface Model.
#' @param dtm A terra SpatRaster representing the Digital Terrain Model.
#' @param res The desired resolution of the CHM. Default is 0.12 m.
#' @param bounds An sf object representing the bounds to crop the CHM to.
#' @return A terra SpatRaster representing the Canopy Height Model.
#' @examples
#' chm_from_coregistered_dsm_dtm(dsm_raster, dtm_raster)
#' @export
chm_from_coregistered_dsm_dtm = function(dsm, dtm, res = 0.12, bounds = NULL) {

  # Ensure units are in m
  if (!identical(terra::linearUnits(dsm), 1)) {
    stop("DSM must be in a projected (meters) coordinate system. Current coordinate system: ", terra::crs(dsm))
  }

  if (!identical(terra::linearUnits(dtm), 1)) {
    stop("DTM must be in a projected (meters) coordinate system. Current coordinate system: ", terra::crs(dtm))
  }

  ## Resample to specified res
  dsm_resamp = terra::project(dsm, terra::crs(dsm), res = res, method = "bilinear")

  ## Interpolate the the DTM to the res, extent, etc of the DSM
  dtm_resamp = terra::resample(dtm, dsm_resamp, method = "bilinear")

  chm = dsm_resamp - dtm_resamp
  
  # If specified, crop and mask to the bounds
  if (!is.null(bounds)) {
    bounds = sf::st_transform(bounds, terra::crs(dsm))
    chm = terra::crop(chm, bounds)
    chm = terra::mask(chm, bounds)
  }

  # can add smoothing step if need smoothing chm before removing zeros

  chm[chm < 0] = 0
  return(chm)
}
