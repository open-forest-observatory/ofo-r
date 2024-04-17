#' Delineate individual trees from canopy height model raster
#'
#' This function uses pixel based height parameters and their distribution
#' around a given neighborhood to find individual tree locations and later the tree crown area and tree heights.
#'
#' @param chm A terra SpatRaster representing the canopy height model.
#' @param ws moving window size. This indicates as number of pixels to consider.
#' @return A spatial point DataFrame representing the tree locations, height, and tree ID.
#' @examples
#'
#' @export



detect_trees <- function(chm_true,ws) {

  algorithm = lmf(ws = ws)

  lidR::find_trees(chm, algorithm)



}



