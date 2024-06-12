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
detect_trees <- function(chm, ws) {

  algorithm = lidR::lmf(ws = ws)

  function(chm, algorithm, uniqueness = "incremental") {
    res <- lidr::locate_trees(las, algorithm, uniqueness)
    if (is(res, "sf")) {
      if (nrow(res) == 0L) {
        coords <- matrix(0, ncol = 2)
        data <- data.frame(treeID = integer(1), Z = numeric(1))
        res <- sp::SpatialPointsDataFrame(coords, data,
                                          proj4string = as(st_crs(las), "CRS"))
        res <- res[0, ]
      }
      else {
        res <- sf::st_zm(res)
        res <- sf::as_Spatial(res)
      }
    }
    return(res)
  }
}
