% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/individual-tree-detection.R
\name{detect_trees}
\alias{detect_trees}
\title{Delineate individual trees from canopy height model raster}
\usage{
detect_trees(chm, ws)
}
\arguments{
\item{chm}{A terra SpatRaster representing the canopy height model.}

\item{ws}{moving window size. This indicates as number of pixels to consider.}
}
\value{
A spatial point DataFrame representing the tree locations, height, and tree ID.
}
\description{
This function uses pixel based height parameters and their distribution
around a given neighborhood to find individual tree locations and later the tree crown area and tree heights.
}
