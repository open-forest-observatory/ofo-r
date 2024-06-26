% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata-extraction_imagery_perimage.R
\name{extract_accuracy}
\alias{extract_accuracy}
\title{Returns accuracy of latitude and longitude. Standard deviation. Units: meters.}
\usage{
extract_accuracy(exif)
}
\arguments{
\item{exif}{the exif metadata file}
}
\value{
accuracy of latitude and longitude of each image. Standard deviation. Units: meters.
}
\description{
DJI EXIF files have an RTK standard longitude deviation (RtkStdLon, the standard deviation (in meters) of the photo recording position in longitude direction) and an RTK standard latitude deviation (RtkStdLat, the standard deviation (in meters) of the photo recording position in latitude direction). This function pulls those three values to include in image-level metadata collation.
}
\examples{
extract_accuracy(exif)

}
