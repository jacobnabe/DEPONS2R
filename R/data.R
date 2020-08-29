# Author: Jacob Nabe-Nielsen
# Date: 1 August 2020
# Version 0.9
# Licence GPL v3
# Documentation of data sets in DEPONS2R

# devtools::document()  # Make rd files based on roxygen comments.

#' @name  bathymetry
#' @docType data
#' @title Bathymetry of the Kattegat area
#' @description The standard bathymetry file for Kattegat which is used in DEPONS
#' simulations. It is based on a raster file with 1000 rows and 600 columns
#' where each grid cell corresponds to 400 m x 400 m. Cells on land are
#' assigned a missing data value of -9999.
#'
#' DEPONS simulations based on the Kattegat area are based on the UTM zone 32
#' projection, (EPSG:32632) as in the study by Nabe-Nielsen et al (2014). The
#' corresponding proj4string is "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
#' (see \url{https://epsg.io/32632}).
#' @format DeponsRaster
#' @seealso \code{\link{DeponsRaster-class}}
#' @keywords datasets
#' @references Nabe-Nielsen, J., Sibly, R. M., Tougaard, J., Teilmann, J., &
#' Sveegaard, S. (2014). Effects of noise and by-catch on a Danish harbour
#' porpoise population. Ecological Modelling, 272, 242–251.
#' \url{https://doi.org/10.1016/j.ecolmodel.2013.09.025}
data("bathymetry")       # uncomment line to trigger roxygen


#' @name coastline
#' @title Coastline of Northern Europe
#' @docType data
#' @description An object of class \code{\link[sp]{SpatialPolygonsDataFrame}} showing the
#' coastline of the North Sea, Kattegat, and the Western Baltic. The map projection
#' used is ETRS89 – EPSG:3035 projection as for the North Sea raster files used
#' by DEPONS. The corresponding proj4string is "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000
#' +y_0=3210000 +datum=WGS84 +units=m +no_defs".
#' @format SpatialPolygonsDataFrame
#' @keywords datasets
data("coastline")


#' @name porpoisetrack
#' @title Simulated porpoise track
#' @docType data
#' @description An object with three elements: \code{title}, \code{date}, and
#' \code{tracks}, where \code{tracks} is a list of objects of class
#' \code{\link[sp]{SpatialPointsDataFrame}}, each of which corresponds to one
#' simulated animal. \code{date} is the simulation date
#' @format DeponsTrack
#' @keywords datasets
data("porpoisetrack")
