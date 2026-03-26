# Author: Jacob Nabe-Nielsen
# Licence GPL v3
# Documentation of data sets in DEPONS2R

# devtools::document()  # Make rd files based on roxygen comments.

#' @name geminidata
#' @docType data
#' @title Observed and simulated data from Gemini windfarm construction
#' @description A nested list containing observed and simulated data used for
#' response-to-piling calibration in the Gemini case study.
#'
#' The observed data component contains CPOD detections, CPOD coordinates,
#' gemini piling coordinates and schedule, and observed DPM data aggregated by
#' distance to piling and hour since piling. `cpod.data`, `cpod.coord`, and
#' `piling.data` are used in [calib_03_make_sim_dpm()], and `obs.dpm` is used
#' to calculate SSD with the function [calib_03_ssd()].
#'
#' The simulated data component contains raw simulated porpoise counts per block
#' (`sim.data`; summed across 10000 replicates) resulting from the best combination of
#' c and RT in DEPONS 4.0, and those simulated data aggregated by
#' distance interval and hour since piling (`sim.dpm`; aggregated with the function
#' [calib_03_make_sim_dpm()]). These simulated DPM data can be visualised against
#' observed ones with the function [calib_03_ssd()].
#' @format A nested list with two elements:
#' \describe{
#'   \item{obs.data}{A list containing:
#'     \describe{
#'       \item{piling.data}{Data frame of gemini piling coordinates and schedule.}
#'       \item{cpod.coord}{Data frame of CPOD station coordinates.}
#'       \item{cpod.data}{Observed CPOD detection data.}
#'       \item{dpm.vs.hr}{Observed DPM data aggregated by distance to piling and
#'       hour since piling.}
#'     }
#'   }
#'   \item{sim.data}{A list containing:
#'     \describe{
#'       \item{sim.data}{Raw simulated porpoise counts per block.}
#'       \item{sim.dpm.vs.hr}{Simulated DPM data aggregated by distance interval
#'       and hour since piling.}
#'     }
#'   }
#' }
#' @keywords datasets
# data("geminidata")

#' @name argosmetrics
#' @docType data
#' @title Fine and Large-scale metrics of Argos data
#' @description A nested list containing fine- and large-scale movement metrics
#' derived from satellite-tracked harbour porpoises (Argos data) in the North Sea
#' and Kattegat.
#'
#' Fine-scale movement metrics include home range (HR, km2), mean net squared
#' displacement (NSD, km2), and residence time (RT, days), calculated from
#' 30-day track segments during which animals used only fine-scale movements.
#'
#' Large-scale movement metrics include HR (km2), maximum NSD (km2), a sinuosity
#' index (Benhamou, 2004), and cumulative distance moved (km), calculated from
#' 100-day track segments irrespective of movement type. Large-scale metrics are
#' available for the North Sea only.
#' @format A nested list of data frames.
#' @keywords datasets
#data("argosmetrics")

#' @name  shipdata
#' @docType data
#' @title Hypothetical ships on  routes through Kattegat
#' @description The ship routes and ships used in the study by Nabe-Nielsen et
#' al. (2014). The fix points that define the routes use the UTM zone 32
#' projection, (EPSG:32632; see \url{https://epsg.io/32632}).
#'
#' The definitions of the ships has been modified since earlier versions of
#' DEPONS (i.e. 2.1 and erlier) in that it now includes ship length, type, and
#' speed (in knots). These are used for calculating the sound source level
#' (following MacGillivray & de Jong 2021)
#' @format DeponsShips
#' @seealso \code{\link{DeponsShips-class}}
#' @keywords datasets
#' @references
#' MacGillivray, A., & de Jong, C (2021). A reference spectrum model for
#' estimating source lev￾els of marine shipping based on Automated
#' Identification System data. Journal of Marince Science and Engineering, 9(4), 369 .
#' \doi{10.3390/jmse9040369}
#'
#' Nabe-Nielsen, J., Sibly, R. M., Tougaard, J., Teilmann, J., &
#' Sveegaard, S. (2014). Effects of noise and by-catch on a Danish harbour
#' porpoise population. Ecological Modelling, 272, 242–251.
#' \doi{10.1016/j.ecolmodel.2013.09.025}
# data("shipdata")


#' @name  aisdata
#' @docType data
#' @title Position for three ships in the inner Danish waters
#' @description Automatic identification system (AIS) data for three ships
#' in Kattegat and the Western Baltic from 20 Dec 2015. The data set includes
#' the variables id (the Maritime Mobile Service Identity number), time,
#' speed (in knots), type, length (in meters), x and y (which provide the
#' coordinates of the ship at a given time. The coordinates use the UTM zone 32
#' projection (CRS = "+proj=utm +zone=32 +units=m +no_defs +datum=WGS84"). Data
#' were downloaded from the Danish Maritime Authority web page
#' (\url{https://dma.dk}).
#' @format data.frame
#' @keywords datasets
# data("aisdata")       # uncomment line to trigger roxygen


#' @name  bathymetry
#' @docType data
#' @title Bathymetry of the Kattegat area
#' @description The standard bathymetry file for Kattegat which is used in DEPONS
#' simulations. It is based on a raster file with 1000 rows and 600 columns
#' where each grid cell corresponds to 400 m x 400 m. Cells on land are
#' assigned a missing data value of -9999.
#'
#' The Kattegat landscapes use the UTM zone 32 projection, (EPSG:32632) as in
#' the study by Nabe-Nielsen et al (2014). The corresponding proj4string is
#' "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
#' (see \url{https://epsg.io/32632}).
#' @format DeponsRaster
#' @seealso \code{\link{DeponsRaster-class}}
#' @keywords datasets
#' @references Nabe-Nielsen, J., Sibly, R. M., Tougaard, J., Teilmann, J., &
#' Sveegaard, S. (2014). Effects of noise and by-catch on a Danish harbour
#' porpoise population. Ecological Modelling, 272, 242–251.
#' \doi{10.1016/j.ecolmodel.2013.09.025}
# data("bathymetry")       # uncomment line to trigger roxygen


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
# data("coastline")


#' @name porpoisedyn
#' @title Simulated porpoise population dynamics
#' @docType data
#' @description An object of class \code{DeponsDyn} with output from a DEPONS
#' simulation based on the Kattegat landscape, assuming that the simulation
#' represents the period 2010-01-01 onward in the real world. Number of animals
#' and energy availability is recorded for the entire landscape.
#' @format DeponsDyn
#' @keywords datasets
#' @seealso \code{\link{DeponsDyn-class}} and \code{\link{porpoisebdyn}}.
# data("porpoisedyn")

#' @name porpoisebdyn
#' @title Simulated porpoise population dynamics
#' @docType data
#' @description An object of class \code{DeponsBlockdyn} with output from a DEPONS
#' simulation based on the North Sea landscape, using a landscape divided into
#' two blocks. Numbers of animals are counted per block.
#' @format DeponsBlockdyn
#' @keywords datasets
#' @seealso \code{\link{DeponsBlockdyn-class}}, \code{\link{porpoisedyn}}
# data("porpoisebdyn")


#' @name porpoisetrack
#' @title Simulated porpoise track
#' @docType data
#' @description An object with five elements: \code{title}, \code{landscape},
#' \code{simtime}, \code{crs}, and \code{tracks}. The \code{crs} stores information
#' about the map projection used ("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs").
#' The \code{tracks} element is a list of objects of class
#' \code{\link[sp]{SpatialPointsDataFrame}}, each ofwhich corresponds to one
#' simulated animal. \code{simtime} is the simulation date.
#' @format DeponsTrack
#' @keywords datasets
#' @seealso \code{\link{DeponsTrack-class}}. See
#' \code{\link[DEPONS2R]{plot.DeponsTrack}} for plotting of simulated tracks.
#data("porpoisetrack")



