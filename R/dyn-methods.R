
# Author: Jacob Nabe-Nielsen
# Date: 27 August 2020
# Version 0.9
# Licence GPL v3
# Description: Methods and classes for reading and summarizing DEPONS raster
#   objects


#' @title  DeponsDyn-class and methods
#' @description Classes and methods for analyzing and plotting output from the
#' DEPONS model.
#' @slot title Character  Name of the object or simulation (character)
#' @slot landscape Character  Identifier for the landscape used in the DEPONS
#' simulations. The landscapes 'DanTysk', 'Gemini', 'Kattegat', 'North Sea',
#' 'Homogeneous', and 'User defined' are distributed with the DEPONS model.
#' @slot simdate POSIXlt object with the date and time when the simulation was
#' finished. This is read from the name of the imput file.
#' @slot crs CRS object providing the coordinate reference system used; see
#' \link[sp]{CRS} for details
#' @slot simstart POSIXlt object with the first day of the simulation, i.e.
#' the first day in the period that the simulations are intended to represent in
#' the real world.
#' @slot data Data frame with simulation output.
#' @details The following columns are included in the simulation output data
#' frame: 'tick', which indicates the number of half-hourly time steps since the
#' start of the simulation; 'count', which indicates the population size at a
#' given time; 'energy', showing the average amount of energy stored by simulated
#' animals, and 'lenergy' which shows the total amount of energy in the landscape.
#' @exportClass DeponsDyn
#' @examples a.DeponsDyn <- new("DeponsDyn")
#' a.DeponsDyn
#' @note DeponsDyn-objects are usually read in from csv files produced during
#' DEPONS simulations.
#' @note Use data(bathymetry) to load example data.
setClass(Class="DeponsDyn",
         slots=list(title="character", landscape="character", simdate="POSIXlt",
                    crs="character", simstart="POSIXlt", data="data.frame")
)


setMethod("initialize", "DeponsDyn",
          function(.Object) {
            .Object@title <- "NA"
            .Object@landscape <- "NA"
            .Object@simdate <- as.POSIXlt(NA)
            .Object@crs <- "NA"
            .Object@simstart <- as.POSIXlt(NA)
            .Object@data <- data.frame("tick"=NA, "count"=NA, "energy"=NA, "lenergy"=NA)
            return((.Object))
          }
)


setMethod("show", "DeponsDyn",
          function(object) {
            cat("class:\t\t", "DeponsDyn \n")
            cat("title:\t\t", object@title, "\n")
            cat("simdate:\t\t", as.character(object@simdate), "\n")
            cat("crs:\t\t", object@crs, "\n")
            cat("simstart:\t\t", as.character(object@simstart), "\n")
            cat("data:\t\t tick \t count \t energy \t lenergy \n" )
            # cat("   start:\t",  object@tick[1] \t object@count[1] \t object@energy[1] \t object@lenergy[1] \n")
          }
)




# fname <- "Statistics.2020.Aug.27.10_55_36.csv"
# popd <- read.csv(fname, sep=";")
# head(popd)

