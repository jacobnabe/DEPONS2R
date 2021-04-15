



#' @title DeponsShips-class
#' @description Objects containing ship routes and ships
#' @description Methods for manipulating, plotting and analyzing ship routes
#' and ship agents used in DEPONS simulations.
#' @slot title Name of the object (character)
#' @slot landscape Name of the landscape that the ships occur in (character)
#' @slot crs CRS object providing the coordinate reference system used; see
#' \code{\link[sp]{CRS}} for details
#' @slot routes \code{data.frame} defining one or more ship routs
#' @slot ships \code{data.frame} defining each of the ships occurring in DEPONS
#' simulations, and the route they occur on
#' @exportClass DeponsShips
##### #' @seealso \code{\link[DEPONS2R]{plot.DeponsShips}} and
##### #' \code{\link[DEPONS2R]{read.DeponsShips}}
setClass(Class="DeponsShips",
         slots=list(title="character", landscape="character", crs="character",
                    routes="data.frame", ships="data.frame")
)

setMethod("initialize", "DeponsShips",
          function(.Object) {
            .Object@title <- "NA"
            .Object@landscape <- "NA"
            .Object@crs <- "NA"
            .Object@routes <-  data.frame("one.route")
            .Object@ships <- data.frame("one.ship")
            return((.Object))
          }
)
