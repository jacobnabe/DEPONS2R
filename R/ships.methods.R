



#' @title DeponsShips-class
#' @description Objects containing ship routes and ships
#' @description Methods for manipulating, plotting and analyzing ship routes
#' and ship agents used in DEPONS simulations.
#' @slot title Name of the object (character)
#' @slot landscape Name of the landscape that the ships occur in (character)
#' @slot crs CRS object providing the coordinate reference system used; see
#' \code{\link[sp]{CRS}} for details
#' @slot routes \code{data.frame} geographic positions of the virtual 'buoys'
#' that define one or more ship routes that ship agents can follow
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


#' @name summary
#' @title Summary
#' @rdname summary
#' @aliases summary,DeponsShips-method
#' @exportMethod summary
setMethod("summary", "DeponsShips",
          function(object) {
            cat("class:    \t", "DeponsShips \n")
            cat("title:    \t", object@title, "\n")
            cat("landscape:\t", object@landscape, "\n")
            cat("crs:      \t", object@crs, "\n")
            cat("routes:  \t", object@routes[[1]], "\n")
            ships <- object@ships
            ships$route <- as.factor(ships$route)
            summary(ships)
          }
)


#' @title Reading DEPONS ship files
#' @description Function  for reading json-files used for controlling how
#' ship agents behave in DEPONS. Ships move along pre-defined routes in 30-min
#' time steps. The routes are defined by the fix-points provided in the
#' json file, and the geographic projection is assumed to match that of the
#' landscape.
#' @param fname Name of the file (character) that defines the ship routes
#' and ships.
#' @param title Optional character string giving name of simulation
#' @param landscape Optional character string with the landscape used in the
#' simulation
#' @param crs Character, coordinate reference system (map projection)
#' @return Returns an object with the elements \code{title} \code{landscape},
#' \code{crs}, \code{routes} and \code{ships}.
#' @export read.DeponsShips
read.DeponsShips <- function(fname, title="NA", landscape="NA", crs=as.character(NA)) {
  if(!file.exists(fname)) stop(paste("The file", fname, "does not exist"))
  ships.json <- jsonlite::fromJSON(txt=fname, simplifyDataFrame = TRUE)
  all.data <- new("DeponsShips")
  all.data@title <- title
  all.data@landscape <- landscape
  all.data@crs <- crs
  all.data@routes <- ships.json[["routes"]]
  all.data@ships <- ships.json[["ships"]]
  return(all.data)
}


#' @title Plot a DeponsShips object
#' @description Plot the tracks that ship agents move along in DEPONS.
#' @aliases plot.DeponsShips
#' @param x DeponsShips object
#' @param y Not used
#' @param ... Optional plotting parameters
#' @exportMethod plot
setMethod("plot", signature("DeponsShips", "missing"),
          function(x, y, ...)  {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            dots <- list(...)
            col <- "black"
            if("col" %in% names(dots)) col <- dots$col
            lwd <- 1
            if("lwd" %in% names(dots)) lwd <- dots$lwd
            lty <- 1
            if("lty" %in% names(dots)) lty <- dots$lty
            add <- FALSE
            if("add" %in% names(dots)) add <- dots$add
            xlab <- "x"
            if("xlab" %in% names(dots)) xlab <- dots$xlab
            ylab <- "y"
            if("ylab" %in% names(dots)) ylab <- dots$ylab
            axes <- TRUE
            if("axes" %in% names(dots)) axes <- dots$axes
            main <- ifelse(x@title=="NA", "DEPONS track", x@title)
            if (!add) {
            } else {
            }
          }
)
