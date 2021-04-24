



#' @title DeponsShips-class
#' @description Objects containing ship routes and ships
#' @description Methods for manipulating, plotting and analyzing ship routes
#' and ship agents used in DEPONS simulations.
#' @slot title Name of the object (character)
#' @slot landscape Name of the landscape that the ships occur in (character)
#' @slot crs CRS object providing the coordinate reference system used; see
#' \code{\link[sp]{CRS}} for details
#' @slot routes \code{data.frame} geographic positions of the 'virtual buoys'
#' that define one or more ship routes that ship agents can follow
#' @slot ships \code{data.frame} defining each of the ships occurring in DEPONS
#' simulations, and the route they occur on
#' @seealso \code{\link[DEPONS2R]{plot.DeponsShips}} and
#' \code{\link[DEPONS2R]{read.DeponsShips}}
#' @examples
#' data(shipdata)
#' ships(shipdata)[1:10,]
#' routes(shipdata)
#' plot(shipdata, col=c("red", "purple", "blue"))
#' @exportClass DeponsShips
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





#' @title Reading DEPONS ship files
#' @description Function  for reading the json-files that are used for controlling
#' how ship agents behave in DEPONS. Ships move along pre-defined routes in 30-min
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
  all.data@routes <- routes <- ships.json[["routes"]]
  all.data@ships <- ships.json[["ships"]]
  return(all.data)
}


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
            the.routes <- object@routes
            cat("routes:  \n")
            for (i in 1:length(the.routes$route)) {
              cat("\t", nrow(the.routes[[2]][[i]]), "\t positions on ")
              cat("  ", the.routes$name[[i]], "     \n")
            }
            cat("ships:  \n")
            ships <- object@ships
            ships$route <- as.factor(ships$route)
            summary(ships)
          }
)


#' @title Writing DEPONS ship files
#' @aliases write,DeponsShips-method
#' @aliases write.DeponsShips
#' @description Function  for writing a json-file for controlling
#' how ship agents behave in DEPONS. Ships move along pre-defined routes in 30-min
#' time steps. The routes are defined by the fix-points provided in the
#' json file, and the geographic projection is assumed to match that of the
#' landscape. The projection is not stored as part of the json file.
#' @param x Name of the DeponsShips object to be exported
#' @param file Name of the file (character) that defines the ship routes
#' @return No return value, called for side effects
#' @export write
setMethod("write", "DeponsShips",
          function(x, file) {
            nc <- nchar(file)
            if(substr(file, nc-4, nc) != ".json") stop ("'file' must have extension '.json'")
            ships.json <- list()
            ships.json[[1]] <- x@routes
            ships.json[[2]] <- x@ships
            names(ships.json) <- c("routes", "ships")
            jout <- jsonlite::toJSON(ships.json)
            write(jout, file=file)
          }
)


#' @title Plot a DeponsShips object
#' @description Plot the tracks that ship agents move along in DEPONS.
#' @aliases plot.DeponsShips
#' @param x DeponsShips object
#' @param y Not used
#' @param ... Optional plotting parameters, including 'col', 'main',
#' 'add.legend', and 'legend.xy' (defaults to 'topright' when add.legend=TRUE)
#' @examples
#' data(shipdata)
#' plot(shipdata, col=c("red", "green", "blue"))
#'
#' \donttest{
#' # convert route coordinate units from 'grid squares' to UTM
#' data(bathymetry)
#' out <- summary(bathymetry)
#' left <- out[[4]][1]
#' bottom <- out[[4]][2]
#' for (i in 1:3) {
#'     newroute <- shipdata@routes[[2]][[i]]*400
#'     newroute$x <- newroute$x + as.numeric(left)
#'     newroute$y <- newroute$y + as.numeric(bottom)
#'     shipdata@routes[[2]][[i]] <- newroute
#'     }
#'
#' # Reproject coastline and clip to size of Kattegat landscape
#' library(sp)
#' data(bathymetry)
#' data(coastline)
#' coastline2 <- spTransform(coastline, crs(bathymetry))
#' bbox <- bbox(bathymetry)
#' clip.poly <- make.clip.poly(bbox, crs(bathymetry))
#' new.coastline <- rgeos::gIntersection(coastline2, clip.poly, byid = TRUE,
#'   drop_lower_td = TRUE)
#' plot(new.coastline, col="lightyellow2")
#' plot(shipdata, col=c("red", "green", "blue"), add=TRUE, add.legend=TRUE)
#' plot(clip.poly, add=TRUE)
#' }
#' @return No return value, called for side effects
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
            add.legend <- FALSE
            if("add.legend" %in% names(dots)) add.legend <- dots$add.legend
            legend.xy <- "topright"
            if("legend.xy" %in% names(dots)) legend.xy <- dots$legend.xy
            main <- ifelse(x@title=="NA", "DEPONS track", x@title)
            if (!add) {
              # Make empty plot of right size
              comb.routes <- data.frame()
              for(r in 1:length(x@routes[[2]])) {
                comb.routes <- rbind(comb.routes, x@routes[[2]][[r]])
              }
              plot(comb.routes, type="n", asp=1, xlab=xlab, ylab=ylab, main=main,
                   axes=axes)
            }
            n.routes <- length(x@routes[[2]])
            if(length(col) != n.routes) col <- rep("black", n.routes)
            if(length(lwd) != n.routes) lwd <- rep(1, n.routes)
            if(length(lty) != n.routes) lty <- rep(1, n.routes)
            for(r in 1:length(x@routes[[2]])) {
              one.route <- data.frame(x@routes[[2]][[r]])
              lines(one.route, col=col[r], lwd=lwd, lty=lty)
            }
            if (add.legend) {
              graphics::legend(legend.xy, fill=col, legend=x@routes[[1]],
                               bg="white", cex=0.8)
            }
          }
)


setGeneric("ships", function(x, value) {
  return(x@ships)
})

#' @name ships
#' @title Get or define ships in DeponsShips objects
#' @rdname ships
#' @aliases ships,DeponsShips-method
#' @aliases ships<-,DeponsShips-method
#' @param x Object of class \code{DeponsShips}
#' @param value data frame with the 'name', 'speed', 'impact', and 'route' of
#' ships to be simulated. Here 'impact' is the sound source level (dB SPL) and
#' 'route' is one of the shipping routes already defined in the DeponsShips object.
#' @examples
#' data(shipdata)
#' ships(shipdata)
#' @seealso \code{\link{routes}}
#' @exportMethod ships
setMethod("ships", signature=("DeponsShips"), function(x) {
  return(x@ships)
})


setGeneric("ships<-", function(x, value) {
  if(class(value)!= 'data.frame') stop("'value' must be a data.frame")
  if(!("route" %in% names(value))) stop("'value' must contain a variable named 'route'")
  if(!all(value$route %in% x@routes[[1]])) stop("At least some of the routes in 'value' are not defined in 'x'")
  if(!("name" %in% names(value))) stop("'value' must contain a variable named 'name'")
  if(!("speed" %in% names(value))) stop("'value' must contain a variable named 'speed'")
  if(!("impact" %in% names(value))) stop("'value' must contain a variable named 'impact'")
  x@ships <- value
  validObject(x)
  x
})



#' @rdname ships
#' @aliases ships<-,DeponsShips-method
#' @exportMethod ships<-
setGeneric("ships<-", function(x, value) {
  if(class(value)!= 'data.frame') stop("'value' must be a data.frame")
  if(!("route" %in% names(value))) stop("'value' must contain a variable named 'route'")
  if(!all(value$route %in% x@routes[[1]])) stop("At least some of the routes in 'value' are not defined in 'x'")
  if(!("name" %in% names(value))) stop("'value' must contain a variable named 'name'")
  if(!("speed" %in% names(value))) stop("'value' must contain a variable named 'speed'")
  if(!("impact" %in% names(value))) stop("'value' must contain a variable named 'impact'")
  x@ships <- value
  validObject(x)
  x
})



setGeneric("routes", function(x) {
  rts <- x@routes[[2]]
  names(rts) <- x@routes[[1]]
  return(rts)
})


#' @name routes
#' @title Get or define routes in DeponsShips objects
#' @aliases routes,DeponsShips-method
#' @aliases routes<-,DeponsShips-method
#' @aliases routes<-
#' @param x Object of class \code{DeponsShips}
#' @param value list with one named element per shipping route. Each element is
#' a data frame with the variables x and y, which define the coordinates of the fix-points
#' on the shipping routes.
#' @seealso \code{\link{ships}}
#' @exportMethod routes
setMethod("routes", signature=("DeponsShips"), function(x) {
  rts <- x@routes[[2]]
  names(rts) <- x@routes[[1]]
  return(rts)
})


setGeneric("routes<-", function(x, value) {
  x@routes <- value
  validObject(x)
  x
})


#' @rdname routes
#' @aliases routes<-,DeponsShips-method
#' @exportMethod routes<-
setMethod("routes<-", signature=("DeponsShips"), function(x, value) {
  if (class(value) != "list") stop("'value' must be a list with one element per ship route")
  n.routes <- length(value)
  out <- data.frame("name"=rep("", length=n.routes), "route"=NA)
  for (i in 1:n.routes) {
    if (!all(names(value[[i]])==c("x", "y"))) {
      stop(paste("The names of element", i, "in 'value' is not 'x' and 'y'"))
    }
    out$route[i] <- value[i]
    out$name[i] <- names(value[i])
  }
  x@routes <- out
  validObject(x)
  x
})
