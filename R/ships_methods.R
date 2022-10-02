

#' @title DeponsShips-class
#' @description Objects containing ship routes and ships
#' @description Methods for manipulating, plotting and analyzing ship routes
#' and ship agents used in DEPONS simulations.
#' @slot title Name of the object (character)
#' @slot landscape Name of the landscape that the ships occur in (character)
#' @slot crs CRS object providing the coordinate reference system used; see
#' \code{\link[sp]{CRS}} for details
#' @slot routes \code{data.frame} geographic positions of the 'virtual buoys'
#' that define one or more ship routes that ship agents follow, and the speed
#' that the ships should use when following this route. They also provide
#' information about how long ships should use speed zero when reaching a
#' specific buoy ('i.e. 'pause', measured in minutes). Can be extracted
#' using the \code{\link{routes}} function.
#' @slot ships \code{data.frame} defining each of the ships occurring in DEPONS
#' simulations, and the routes they occur on. The data frame includes the variables
#' 'name', 'type', 'length', and 'route'. Info can be extracted using the
#' \code{\link{ships}} function.
#' @seealso \code{\link{plot.DeponsShips}},
# #' \code{\link[DEPONS2R]{ais.to.DeponsShips}}
#' and \code{\link[DEPONS2R]{read.DeponsShips}}
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



#' @title Interpolate AIS data
#' @name interpolate.ais.data
#' @description Interpolates ship movement tracks obtained from Automatic
#' Identification System (AIS) data to obtain exactly one position per 30
#' minutes. The first and last position in the original track are omitted
#' unless minutes = 0 or 30 and seconds = 0.
#' @param aisdata Data frame including the columns 'id' (ship identifier),
#' 'time' (text string readable by \code{\link{as.POSIXct}}), 'x' and 'y'
#' (recorded ship position, unit: meters), and potentially additional columns
#' @return Returns a data frame with the same columns as the input data
#' @seealso \code{\link{read.DeponsShips}}
#' @examples
#' data(aisdata)
#' ais.testdata <- aisdata[c(12,14,16) ,]
#' plot(ais.testdata[c("x", "y")], asp=1, col="green", pch=16, xlim=c(780000, 837000))
#' lines(ais.testdata[c("x", "y")])
#' # Add 600 sec to 'time' to mis-allign with intervcal needed
#' ais.testdata$time <- format(as.POSIXlt(ais.testdata$time)+600)
#' text(ais.testdata[c("x", "y")]-900, ais.testdata$time, adj=0, cex=0.5)
#' interpolated <- interpolate.ais.data(ais.testdata)
#' points(interpolated[,c("x", "y")], col="red")
#' text(interpolated[c("x", "y")]-900, interpolated$time, adj=0, cex=0.5)
#' legend("bottomright", bty="n", pch=c(16, 1), col=c("green", "red"),
#'     legend=c("original positions", "interpolated"))
#' @export interpolate.ais.data
interpolate.ais.data <- function (aisdata) {
  if (!("x" %in% names(aisdata)))
    stop("aisdata must contain the column 'x'")
  if (!("y" %in% names(aisdata)))
    stop("aisdata must contain the column 'y'")
  if (!("time" %in% names(aisdata)))
    stop("aisdata must contain the column 'time'")
  if (!("id" %in% names(aisdata)))
    stop("aisdata must contain the column 'id'")
  if (!inherits(aisdata$time, "character"))
    stop("'time' must be character")
  ids <- sort(unique(aisdata$id))
  out.data <- data.frame()

  # the.id <- ids[1]
  for (the.id in ids) {
    aisdata.one.id <- aisdata[aisdata$id == the.id, ]
    t.first <- aisdata.one.id[1, "time"]
    mins <- as.numeric(substr(t.first, nchar(t.first) - 4,
                              nchar(t.first) - 3))
    secs <- as.numeric(substr(t.first, nchar(t.first) - 1,
                              nchar(t.first)))
    if (mins == 0 && secs == 0) {
      new.t.first <- t.first
    } else if (mins < 30) {
      secs.to.add <- 60 * (30 - mins)
      new.t.first <- format(as.POSIXct(t.first) +
                                    secs.to.add)
    } else {
      secs.to.add <- 60 * (60 - mins)
      new.t.first <- format(as.POSIXct(t.first) +
                                    secs.to.add)
    }
    t.last <- aisdata.one.id[nrow(aisdata.one.id), "time"]


    mins <- as.numeric(substr(t.last, nchar(t.last) - 4,
                              nchar(t.last) - 3))
    new.mins <- ifelse(mins < 30, "00", "30")
    substr(t.last, nchar(t.last) - 4, nchar(t.last) - 3) <- new.mins
    new.t.last <- t.last


    all.new.times <- seq(as.POSIXct(new.t.first), as.POSIXct(new.t.last),
                         (60 * 30))

    secs.org.pos <- as.numeric(as.POSIXlt(aisdata.one.id$time))
    secs.new.pos <- as.numeric(all.new.times)
    prev.org.pos <- function(x) max(which(secs.org.pos <=
                                            secs.new.pos[x]))
    next.org.pos <- function(x) {
      if (!any(secs.org.pos > secs.new.pos[x]))
        return(NA)
      the.next.pos <- min(which(secs.org.pos > secs.new.pos[x]))
    }
    interp.start.pos <- sapply(1:length(all.new.times), FUN = "prev.org.pos")
    interp.end.pos <- sapply(1:length(all.new.times), FUN = "next.org.pos")
    secs.from.start.pos <- secs.new.pos - secs.org.pos[interp.start.pos]
    secs.to.end.pos <- secs.org.pos[interp.end.pos] - secs.new.pos
    prop.of.step <- secs.from.start.pos/(secs.from.start.pos +
                                           secs.to.end.pos)
    new.x <- aisdata.one.id$x[interp.start.pos] + prop.of.step *
      (aisdata.one.id$x[interp.end.pos] - aisdata.one.id$x[interp.start.pos])
    new.y <- aisdata.one.id$y[interp.start.pos] + prop.of.step *
      (aisdata.one.id$y[interp.end.pos] - aisdata.one.id$y[interp.start.pos])
    new.time <- as.POSIXct(aisdata.one.id$time[interp.start.pos]) +
      prop.of.step * (as.numeric(as.POSIXct(aisdata.one.id$time[interp.end.pos])) -
                        as.numeric(as.POSIXct(aisdata.one.id$time[interp.start.pos])))
    new.time <- as.character(format(new.time))

    # Handle special case where last pos has minutes=0 or 30
    if(mins==0 || mins==30) {
      new.x[length(new.x)] <- aisdata.one.id$x[length(aisdata.one.id$x)]
      new.y[length(new.y)] <- aisdata.one.id$y[length(aisdata.one.id$y)]
      new.time[length(new.time)] <- aisdata.one.id$time[length(aisdata.one.id$time)]
    }
    # set seconds to zero
    substr(new.time, nchar(new.time) - 1, nchar(new.time)) <- "00"
    other.columns <- names(aisdata.one.id)[(!(names(aisdata.one.id) %in%
                                                c("id", "time", "x", "y")))]
    other.columns <- aisdata.one.id[interp.start.pos, other.columns]
    out.data.one.id <- data.frame(id = the.id, other.columns,
                                  x = new.x, y = new.y, time = new.time)
    out.data.one.id <- out.data.one.id[!is.na(out.data.one.id$x), ]
    out.data <- rbind(out.data, out.data.one.id)
  }
  row.names(out.data) <- NULL
  return(out.data)
}



#' @title Read DEPONS ship files
#' @description Function  for reading the json-files that are used for controlling
#' how ship agents behave in DEPONS. Ships move along pre-defined routes in 30-min
#' time steps. The routes are defined by the fix-points provided in the
#' json file, and the geographic projection is assumed to match that of the
#' landscape.
#' @param fname Name of the file (character) where ship routes and ships
#' are defined.
#' @param title Optional character string with the name of the simulation
#' @param landscape Optional character string with the landscape used in the
#' simulation
#' @param crs Character, coordinate reference system (map projection)
# ##' @seealso \code{\link{ais.to.DeponsShips}}, \code{\link[DEPONS2R]{write.DeponsShips}}
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


setMethod("show", "DeponsShips",
          function(object) {
            cat("Object of class:\t", "DeponsShips \n")
            cat("title:          \t", object@title, "\n")
            cat("landscape:      \t", object@landscape, "\n")
            cat("crs:            \t", object@crs, "\n")
            the.routes <- object@routes
            cat("\nObject contains ", nrow(the.routes), " ship routes ")
            ships <- object@ships
            cat("and ", length(unique(ships$name)), " ships \n")
          }
)


#' @title Write DEPONS ship files
#' @aliases write,DeponsShips-method
#' @aliases write.DeponsShips
#' @description Function  for writing a json-file for controlling
#' how ship agents behave in DEPONS. Ships move along pre-defined routes in 30-min
#' time steps. The routes are defined by the fix-points provided in the
#' json file, and the geographic projection is assumed to match that of the
#' landscape. The projection is not stored as part of the json file.
#' @param x Name of the DeponsShips object to be exported
#' @param file Name of the output file (character)
#' @note The exported json file is intended for use in DEPONS 2.3 or later
#' (released July 2022) where the sound pressure level (SPL) is calculated
#' within DEPONS based on ship type, ship length and speed.
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
              comb.routes <- comb.routes[,c("x", "y")]
              plot(comb.routes, type="n", asp=1, xlab=xlab, ylab=ylab, main=main,
                   axes=axes)
            }
            n.routes <- length(x@routes[[2]])
            if(length(col) != n.routes) col <- rep("black", n.routes)
            if(length(lwd) != n.routes) lwd <- rep(1, n.routes)
            if(length(lty) != n.routes) lty <- rep(1, n.routes)
            for(r in 1:length(x@routes[[2]])) {
              one.route <- data.frame(x@routes[[2]][[r]])
              one.route <- one.route[,c("x", "y")]
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
#' @param value data frame with the 'name', 'type', 'length', and 'route' of
#' ships to be simulated, as well as 'tickStart' and 'tickEnd' defining when
#' the ships are to be included in simulations. 'route' is one of the shipping
#' routes defined in the DeponsShips object.
#' @examples
#' data(shipdata)
#' ships(shipdata)
#' @seealso \code{\link{routes}}
#' @exportMethod ships
setMethod("ships", signature=("DeponsShips"), function(x) {
  return(x@ships)
})


setGeneric("ships<-", function(x, value) {
  if(!inherits(value,'data.frame')) stop("'value' must be a data.frame")
  if(!("route" %in% names(value))) stop("'value' must contain a variable named 'route'")
  if(!all(value$route %in% x@routes[[1]])) stop("At least some of the routes in 'value' are not defined in 'x'")
  if(!("name" %in% names(value))) stop("'value' must contain a variable named 'name'")
  if(!("type" %in% names(value))) stop("'value' must contain a variable named 'type'")
  if(!("length" %in% names(value))) stop("'value' must contain a variable named 'length'")
  x@ships <- value
  validObject(x)
  x
})



#' @rdname ships
#' @aliases ships<-,DeponsShips-method
#' @exportMethod ships<-
setGeneric("ships<-", function(x, value) {
  if(!inherits(value,"data.frame")) stop("'value' must be a data.frame")
  if(!("route" %in% names(value))) stop("'value' must contain a variable named 'route'")
  if(!all(value$route %in% x@routes[[1]])) stop("At least some of the routes in 'value' are not defined in 'x'")
  if(!("name" %in% names(value))) stop("'value' must contain a variable named 'name'")
  if(!("type" %in% names(value))) stop("'value' must contain a variable named 'type'")
  if(!("length" %in% names(value))) stop("'value' must contain a variable named 'length'")
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
#' a data frame with the variables x, y, speed, and 'pause' which define the
#' coordinates of the fix-points on the shipping routes and the speeds that ships
#' have after passing the fix point and until reaching the next fix point. The
#' variable 'pause' instructs ships about how many minutes to wait before continuing
#' to move.
#' @note The unit of 'speed' is knots.
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
  if (as.character(class(value)) != "list") stop("'value' must be a list with one element per ship route")
  n.routes <- length(value)
  out <- data.frame("name"=rep("", length=n.routes), "route"=NA)
  for (i in 1:n.routes) {
    if (!all(names(value[[i]])==c("x", "y", "speed", "pause"))) {
      stop(paste("The names of element", i, "in 'value' is not 'x', 'y', 'speed', and 'pause"))
    }
    out$route[i] <- value[i]
    out$name[i] <- names(value[i])
  }
  x@routes <- out
  validObject(x)
  x
})



