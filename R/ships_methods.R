

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








#' @title Convert ship tracks to DeponsShips object
#' @name ais.to.DeponsShips
#' @description Convert Automatic Identification System (AIS) data for ships to
#' ship track objects. This is done by cropping one or more ship tracks to the
#' extent of a landscape and converting the data to a \code{DeponsShips-class}
#' object. If the AIS data does not include ship positions recorded in half-hour
#' steps, the tracks are interpolated to make objects suitable for use in DEPONS.
#' @param data data.frame with ship positions and the times at which the
#' positions were recorded. Must contain the columns 'id', 'time' (of the form
#' "%Y-%m-%d %H:%M:%S", character, see \code{\link{as.POSIXct}}), 'type' (ship
#' type, character), 'length' (ship length, meters), 'x', and 'y' (position,
#' meters/UTM).
#' @param landsc A \code{DeponsRaster} object corresponding to the
#' landscape that the ships move in. It is assumed that the spatial projection
#' of the ship positions corresponds to that of the DeponsRaster object
#' @param title Title of the output object
#' @param ... Optional parameters, including 'startday' and 'endday'
#' ("%Y-%m-%d %H:%M:%S", character) for defining the first and last date to use
#' from 'data'. If startday = endday the output object will contain up to
#' 49 positions from the selected date for each vessel track.
#' @return Returns a \code{DeponsShips} object containing one or more ships
#' assigned to each of the routes in the object. All ships on a particular
#' route move at the same speed along the route. The routes are
#' defined by x and y coordinates based on the same coordinate reference
#' system as the landscape they are located in. The speed that ships use after
#' reaching a particular position (a particular 'virtual buoy') is calculated
#' from the distance to the following position, and the time it takes reaching
#' that position. If speed is included in the input AIS data, this is NOT used.
#' The routes include one position per half-hour time step, corresponding to
#' the default time step used in the DEPONS model. If input data does not
#' include one position per half hour, new positions are generated using linear
#' interpolation. If the input data contains many positions in a particular
#' half-hour interval, only the positions closest to the half-hour interval are
#' used. The routes contain information about the number of half-hour
#' intervals were ships 'pause' at a particular location, e.g. in a
#' port. These are calculated based on the input AIS data.
#' @seealso \code{\link{aisdata}} for an example of data that can be used as
#' input to ais.to.DeponsShips and \code{\link{interpolate.ais.data}} interpolation
#' of tracks. See \code{\link[DEPONS2R]{write.DeponsShips}} for conversion of
#' \code{DeponsShips} objects to json-files to be used in DEPONS. Use
#' \code{\link{routes}}, \code{\link{ships}}, and \code{\link{title}} for
#' inspection/modification of the ship tracks.
#' @examples
#' data(aisdata)
#' plot(aisdata$x, aisdata$y, type="n", asp=1)
#' ids <- sort(unique(aisdata$id))
#' my.colors <- heat.colors(length(ids))
#' for (i in 1:length(ids)) {
#'   id <- ids[i]
#'   points(aisdata$x[aisdata$id==id], aisdata$y[aisdata$id==id],
#'      cex=0.6, col=my.colors[i])
#' }
#' data(bathymetry)
#' plot(bathymetry, add=TRUE)
#' depons.ais <- ais.to.DeponsShips(aisdata, bathymetry)
#' the.routes <- routes(depons.ais)
#' for (i in 1:length(ids)) {
#' points(the.routes[[i]]$x, the.routes[[i]]$y,
#'         cex=0.6, pch=16, col=my.colors[i])
#' }
#' depons.ais <- ais.to.DeponsShips(aisdata, bathymetry,
#'    startday="2015-12-20", endday="2015-12-20")
#' routes(depons.ais)
#' aisdata2 <- aisdata
#' aisdata2$time <- format(as.POSIXct(aisdata$time)+300)
#' depons.ais2 <- ais.to.DeponsShips(aisdata2, bathymetry,
#'                                startday="2015-12-20", endday="2015-12-21")
#' routes(depons.ais2)
#' @export ais.to.DeponsShips
# setMethod("as", signature("data.frame", "DeponsRaster"), function(data, landsc, title="NA") {
ais.to.DeponsShips <- function(data, landsc, title="NA", ...) {
  if(!inherits(data,"data.frame"))
    stop("data must be a data.frame with the variables 'id', 'time', 'speed', 'type', 'length', 'x', and 'y'")
  if(!all(c('id', 'time', 'type', 'length', 'x', 'y') %in% names(data)))
    stop("data must be a data.frame with the variables 'id', 'time', 'type', 'length', 'x', and 'y'")
  if(!inherits(data$speed,"numeric")) stop("'speed' must be numeric")
  if(!(inherits(data$length,"numeric") || inherits(data$length,"integer"))) stop("'length' must be numeric")
  if(!inherits(data$x,"numeric")) stop("'x' must be numeric")
  if(!inherits(data$y,"numeric")) stop("'y' must be numeric")
  if(!inherits(landsc,"DeponsRaster")) stop("'landsc' must be a DeponsRaster object")
  dots <- list(...)
  startday <- ifelse("startday" %in% names(dots), dots$startday, "NA")
  endday <- ifelse("endday" %in% names(dots), dots$endday, "NA")
  if(!(inherits(startday,"character"))) stop(paste("'startday' must be character"))
  if(!(inherits(endday,"character"))) stop("'endday' must be character")
  data <- interpolate.ais.data(data)
  message(paste0("Conversion assumes that 'x' and 'y' coordinates of ships use the '",
                 crs(landsc), "' projection"))
  time <- try(as.POSIXlt(data$time, tz="UTC"))
  if(!("POSIXlt" %in% class(time)))
    stop("'time' must be of the form 'YYYY-MM-DD HH:MM:SS' for conversion to POSIXlt")
  data$time <- time
  bb <- bbox(landsc)

  # Calculate speed now that data has been interpolated
  data.all<-list()
  for (i in unique(data$id)) {
    data_sub<-data[data$id==i,]
    if(nrow(data_sub)<2) {
      next
    }
    dx <- data_sub$x[2:length(data_sub$x)] - data_sub$x[1:(length(data_sub$x)-1)]
    dy <- data_sub$y[2:length(data_sub$y)] - data_sub$y[1:(length(data_sub$y)-1)]
    dt <- difftime(data_sub$time[2:length(data_sub$time)], data_sub$time[1:(length(data_sub$time)-1)],
                   units="secs")
    dist <- sqrt(dx^2 + dy^2)
    speed <- dist/as.numeric(dt)
    speed[is.na(speed)] <- 0 # replace NAs with 0
    # convert to km per hour
    speed <- speed * 60 * 60 / 1000
    # convert to knots
    speed <- speed/1.85200
    # Last speed is 0 (i.e. ship has arrived at last coordinate)
    speed <- c(speed, 0)
    # Add to one track
    data_sub$speed<-speed
    # Save results
    data.all<-rbind(data.all, data_sub)
  }
  data <- data.all

  # For each track in 'data': find places where the track crosses bb and make
  # linear intrapolation to place new waypoint at the edge of the landscape --
  # then remove waypoints outside the edge from the route
  get.n.or.s.cross <- function(a.track, cross.row) {
    moving.out <- a.track$inside[cross.row] && !a.track$inside[cross.row+1]
    # if(moving.out) cross.row <- cross.row+1
    dy <- a.track$y[cross.row+1] - a.track$y[cross.row]
    # Which edge is crossed?
    edge <- ifelse(abs(a.track$y[cross.row]-bb["y", "max"]) <
                     abs(a.track$y[cross.row]-bb["y", "min"]), "max", "min")
    dy.to.cross <- a.track$y[cross.row] - bb["y", edge]
    proportion.to.cross <- abs(dy.to.cross/dy)
    y.at.cross <- a.track$y[cross.row] + proportion.to.cross*dy
    dx <- a.track$x[cross.row+1] - a.track$x[cross.row]
    x.at.cross <- a.track$x[cross.row] + proportion.to.cross*dx
    dt <- as.numeric(difftime(a.track$time[cross.row+1],
                              a.track$time[cross.row], units="sec"))
    t.at.cross <- a.track$time[cross.row] + proportion.to.cross*dt
    # Replace data in cross row with time and pos for crossing
    a.track$x[cross.row + as.numeric(moving.out)] <- x.at.cross # put value in row just outsd landsc
    a.track$y[cross.row + as.numeric(moving.out)] <- y.at.cross
    a.track$time[cross.row + as.numeric(moving.out)] <- t.at.cross
    a.track$inside[cross.row + as.numeric(moving.out)] <- TRUE
    return(a.track)
  }
  get.e.or.w.cross <- function(a.track, cross.row) {
    moving.out <- a.track$inside[cross.row] && !a.track$inside[cross.row+1]
    # if(moving.out) cross.row <- cross.row+1
    dx <- a.track$x[cross.row+1] - a.track$x[cross.row]
    # Which edge is crossed?
    edge <- ifelse(abs(a.track$x[cross.row]-bb["x", "max"]) <
                     abs(a.track$x[cross.row]-bb["x", "min"]), "max", "min")
    dx.to.cross <- a.track$x[cross.row] - bb["x", edge]
    proportion.to.cross <- abs(dx.to.cross/dx)
    x.at.cross <- a.track$x[cross.row] + proportion.to.cross*dx
    dy <- a.track$y[cross.row+1] - a.track$y[cross.row]
    y.at.cross <- a.track$y[cross.row] + proportion.to.cross*dy
    dt <- as.numeric(difftime(a.track$time[cross.row+1],
                              a.track$time[cross.row], units="sec"))
    t.at.cross <- a.track$time[cross.row] + proportion.to.cross*dt
    # Replace data in cross row with time and pos for crossing
    a.track$y[cross.row + as.numeric(moving.out)] <- y.at.cross # put value in row just outsd landsc
    a.track$x[cross.row + as.numeric(moving.out)] <- x.at.cross
    a.track$time[cross.row + as.numeric(moving.out)] <- t.at.cross
    a.track$inside[cross.row + as.numeric(moving.out)] <- TRUE
    return(a.track)
  }
  all.cropped.tracks <- data.frame()
  for(id in unique(data$id)) {
    one.track <- data[data$id==id ,]
    # Are positions inside the landscape?
    one.track$inside <- one.track$x>=bb["x", "min"] & one.track$x<=bb["x", "max"] &
      one.track$y>=bb["y", "min"] & one.track$y<=bb["y", "max"]
    nrw <- nrow(one.track)
    # Find data row just before crossing edge of landsc
    cross.row <- which(one.track$inside[2:nrw] != one.track$inside[1:(nrw-1)])
    if(length(cross.row)==0) { # that is, if the edge isn't crossed
      cropped.track <- one.track[, c("id", "time", "speed", "type", "length", "x", "y")]
      all.cropped.tracks <- rbind(all.cropped.tracks, cropped.track)
      next
    }

    # Find pos and time were edge is crossed. Crossing northern or southern edge?
    for (i in 1:length(cross.row)) {
      crossing.n.or.s <- (one.track$y[cross.row[i]] < bb["y", "max"] && one.track$y[cross.row[i]+1] > bb["y", "max"]) ||
        (one.track$y[cross.row[i]] < bb["y", "min"] && one.track$y[cross.row[i]+1] > bb["y", "min"]) ||
        (one.track$y[cross.row[i]] > bb["y", "max"] && one.track$y[cross.row[i]+1] < bb["y", "max"]) ||
        (one.track$y[cross.row[i]] > bb["y", "min"] && one.track$y[cross.row[i]+1] < bb["y", "min"])

      if(crossing.n.or.s) one.track <- get.n.or.s.cross(one.track, cross.row[i])
      else one.track <- get.e.or.w.cross(one.track, cross.row[i])
    }
    cropped.track <- one.track[one.track$inside, c("id", "time", "speed", "type", "length", "x", "y")]
    all.cropped.tracks <- rbind(all.cropped.tracks, cropped.track)
    rm(one.track, cropped.track)
  }

  # Convert all.cropped.tracks to a DeponsShips object
  all.cropped.DS <- new("DeponsShips")
  # slotNames(all.cropped.DS)
  all.cropped.DS@crs <- as.character(crs(landsc))
  all.cropped.DS@landscape <- landscape(landsc)
  all.cropped.DS@title <- title
  # Generate one route per ship
  ids <- sort(unique(all.cropped.tracks$id))
  all.routes <- list()

  if (!(startday %in% c("NA"))) {
    startday <- substr(startday, 1, 10) # keep only date
    startday <- as.POSIXct(startday, tz="UTC")
  }
  if (!(endday %in% c("NA"))) {
    endday <- substr(endday, 1, 10)
    endday <- as.POSIXct(endday, tz="UTC")
    endday <- endday + 60 * 60 * 24 - 1
  }
  if (endday < startday) stop("endday should not be before startday")

  # This function rounds the times of coordinates which are no longer on a tick after the ship track cropping step. This happens when a ship leaves or enters the landscape.
  # The function assigns the row the preceding or subsequent tick depending on what times already exists in the dataset. If both the previous and subsequent tick already exist then this row is just removed.
  # This function enables us to add pauses to the function while the ship is outside of the landscape
  roundTimes<-function(one.track) {

    # Check all time stamps are full minutes if not they will need to be rounded up or down
    # to nearest tick.
    # This happens when porps leave or enter via the edges of the landscape
    one.track$secs<-as.numeric(substr(format(one.track$time), 18, 19))
    one.track$hour<-substr(format(one.track$time), 12, 13)
    one.track$mins<-as.numeric(substr(format(one.track$time), 15, 16))
    one.track$mins<-ifelse(one.track$mins==30, 0, one.track$mins)

    # These are times to round
    times.to.round<-one.track[!one.track$mins==0 | !one.track$secs==0,]

    if (nrow(times.to.round)>0) {
      all.times.to.round <- list()
      for (j in 1:nrow(times.to.round)) {
        times.to.round.sub <- times.to.round[j,]
        next.hour <- substr(format(times.to.round.sub$time + 60*60), 12, 13)
        times.to.round.sub$time.below <- ifelse(times.to.round.sub$mins>=0 & times.to.round.sub$mins<30,
                                                as.character(paste0(substr(format(times.to.round.sub$time), 1, 10), " ",
                                                                    times.to.round.sub$hour, ":00:00"), tz="UTC"),
                                                as.character(paste0(substr(format(times.to.round.sub$time), 1, 10), " ",
                                                                    times.to.round.sub$hour, ":30:00"), tz="UTC"))
        times.to.round.sub$time.above <- ifelse(times.to.round.sub$mins>=0 & times.to.round.sub$mins<30,
                                                as.character(paste0(substr(format(times.to.round.sub$time), 1, 10), " ",
                                                                    times.to.round.sub$hour, ":30:00"), tz="UTC"),
                                                as.character(paste0(substr(paste(times.to.round.sub$time), 1, 10), " ", next.hour, ":00:00"),
                                                             tz="UTC"))

        # Determine which time is in the dataset already
        track.compare <- one.track
        track.compare$time <- as.character(track.compare$time)
        time1 <- track.compare[track.compare$time==times.to.round.sub$time.below,]
        time2 <- track.compare[track.compare$time==times.to.round.sub$time.above,]

        # Situation where both times are already in the landscape
        if (nrow(time1)>0 && nrow(time2)>0) {
          # Ship goes out at tick 1 and comes back at tick 2 (i.e. both surrounding times are in the
          # dataset already)
          times.to.round.sub$time <- times.to.round.sub$time.below
          times.to.round.sub <- times.to.round.sub[,1:7]
          times.to.round.sub$remove<-"TRUE"
        } else {
          # Situations where one time isn't in the landscape
          if (nrow(time1)==0) {
            times.to.round.sub$time <- times.to.round.sub$time.below
            times.to.round.sub <- times.to.round.sub[,1:7]
            times.to.round.sub$remove <- "FALSE"
          } else {
            times.to.round.sub$time<-times.to.round.sub$time.above
            times.to.round.sub<-times.to.round.sub[,1:7]
            times.to.round.sub$remove<-"FALSE"
          }
        }

        all.times.to.round <- rbind(all.times.to.round, times.to.round.sub)
      }

      # These are times to round
      all.times.to.round <- all.times.to.round[all.times.to.round$remove==FALSE,]
      all.times.to.round <- all.times.to.round[,1:7]
      one.track.good <- one.track[one.track$mins==0 & one.track$secs==0,]
      one.track.good <- one.track.good[,1:7]
      one.track <- rbind(one.track.good, all.times.to.round)
      one.track <- one.track[order(one.track$time),]
    }

    #one.track$time <- paste(format(one.track$time, format=c("%Y-%m-%d %H:%M:%S"), tz="UTC"))
    #one.track$time <- as.POSIXct(one.track$time, format=c("%Y-%m-%d %H:%M:%S"), tz="UTC")
    #one.track$speed[nrow(one.track)]<-0 # make sure porp stops at last coordinate
    one.track <- one.track[,1:7]

  }

  # Calculate required number of ticks if start and endday are specified
  numberTicks<-function(startday, endday) {

    # Determine required duration of ship track in number of ticks
    all.ticks <- data.frame(seq(startday, endday + 30*60, 30*60))
    colnames(all.ticks)<-c("time")

    # Make sure final number of ticks-1 is a multiple of 48
    no.ticks<-nrow(all.ticks)
    if ((no.ticks-1)%%48!=0) {
      stop("Wrong number of ticks")

    }

    return(all.ticks)

  }

  # Function to add coordinates at start and end of individual ship tracks so that they repeat at regular intervals (occurs if ship track is shorter than simulation duration)
  addMissingTicksStartEnd<-function(all.ticks, one.track, startday, endday, startday.track, endday.track) {

    # If ship route occurs after startday then add missing ticks at start of day
    if (startday.track[1]>startday[1]) {
      first.row<-one.track[1,]
      time<-all.ticks[all.ticks$time < first.row$time,]
      id<-rep(one.track$id[1], length(time))
      speed<-rep(0, length(time))
      type<-rep(first.row$type[1], length(time))
      length<-rep(first.row$length[1], length(time))
      x<-rep(first.row$x[1], length(time))
      y<-rep(first.row$y[1], length(time))
      rows.add<-data.frame(id, time, speed, type, length, x, y)
      one.track<-rbind(rows.add, one.track)
    }

    # If ship route ends before endday then add missing ticks at end of day
    if(endday.track[1]<endday[1]) {
      last.row<-one.track[nrow(one.track),]
      time<-all.ticks[all.ticks$time > last.row$time,]
      id<-rep(one.track$id[1], length(time))
      speed<-rep(0, length(time))
      type<-rep(last.row$type[1], length(time))
      length<-rep(last.row$length[1], length(time))
      x<-rep(last.row$x[1], length(time))
      y<-rep(last.row$y[1], length(time))
      rows.add<-data.frame(id, time, speed, type, length, x, y)
      one.track<-rbind(one.track, rows.add)
    }

    return(one.track)

  }

  # Functon which adds coordinates in the middle of individual ship tracks so that they repeat at regular intervals (occurs if ship temporarily leaves landscape)
  addMissingTicksMiddle<-function(one.track, match) {

    # Create data frame for missing rows (nrow(match)) by duplicating ship characteristics
    # and setting speed to 0 knots
    id<-rep(one.track$id[1], nrow(match))
    speed<-rep(0, nrow(match))
    type<-rep(one.track$type[1], nrow(match))
    length<-rep(one.track$length[1],nrow(match))
    x<-rep(NA, nrow(match))
    y<-rep(NA, nrow(match))
    rows.add<-data.frame(id, match, speed, type, length, x, y)
    one.track<-rbind(one.track, rows.add)
    one.track<-one.track[order(one.track$time),]

    # Fill missing coordinates with next x & y values
    NAs <- one.track[is.na(one.track$x),]
    NAS_info <- list()

    for (k in 1:nrow(NAs)) {
      Na_sub<-NAs[k,]
      # subset times afterwards
      times.after<-one.track[one.track$time>Na_sub$time,]
      times.after<-times.after[!is.na(times.after$x),]
      # Fill in with first x/y
      Na_sub$x<-times.after$x[1]
      Na_sub$y<-times.after$y[1]
      # Save results
      NAS_info<-rbind(NAS_info, Na_sub)
    }

    one.track<-one.track[!is.na(one.track$x),]
    one.track<-rbind(one.track, NAS_info)
    one.track<-one.track[order(one.track$time),]

    return(one.track)

  }


  # Interim function needed in function collapsePauses
  lead_lag <- function(v, n) {
    if (n > 0) c(rep(NA, n), head(v, length(v) - n))
    else c(tail(v, length(v) - abs(n)), rep(NA, abs(n)))
  }

  # Function to collapse dataset when there are pauses in ship movement
  # Adds a column named 'pause' which indicates the number of ticks during which
  # ship movement should be paused at a specific x and y location
  collapsePauses<-function(one.track) {

    # Set 0.1 knots as a threshold for moving
    new_speeds <- ifelse(one.track$speed<=0.1, 0, one.track$speed)

    # Label recurring 0s as 1s & add lox/time information
    recurringZero <- data.frame(new_speeds)
    recurringZero$recurringSpeed <- ifelse(recurringZero$new_speeds==0, 1, 0)
    recurringZero$x <- one.track$x
    recurringZero$y <- one.track$y
    recurringZero$time <- one.track$time

    # Add a pause id so that pause duration can be summed by pause id
    seq_length <- rle(recurringZero$recurringSpeed)$lengths # Calculate duration of pause ids
    NoIds <- length(seq_length)
    recurringZero$pause_no <- rep(1:NoIds, seq_length)
    recurringZero$duration <- 30
    recurringZero$duration <- ifelse(recurringZero$recurringSpeed==0, 0, recurringZero$duration)

    # Sum recurring zeros to calculate duration of pauses  &
    pauses<-aggregate(recurringZero$duration, list(recurringZero$pause_no), FUN=sum)
    recurringZero$pauseTime<-rep(pauses$x/30, seq_length) # divide by 30 to get tick fraction

    # Find out what the next non-zero speed is and re-label the row where the ship pauses
    # This is so the ship knows which speed to move at once it has finished pausing

    recurringZero$new_recurringSpeed<-ifelse(recurringZero$recurringSpeed==1,
                                             lead_lag(recurringZero$new_speeds, -1), recurringZero$new_speeds)
    recurringZero$new_recurringSpeed<-ifelse(is.na(recurringZero$new_recurringSpeed), 0,
                                             recurringZero$new_recurringSpeed)
    recurringZero$new_pauseno<-ifelse(recurringZero$recurringSpeed==0, lead_lag(recurringZero$pause_no,
                                                                                +1), recurringZero$pause_no)

    # Collapse dataset if there are pauses
    if (max(recurringZero$pauseTime, na.rm=TRUE)>0) {
      # Collapse recurring zeros & average x/y lox per pause as AIS coordinates can jitter around
      pauses<-recurringZero[recurringZero$recurringSpeed==1,]
      max_speed <- aggregate(pauses$new_recurringSpeed, list(pauses$new_pauseno), FUN=max)
      new_x <- aggregate(pauses$x, list(pauses$pause_no), FUN=mean)
      new_y <- aggregate(pauses$y, list(pauses$pause_no), FUN=mean)
      new_time <- aggregate(pauses$time, list(pauses$pause_no), FUN=min)
      new_pauseTime <- aggregate(pauses$pauseTime, list(pauses$pause_no), FUN=max)

      #Adjust last pause time (as should be -1)
      new_pauseTime$x[nrow(new_pauseTime)]<-new_pauseTime$x[nrow(new_pauseTime)] -1

      # Create new data frame with these values per pause id
      pauses_collapsed <- data.frame(max_speed$x, rep(1), new_x$x, new_y$x, as.POSIXct(new_time$x),
                                     max_speed$Group.1, new_pauseTime$x, rep(1), max_speed$Group.1)
      colnames(pauses_collapsed) <- c("new_speeds", "recurringSpeed", "x", "y", "time", "pause_no",
                                      "pauseTime", "new_recurringSpeed", "new_pauseno")

      # Join with the rest of the dataset & arrange by time
      movingperiods <- recurringZero[recurringZero$recurringSpeed==0 |
                                       is.na(recurringZero$new_recurringSpeed) ,]
      movingperiods <- recurringZero[!recurringZero$new_pauseno %in% c(pauses_collapsed$new_pauseno) ,]
      movingperiods <- movingperiods[-c(7)]
      #pauses_collapsed<-pauses_collapsed[-c(9)]
      pauses_collapsed$new_recurringSpeed<-pauses_collapsed$new_speeds
      pauses_joined<-rbind(movingperiods, pauses_collapsed)
      pauses_joined<-pauses_joined[order(pauses_joined$time),]

    } else {
      pauses_joined<-recurringZero
    }

    # Create final dataset
    one.route <- data.frame("x"=pauses_joined$x, "y"=pauses_joined$y, "speed"=pauses_joined$new_speeds,
                            "pause"=pauses_joined$pauseTime)

    names(one.route)[4] <- "pause"

    return(one.route)

  }

  for (i in 1:length(ids)) {

    print(i)

    # Subset one track & arrange by time
    id <- ids[i]
    one.track <- all.cropped.tracks[all.cropped.tracks$id==id,]
    one.track<-one.track[order(one.track$time),]

    # Adjust times if ship enters or leaves the landscape between ticks
    one.track<-roundTimes(one.track)

    # Determine start and end time of ship track
    startday.track <- min(one.track$time)
    endday.track <- max(one.track$time)

    # If start & endday are provided them calculate for how many ticks the ship track should last
    # If no start & end are provided then duration is equal to number of 30 min steps between start & end of
    # unique ship track
    if (!(startday %in% c("NA")) && !(endday %in% c("NA"))) {
      all.ticks<-numberTicks(startday, endday)
    } else {
      all.ticks<-data.frame(seq(startday.track, endday.track, 30*60))
      colnames(all.ticks)<-c("time")
    }

    # If there is a start & end date, the next line adds coordinates and pauses at the start and end of ship track
    # if required so that the simulation runs from startday to endday
    if (!(startday %in% c("NA"))) {
      one.track<-addMissingTicksStartEnd(all.ticks, one.track, startday, endday, startday.track, endday.track)
    }

    # Add missing ticks in middle of day (this happens if ship temporarily leaves the landscape)
    # First we determine whether there are missing ticks
    match <- subset(all.ticks, time != one.track$time)
    if(nrow(match)>0) {
      one.track<-addMissingTicksMiddle(one.track, match)
    }

    # Calculate duration of pauses & collapse rows where ship not moving:
    one.route<-collapsePauses(one.track)

    # Final check to make sure dataset contains correct number of ticks
    if (!(startday %in% c("NA"))) {
      # Calculate number of ticks to make sure add up to 48
      one.route2 <- one.route
      one.route2$index <- 1:nrow(one.route2) # index the rows
      one.route2$count.ticks <- 1
      # Add pauses to ticks
      one.route2$count.ticks <- ifelse(one.route2$pause>0, one.route2$count.ticks+one.route2$pause,
                                       one.route2$count.ticks)
      ticks <- sum(one.route2$count.ticks)
      if(!((ticks-1) %% 48)==0) stop("Tick number is wrong")
    }

    # Save route characteristics
    all.routes[[i]] <- one.route

  }

  # Save all routes
  names(all.routes) <- paste("Route", ids, sep="_")
  routes(all.cropped.DS) <- all.routes

  # Save all ship characteristics
  all.ships <- unique(all.cropped.tracks[, c("id", "type", "length")])
  names(all.ships)[1] <- "name"
  all.ships$route <- paste0("Route_", unique(all.cropped.tracks$id))
  ships(all.cropped.DS) <- all.ships
  validObject(all.cropped.DS)

  return(all.cropped.DS)


}
