

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
#' @return Returns a data frame with the same columns as the input data. Tracks
#' that are too short to interpolate are omitted (with a warning)
#' @seealso \code{\link{read.DeponsShips}} and \code{\link{ais.to.DeponsShips}}
#' @examples
#' data(aisdata)
#' ais.testdata <- aisdata[c(12,14,16) ,]
#' plot(ais.testdata[c("x", "y")], asp=1, col="green", pch=16, xlim=c(780000, 837000))
#' lines(ais.testdata[c("x", "y")])
#' # Add 600 sec to 'time' to mis-allign with intervcal needed
#' ais.testdata$time <- format(as.POSIXlt(ais.testdata$time, tz = "UTC")+600)
#' text(ais.testdata[c("x", "y")]-900, ais.testdata$time, adj=0, cex=0.5)
#' interpolated <- interpolate.ais.data(ais.testdata)
#' points(interpolated[,c("x", "y")], col="red")
#' text(interpolated[c("x", "y")]-900, interpolated$time, adj=0, cex=0.5)
#' legend("bottomright", bty="n", pch=c(16, 1), col=c("green", "red"),
#'     legend=c("original positions", "interpolated"))
#' @export interpolate.ais.data
interpolate.ais.data <- function (aisdata)
{
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
  too.short <- vector()
  for (the.id in ids) {
    aisdata.one.id <- aisdata[aisdata$id == the.id, ]
    t.first <- aisdata.one.id[1, "time"]
    if (nchar(t.first) == 10) {
      t.first <- paste(t.first, "00:00:00", sep = " ")
    }
    mins <- as.numeric(substr(t.first, nchar(t.first) - 4,
                              nchar(t.first) - 3))
    secs <- as.numeric(substr(t.first, nchar(t.first) - 1,
                              nchar(t.first)))
    if ((mins == 0 && secs == 0) || (mins == 30 && secs ==
                                     0)) {
      new.t.first <- t.first
    } else if (mins < 30) {
      secs.to.add <- 60 * (30 - mins)
      new.t.first <- format(as.POSIXct(t.first, tz = "UTC") + secs.to.add)
    } else {
      secs.to.add <- 60 * (60 - mins)
      new.t.first <- format(as.POSIXct(t.first, tz = "UTC") + secs.to.add)
    }
    t.last <- aisdata.one.id[nrow(aisdata.one.id), "time"]
    mins <- as.numeric(substr(t.last, nchar(t.last) - 4,
                              nchar(t.last) - 3))
    new.mins <- ifelse(mins < 30, "00", "30")
    substr(t.last, nchar(t.last) - 4, nchar(t.last) - 3) <- new.mins
    new.t.last <- t.last
    if (as.POSIXct(new.t.first, tz = "UTC") >= as.POSIXct(new.t.last, tz = "UTC")) {
      too.short <- c(too.short,the.id)  # if track too short to interpolate, store ID and skip processing this ID
      warning(paste("ID", the.id, "cannot be interpolated (it is too short)"))
      next
    }
    all.new.times <- seq(as.POSIXct(new.t.first, tz = "UTC"), as.POSIXct(new.t.last, tz = "UTC"),
                         (60 * 30))
    aisdata.one.id$time <- ifelse(nchar(aisdata.one.id$time) ==
                                    10, paste(aisdata.one.id$time, "00:00:00"), aisdata.one.id$time)
    secs.org.pos <- as.numeric(as.POSIXlt(aisdata.one.id$time,
                                          format = c("%Y-%m-%d %H:%M:%OS"), tz = "UTC"))
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
    new.time <- as.POSIXct(aisdata.one.id$time[interp.start.pos], tz = "UTC") +
      prop.of.step * (as.numeric(as.POSIXct(aisdata.one.id$time[interp.end.pos], tz = "UTC")) -
                        as.numeric(as.POSIXct(aisdata.one.id$time[interp.start.pos], tz = "UTC")))
    new.time <- as.character(format(new.time))
    if (mins == 0 || mins == 30) {
      new.x[length(new.x)] <- aisdata.one.id$x[length(aisdata.one.id$x)]
      new.y[length(new.y)] <- aisdata.one.id$y[length(aisdata.one.id$y)]
      new.time[length(new.time)] <- aisdata.one.id$time[length(aisdata.one.id$time)]
    }
    substr(new.time, nchar(new.time) - 1, nchar(new.time)) <- "00"
    other.columns <- names(aisdata.one.id)[(!(names(aisdata.one.id) %in%
                                                c("id", "time", "x", "y")))]
    other.columns <- aisdata.one.id[interp.start.pos, other.columns]
    out.data.one.id <- data.frame(id = the.id, other.columns,
                                  x = new.x, y = new.y, time = new.time)
    out.data.one.id <- out.data.one.id[!is.na(out.data.one.id$x),
    ]
    out.data <- rbind(out.data, out.data.one.id)
  }
  row.names(out.data) <- NULL
  return(out.data)
}



#'Check if ships move at unrealistic speeds or are outside the map boundary
#'
#'@description
#'Checks if calculated speeds in DeponsShips objects are unrealistic, which may result from inaccurate AIS positional records or from ships leaving the map area, then re-entering at a remote position. As ship speed in DEPONS directly influences the amount of noise generated, it is advisable to detect and remove such instances to avoid the creation of extreme noise sources. The function can also repair issues arising from ship positions that are a fraction of a meter outside the map boundary (causing loading errors on simulation start).
#'
#'@details
#'The default replacement speeds (knots) for recognized ship types are as follows (class reference speeds from MacGillivray & de Jong, 2021, Table 1): Fishing, 6.4; Tug, 3.7; Naval, 11.1; Recreational, 10.6; Government/Research, 8; Cruise, 17.1; Passenger, 9.7; Bulker, 13.9; Containership, 18.0; Tanker, 12.4; Dredger, 9.5; Other, 7.4.
#'
#'If a simulation fails during data loading with an error that indicates ship positions outside the simulation area, this may be caused by a mismatch in rounding between the map extent of the map used with \code{\link{ais.to.DeponsShips}}, and of generated ship position exactly on the boundary. If a map representative of the simulation area extent is provided (usually the bathymetry map), the function will also repair these positions by rounding them up/down to the floor/ceiling of the map extent (fractional meter adjustments).
#'@param x DeponsShips object
#'@param threshold The speed (knots) above which calculated values are considered unrealistic/excessive. Defaults to 35 knots.
#'@param fix Logical. If FALSE (default), the function returns a data frame of ship tracks containing speeds that exceed the threshold; if TRUE, the function returns a DeponsShips object where these instances have been replaced.
#'@param replacements Named list, where names are ship types and values are replacement speeds (knots) for speeds above the threshold within those types. Only ship types named in the list are processed. If NA (default), reference speeds from Table 1 in MacGillivray & de Jong (2021) are used.
#'@param landscape DeponsRaster object. Optional; a map representative of the simulation map extent (usually the bathymetry map). If provided and fix = TRUE, ship positions on the boundary will be adjusted to avoid errors from fractional mis-positioning.
#'
#'@returns
#'If fix = FALSE, a data frame with columns "route number", "name", "type", "length", and "speed", containing one entry for each ship where an excessive speed occurred. If fix = TRUE, a DeponsShip object where instances of excessive speed have been replaced, and (if a map has been provided) where ship positions on the boundary have been adjusted.
#'
#'@section Reference:
#'MacGillivray, A., & de Jong, C (2021). A reference spectrum model for estimating source levels of marine shipping based on Automated Identification System data. Journal of Marince Science and Engineering, 9(4), 369. doi:10.3390/jmse9040369
#'
#'@examples
#'\dontrun{
#'x <- shipdata
#'check.DeponsShips(x)
#'
#'x@routes$route[[1]]$speed <- x@routes$route[[1]]$speed * 3
#'check.DeponsShips(x)
#'x <- check.DeponsShips(x, fix = T)}
#'@seealso \code{\link{ais.to.DeponsShips}} for creation of DeponsShips objects (including calculated speeds) from AIS data

check.DeponsShips <- function(x, threshold = 35, fix = F, replacements = NA, landscape = NULL) {
  if (!inherits(x, "DeponsShips"))
    stop("'x' must be a DeponsShips object")
  if (!inherits(replacements, "logical") && !inherits(replacements, "list"))
    stop("'replacements' must be a named list, with names denoting ship types and values denoting replacement speeds")
  if (!is.null(landscape)) {
    if (!inherits(landscape, "DeponsRaster"))
      stop("'landscape' must be a DeponsRaster")
  }
  if (fix == F) {
    excesses <- data.frame(matrix(ncol=6, nrow=0))
    for (i in 1:length(x@routes$name)) {
      excess <- which(x@routes$route[[i]]$speed > threshold)
      if (length(excess) > 0) {
        for (i2 in 1:length(excess)) {
          excesses <- rbind(excesses,
                            c(i,
                              excess[i2],
                              x@ships$name[i],
                              x@ships$type[i],
                              x@ships$length[i],
                              x@routes$route[[i]]$speed[excess[i2]]))
        }
      }
    }
    names(excesses) <- c("route number", "route position", "name", "type", "length", "speed")
    if (nrow(excesses) == 0) {
      message("No excessive speeds found")
      return()}
    return(excesses)
  }

  # mean speeds as per McGillivray & De Jong 2021 (Table 1)
  mean_speeds <- list("Fishing" = 6.4,
                      "Tug" = 3.7,
                      "Naval" = 11.1,
                      "Recreational" = 10.6,
                      "Government/Research" = 8,
                      "Cruise" = 17.1,
                      "Passenger" = 9.7,
                      "Bulker" = 13.9,
                      "Containership" = 18.0,
                      "Tanker" = 12.4,
                      "Other" = 7.4,
                      "Dredger" = 9.5)

  if (is.na(replacements)) reps <- mean_speeds else reps <- replacements

  # change speeds over thresholds to replacement values, for types for which replacements have been provided
  for (i in 1:length(x@ships$name)) {
    if (x@ships$type[i] %in% names(reps)) {
      x@routes$route[[i]]$speed[as.numeric(x@routes$route[[i]]$speed) > threshold] <- reps[x@ships$type[i]]
      x@routes$route[[i]]$speed <- unlist(x@routes$route[[i]]$speed)
    }
  }

  # if calibration landscape provided, round positions on border up/down to floor/ceiling of map extent
  if (!is.null(landscape)) {
    for (i in 1:length(x@ships$name)) {
      x@routes$route[[i]]$x[x@routes$route[[i]]$x < landscape@ext[1]] <- ceiling(x@routes$route[[i]]$x)
      x@routes$route[[i]]$x[x@routes$route[[i]]$x > landscape@ext[3]] <- floor(x@routes$route[[i]]$x)
      x@routes$route[[i]]$y[x@routes$route[[i]]$y < landscape@ext[2]] <- ceiling(x@routes$route[[i]]$y)
      x@routes$route[[i]]$y[x@routes$route[[i]]$y > landscape@ext[3]] <- floor(x@routes$route[[i]]$y)
    }
  }
  return(x)
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
#' @seealso \code{\link{ais.to.DeponsShips}}, \code{\link[DEPONS2R]{write.DeponsShips}}
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
#' coastline_sf <- sf::st_as_sf(coastline)
#' coastline2 <- sf::st_transform(coastline_sf, crs(bathymetry))
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

            # Make empty plot of right size
            comb.routes <- data.frame()
             for(r in 1:length(x@routes[[2]])) {
              comb.routes <- rbind(comb.routes, x@routes[[2]][[r]])
            }
            comb.routes <- comb.routes[,c("x", "y")]
            plot(comb.routes, type="n", asp=1, xlab=xlab, ylab=ylab, main=main,
                 axes=axes)

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
#' input to ais.to.DeponsShips. The function builds on
#' \code{\link{interpolate.ais.data}}, which interpolates tracks to ensure
#' that there is a position every 30 minutes. Use \code{\link{check.DeponsShips}}
#' for testing if speeds are realistic.
#' See \code{\link[DEPONS2R]{write.DeponsShips}} for conversion of
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
ais.to.DeponsShips <- function (data, landsc, title = "NA", ...)
{
  if (!inherits(data, "data.frame"))
    stop("data must be a data.frame with the variables 'id', 'time', 'type', 'length', 'x', and 'y'")
  if (!all(c("id", "time", "type", "length", "x", "y") %in%
           names(data)))
    stop("data must be a data.frame with the variables 'id', 'time', 'type', 'length', 'x', and 'y'")
  if (!(inherits(data$length, "numeric") || inherits(data$length,
                                                     "integer")))
    stop("'length' must be numeric")
  if (!inherits(data$x, "numeric"))
    stop("'x' must be numeric")
  if (!inherits(data$y, "numeric"))
    stop("'y' must be numeric")
  if (!inherits(landsc, "DeponsRaster"))
    stop("'landsc' must be a DeponsRaster object")
  dots <- list(...)
  startday <- ifelse("startday" %in% names(dots), dots$startday,
                     "NA")
  endday <- ifelse("endday" %in% names(dots), dots$endday,
                   "NA")
  if (!(inherits(startday, "character")))
    stop(paste("'startday' must be character"))
  if (!(inherits(endday, "character")))
    stop("'endday' must be character")
  data <- interpolate.ais.data(data)
  message(paste0("Conversion assumes that 'x' and 'y' coordinates of ships use the '",
                 crs(landsc), "' projection"))

  # remove instances on day 1 of month that get set to 0 if track is too short
  zero.days <- which(substr(data$time, start=9, stop=10) == "00")
  if (length(zero.days) != 0) {
    data <- data[-zero.days,]
  }

  time <- try(as.POSIXct(data$time, tz = "UTC"))
  if (!("POSIXct" %in% class(time)))
    stop("'time' must be of the form 'YYYY-MM-DD HH:MM:SS' for conversion to POSIX")

  # remove instances with dates not accommodated in the model year
  testdates <- as.POSIXlt(data$time, tz = "UTC")
  bad.dates <- which(((testdates$mon %in% c(4,6,7,9,11)) & (testdates$mday == 31)) |
                       ((testdates$mon == 1) & (testdates$mday == 29)))
  if (length(bad.dates) > 0) {
    data <- data[-bad.dates,]
    message("Some entries with dates not accommodated in the model year (Feb 29, May/Jul/Aug/Oct/Dec 31) were omitted")
  }

  data$time <- as.POSIXct(data$time, tz = "UTC")
  bb <- bbox(landsc)
  data.all <- list()
  for (i in unique(data$id)) {
    data_sub <- data[data$id == i, ]
    if (nrow(data_sub) < 2) {
      next
    }
    dx <- data_sub$x[2:length(data_sub$x)] - data_sub$x[1:(length(data_sub$x) -
                                                             1)]
    dy <- data_sub$y[2:length(data_sub$y)] - data_sub$y[1:(length(data_sub$y) -
                                                             1)]
    dt <- difftime(data_sub$time[2:length(data_sub$time)],
                   data_sub$time[1:(length(data_sub$time) - 1)], units = "secs", tz = "UTC")
    dist <- sqrt(dx^2 + dy^2)
    speed <- dist/as.numeric(dt)
    speed[is.na(speed)] <- 0
    speed <- speed * 60 * 60/1000
    speed <- speed/1.852
    speed <- c(speed, 0)
    data_sub$speed <- speed
    data.all <- rbind(data.all, data_sub)
  }
  data <- data.all
  get.n.or.s.cross <- function(a.track, cross.row) {
    moving.out <- a.track$inside[cross.row] && !a.track$inside[cross.row +
                                                                 1]
    dy <- a.track$y[cross.row + 1] - a.track$y[cross.row]
    edge <- ifelse(abs(a.track$y[cross.row] - bb["y", "max"]) <
                     abs(a.track$y[cross.row] - bb["y", "min"]), "max",
                   "min")
    dy.to.cross <- a.track$y[cross.row] - bb["y", edge]
    proportion.to.cross <- abs(dy.to.cross/dy)
    y.at.cross <- a.track$y[cross.row] + proportion.to.cross *
      dy
    dx <- a.track$x[cross.row + 1] - a.track$x[cross.row]
    x.at.cross <- a.track$x[cross.row] + proportion.to.cross *
      dx
    dt <- as.numeric(difftime(a.track$time[cross.row + 1],
                              a.track$time[cross.row], units = "sec", tz = "UTC"))
    t.at.cross <- a.track$time[cross.row] + proportion.to.cross *
      dt
    a.track$x[cross.row + as.numeric(moving.out)] <- x.at.cross
    a.track$y[cross.row + as.numeric(moving.out)] <- y.at.cross
    a.track$time[cross.row + as.numeric(moving.out)] <- t.at.cross
    a.track$inside[cross.row + as.numeric(moving.out)] <- TRUE
    return(a.track)
  }
  get.e.or.w.cross <- function(a.track, cross.row) {
    moving.out <- a.track$inside[cross.row] && !a.track$inside[cross.row +
                                                                 1]
    dx <- a.track$x[cross.row + 1] - a.track$x[cross.row]
    edge <- ifelse(abs(a.track$x[cross.row] - bb["x", "max"]) <
                     abs(a.track$x[cross.row] - bb["x", "min"]), "max",
                   "min")
    dx.to.cross <- a.track$x[cross.row] - bb["x", edge]
    proportion.to.cross <- abs(dx.to.cross/dx)
    x.at.cross <- a.track$x[cross.row] + proportion.to.cross *
      dx
    dy <- a.track$y[cross.row + 1] - a.track$y[cross.row]
    y.at.cross <- a.track$y[cross.row] + proportion.to.cross *
      dy
    dt <- as.numeric(difftime(a.track$time[cross.row + 1],
                              a.track$time[cross.row], units = "sec", tz = "UTC"))
    t.at.cross <- a.track$time[cross.row] + proportion.to.cross *
      dt
    a.track$y[cross.row + as.numeric(moving.out)] <- y.at.cross
    a.track$x[cross.row + as.numeric(moving.out)] <- x.at.cross
    a.track$time[cross.row + as.numeric(moving.out)] <- t.at.cross
    a.track$inside[cross.row + as.numeric(moving.out)] <- TRUE
    return(a.track)
  }
  all.cropped.tracks <- data.frame()
  for (id in unique(data$id)) {
    one.track <- data[data$id == id, ]
    one.track$inside <- one.track$x >= bb["x", "min"] &
      one.track$x <= bb["x", "max"] & one.track$y >= bb["y",
                                                        "min"] & one.track$y <= bb["y", "max"]
    inside <- subset(one.track, inside == TRUE)
    if (nrow(inside) < 1) {
      next
    }
    nrw <- nrow(one.track)
    cross.row <- which(one.track$inside[2:nrw] != one.track$inside[1:(nrw -
                                                                        1)])
    if (length(cross.row) == 0) {
      cropped.track <- one.track[, c("id", "time", "speed",
                                     "type", "length", "x", "y")]
      all.cropped.tracks <- rbind(all.cropped.tracks,
                                  cropped.track)
      next
    }
    one.track.join <- list()
    for (i in 1:length(cross.row)) {
      crossing.n.or.s <- (one.track$y[cross.row[i]] <
                            bb["y", "max"] && one.track$y[cross.row[i] +
                                                            1] > bb["y", "max"]) || (one.track$y[cross.row[i]] <
                                                                                       bb["y", "min"] && one.track$y[cross.row[i] +
                                                                                                                       1] > bb["y", "min"]) || (one.track$y[cross.row[i]] >
                                                                                                                                                  bb["y", "max"] && one.track$y[cross.row[i] +
                                                                                                                                                                                  1] < bb["y", "max"]) || (one.track$y[cross.row[i]] >
                                                                                                                                                                                                             bb["y", "min"] && one.track$y[cross.row[i] +
                                                                                                                                                                                                                                             1] < bb["y", "min"])
      if (crossing.n.or.s) {
        one.track.partial <- get.n.or.s.cross(one.track,
                                              cross.row[i])
      } else {
        one.track.partial <- get.e.or.w.cross(one.track,
                                              cross.row[i])
      }
      one.track$time <- paste0(round(one.track$time, "secs"))
      one.track.partial$time <- paste(round(one.track.partial$time,
                                            "secs"))
      one.track.partial.sub <- subset(one.track.partial,
                                      !time %in% c(one.track$time))
      one.track$time <- ifelse(nchar(one.track$time) <
                                 11, paste0(one.track$time, " 00:00:00"), one.track$time)
      one.track$time <- as.POSIXct(one.track$time, tz = "UTC",
                                   format = "%Y-%m-%d %H:%M:%S")
      one.track.partial.sub$time <- as.POSIXct(one.track.partial.sub$time,
                                               tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
      one.track.join <- rbind(one.track.join, one.track.partial.sub)
    }
    one.track <- subset(one.track, one.track$inside == TRUE)
    one.track <- rbind(one.track, one.track.join)
    one.track <- one.track[order(one.track$time), ]
    cropped.track <- one.track[, c("id", "time", "speed",
                                   "type", "length", "x", "y")]
    all.cropped.tracks <- rbind(all.cropped.tracks, cropped.track)
    rm(one.track, cropped.track)
  }
  all.cropped.DS <- new("DeponsShips")
  all.cropped.DS@crs <- as.character(crs(landsc))
  all.cropped.DS@landscape <- landscape(landsc)
  all.cropped.DS@title <- title
  ids <- sort(unique(all.cropped.tracks$id))
  all.routes <- list()
  if (!(startday %in% c("NA"))) {
    startday <- substr(startday, 1, 10)
    startday <- as.POSIXct(startday, tz = "UTC")
    testday <- as.POSIXlt(startday, tz = "UTC")
    if (((testday$mon %in% c(4,6,7,9,11)) && (testday$mday == 31)) ||
        ((testday$mon == 1) && (testday$mday == 29))) {
      stop("startday and endday cannot be on dates not accommodated in the model year (Feb 29, May/Jul/Aug/Oct/Dec 31)")
    }
  }
  if (!(endday %in% c("NA"))) {
    endday <- substr(endday, 1, 10)
    endday <- as.POSIXct(endday, tz = "UTC")
    testday <- as.POSIXlt(endday, tz = "UTC")
    if (((testday$mon %in% c(4,6,7,9,11)) && (testday$mday == 31)) ||
        ((testday$mon == 1) && (testday$mday == 29))) {
      stop("startday and endday cannot be on dates not accommodated in the model year (Feb 29, May/Jul/Aug/Oct/Dec 31)")
    }
    endday <- endday + 60 * 60 * 24 - 1
  }
  if ((startday %in% c("NA") && !(endday %in% c("NA")) || (!(startday %in% c("NA")) && endday %in% c("NA")))) {
    stop("either both or neither of startday and endday must be provided")
  }
  if (endday < startday)
    stop("endday should not be before startday")
  roundTimes <- function(one.track) {
    one.track$secs <- as.numeric(substr(format(one.track$time),
                                        18, 19))
    one.track$hour <- substr(format(one.track$time), 12,
                             13)
    one.track$mins <- as.numeric(substr(format(one.track$time),
                                        15, 16))
    one.track$mins <- ifelse(is.na(one.track$mins), 0, one.track$mins)
    one.track$secs <- ifelse(is.na(one.track$secs), 0, one.track$secs)
    times.to.round <- one.track[!one.track$mins %in% c(0,
                                                       30) | !one.track$secs == 0, ]
    if (nrow(times.to.round) > 0) {
      all.times.to.round <- list()
      for (j in 1:nrow(times.to.round)) {
        times.to.round.sub <- times.to.round[j, ]
        next.hour <- substr(format(times.to.round.sub$time +
                                     60 * 60), 12, 13)
        times.to.round.sub$time.below <- ifelse(times.to.round.sub$mins >=
                                                  0 & times.to.round.sub$mins < 30, as.character(paste0(substr(format(times.to.round.sub$time),
                                                                                                               1, 10), " ", times.to.round.sub$hour, ":00:00"),
                                                                                                 tz = "UTC"), as.character(paste0(substr(format(times.to.round.sub$time),
                                                                                                                                         1, 10), " ", times.to.round.sub$hour, ":30:00"),
                                                                                                                           tz = "UTC"))
        times.to.round.sub$time.above <- ifelse(times.to.round.sub$mins >=
                                                  0 & times.to.round.sub$mins < 30, as.character(paste0(substr(format(times.to.round.sub$time),
                                                                                                               1, 10), " ", times.to.round.sub$hour, ":30:00"),
                                                                                                 tz = "UTC"), as.character(paste0(substr(paste(times.to.round.sub$time),
                                                                                                                                         1, 10), " ", next.hour, ":00:00"), tz = "UTC"))
        if (j > 1) {
          all.times.to.round.compare <- all.times.to.round[,
                                                           1:7]
          all.times.to.round.compare$time <- as.POSIXct(all.times.to.round.compare$time,
                                                        tz = "UTC")
          one.track.compare <- one.track[, 1:7]
          combine <- rbind(all.times.to.round.compare,
                           one.track.compare)
          track.compare <- combine[order(combine$time),
          ]
        }
        else {
          track.compare <- one.track[, 1:7]
        }
        track.compare$time <- as.character(track.compare$time)
        track.compare$time <- ifelse(nchar(track.compare$time) <
                                       11, paste0(track.compare$time, " 00:00:00"),
                                     track.compare$time)
        time1 <- track.compare[track.compare$time ==
                                 times.to.round.sub$time.below, ]
        time2 <- track.compare[track.compare$time ==
                                 times.to.round.sub$time.above, ]
        if (nrow(time1) > 0 && nrow(time2) > 0) {
          times.to.round.sub$time <- times.to.round.sub$time.below
          times.to.round.sub <- times.to.round.sub[,
                                                   1:7]
          times.to.round.sub$remove <- "TRUE"
        }
        else {
          if (nrow(time1) == 0) {
            times.to.round.sub$time <- times.to.round.sub$time.below
            times.to.round.sub <- times.to.round.sub[,
                                                     1:7]
            times.to.round.sub$remove <- "FALSE"
          }
          else {
            times.to.round.sub$time <- times.to.round.sub$time.above
            times.to.round.sub <- times.to.round.sub[,
                                                     1:7]
            times.to.round.sub$remove <- "FALSE"
          }
        }
        all.times.to.round <- rbind(all.times.to.round,
                                    times.to.round.sub)
      }
      all.times.to.round <- all.times.to.round[all.times.to.round$remove ==
                                                 FALSE, ]
      all.times.to.round <- all.times.to.round[, 1:7]
      one.track.good <- one.track[one.track$mins %in%
                                    c(0, 30) & one.track$secs == 0, ]
      one.track.good <- one.track.good[, 1:7]
      one.track.good$time <- as.character(format(one.track.good$time))
      all.times.to.round$time <- as.character(format(all.times.to.round$time))
      one.track <- rbind(one.track.good, all.times.to.round)
      one.track <- one.track[order(one.track$time), ]
    }
    one.track$time <- as.POSIXct(one.track$time, format = c("%Y-%m-%d %H:%M:%S"),
                                 tz = "UTC")
    one.track <- one.track[, 1:7]
    return(one.track)
  }
  numberTicks <- function(startday, endday) {
    all.ticks <- data.frame(seq(startday, endday + 30 *
                                  60, 30 * 60))
    colnames(all.ticks) <- c("time")
    no.ticks <- nrow(all.ticks)
    if ((no.ticks - 1)%%48 != 0) {
      stop(paste0("ID ", ids[i], " - Tick number is wrong"))
    }
    return(all.ticks)
  }
  addMissingTicksStartEnd <- function(all.ticks, one.track,
                                      startday, endday, startday.track, endday.track) {
    if (startday.track[1] > startday[1]) {
      first.row <- one.track[1, ]
      time <- all.ticks[all.ticks$time < first.row$time,
      ]
      id <- rep(one.track$id[1], length(time))
      speed <- rep(0, length(time))
      type <- rep(first.row$type[1], length(time))
      length <- rep(first.row$length[1], length(time))
      x <- rep(first.row$x[1], length(time))
      y <- rep(first.row$y[1], length(time))
      rows.add <- data.frame(id, time, speed, type, length,
                             x, y)
      one.track <- rbind(rows.add, one.track)
    }
    if (endday.track[1] < endday[1]) {
      last.row <- one.track[nrow(one.track), ]
      time <- all.ticks[all.ticks$time > last.row$time,
      ]
      id <- rep(one.track$id[1], length(time))
      speed <- rep(0, length(time))
      type <- rep(last.row$type[1], length(time))
      length <- rep(last.row$length[1], length(time))
      x <- rep(last.row$x[1], length(time))
      y <- rep(last.row$y[1], length(time))
      rows.add <- data.frame(id, time, speed, type, length,
                             x, y)
      one.track <- rbind(one.track, rows.add)
    }
    return(one.track)
  }
  addMissingTicksMiddle <- function(one.track, match) {
    id <- rep(one.track$id[1], nrow(match))
    speed <- rep(0, nrow(match))
    type <- rep(one.track$type[1], nrow(match))
    length <- rep(one.track$length[1], nrow(match))
    x <- rep(NA, nrow(match))
    y <- rep(NA, nrow(match))
    rows.add <- data.frame(id, match, speed, type, length,
                           x, y)
    one.track <- rbind(one.track, rows.add)
    one.track <- one.track[order(one.track$time), ]
    NAs <- one.track[is.na(one.track$x), ]
    NAS_info <- list()
    for (k in 1:nrow(NAs)) {
      Na_sub <- NAs[k, ]
      times.after <- one.track[one.track$time > Na_sub$time,
      ]
      times.after <- times.after[!is.na(times.after$x),
      ]
      Na_sub$x <- times.after$x[1]
      Na_sub$y <- times.after$y[1]
      NAS_info <- rbind(NAS_info, Na_sub)
    }
    one.track <- one.track[!is.na(one.track$x), ]
    one.track <- rbind(one.track, NAS_info)
    one.track <- one.track[order(one.track$time), ]
    return(one.track)
  }
  lead_lag <- function(v, n) {
    if (n > 0)
      c(rep(NA, n), head(v, length(v) - n))
    else c(tail(v, length(v) - abs(n)), rep(NA, abs(n)))
  }
  collapsePauses <- function(one.track) {
    new_speeds <- ifelse(one.track$speed <= 0.1, 0, one.track$speed)
    recurringZero <- data.frame(new_speeds)
    recurringZero$recurringSpeed <- ifelse(recurringZero$new_speeds ==
                                             0, 1, 0)
    recurringZero$x <- one.track$x
    recurringZero$y <- one.track$y
    recurringZero$time <- one.track$time
    seq_length <- rle(recurringZero$recurringSpeed)$lengths
    NoIds <- length(seq_length)
    recurringZero$pause_no <- rep(1:NoIds, seq_length)
    recurringZero$duration <- 30
    recurringZero$duration <- ifelse(recurringZero$recurringSpeed ==
                                       0, 0, recurringZero$duration)
    pauses <- aggregate(recurringZero$duration, list(recurringZero$pause_no),
                        FUN = sum)
    recurringZero$pauseTime <- rep(pauses$x/30, seq_length)
    recurringZero$new_recurringSpeed <- ifelse(recurringZero$recurringSpeed ==
                                                 1, lead_lag(recurringZero$new_speeds, -1), recurringZero$new_speeds)
    recurringZero$new_recurringSpeed <- ifelse(is.na(recurringZero$new_recurringSpeed),
                                               0, recurringZero$new_recurringSpeed)
    recurringZero$new_pauseno <- ifelse(recurringZero$recurringSpeed ==
                                          0, lead_lag(recurringZero$pause_no, +1), recurringZero$pause_no)
    if (max(recurringZero$pauseTime, na.rm = TRUE) > 0) {
      pauses <- recurringZero[recurringZero$recurringSpeed ==
                                1, ]
      max_speed <- aggregate(pauses$new_recurringSpeed,
                             list(pauses$new_pauseno), FUN = max)
      new_x <- aggregate(pauses$x, list(pauses$pause_no),
                         FUN = mean)
      new_y <- aggregate(pauses$y, list(pauses$pause_no),
                         FUN = mean)
      new_time <- aggregate(pauses$time, list(pauses$pause_no),
                            FUN = min)
      new_pauseTime <- aggregate(pauses$pauseTime, list(pauses$pause_no),
                                 FUN = max)
      if (new_speeds[length(new_speeds)] == 0) {
        new_pauseTime$x[nrow(new_pauseTime)] <- new_pauseTime$x[nrow(new_pauseTime)] -
          1
      }
      pauses_collapsed <- data.frame(max_speed$x, rep(1),
                                     new_x$x, new_y$x, as.POSIXct(new_time$x, tz = "UTC"),
                                     max_speed$Group.1,
                                     new_pauseTime$x, rep(1), max_speed$Group.1)
      colnames(pauses_collapsed) <- c("new_speeds", "recurringSpeed",
                                      "x", "y", "time", "pause_no", "pauseTime", "new_recurringSpeed",
                                      "new_pauseno")
      movingperiods <- recurringZero[recurringZero$recurringSpeed ==
                                       0 | is.na(recurringZero$new_recurringSpeed),
      ]
      movingperiods <- recurringZero[!recurringZero$new_pauseno %in%
                                       c(pauses_collapsed$new_pauseno), ]
      movingperiods <- movingperiods[-c(7)]
      pauses_collapsed$new_recurringSpeed <- pauses_collapsed$new_speeds
      pauses_joined <- rbind(movingperiods, pauses_collapsed)
      pauses_joined <- pauses_joined[order(pauses_joined$time),
      ]
    }
    else {
      pauses_joined <- recurringZero
    }
    one.route <- data.frame(x = pauses_joined$x, y = pauses_joined$y,
                            speed = pauses_joined$new_speeds, pause = pauses_joined$pauseTime)
    names(one.route)[4] <- "pause"
    return(one.route)
  }
  for (i in 1:length(ids)) {
    id <- ids[i]
    one.track <- all.cropped.tracks[all.cropped.tracks$id ==
                                      id, ]
    one.track <- one.track[order(one.track$time), ]
    one.track$time <- paste(round(one.track$time, "secs"))
    one.track$time <- ifelse(nchar(one.track$time) < 12,
                             paste(one.track$time, "00:00:00", sep = " "), one.track$time)
    one.track$time <- as.POSIXct(one.track$time, tz = "UTC",
                                 format = "%Y-%m-%d %H:%M:%S")
    one.track <- roundTimes(one.track)
    startday.track <- min(one.track$time)
    endday.track <- max(one.track$time)
    if (!(startday %in% c("NA")) && !(endday %in% c("NA"))) {
      all.ticks <- numberTicks(startday, endday)
    }
    else {
      all.ticks <- data.frame(seq(startday.track, endday.track,
                                  30 * 60))
      colnames(all.ticks) <- c("time")
    }
    if (!(startday %in% c("NA"))) {
      one.track <- addMissingTicksStartEnd(all.ticks,
                                           one.track, startday, endday, startday.track,
                                           endday.track)
    }
    match <- subset(all.ticks, !time %in% c(one.track$time))
    if (nrow(match) > 0) {
      one.track <- addMissingTicksMiddle(one.track, match)
    }
    one.route <- collapsePauses(one.track)
    if (!(startday %in% c("NA"))) {
      one.route2 <- one.route
      one.route2$index <- 1:nrow(one.route2)
      one.route2$count.ticks <- 1
      one.route2$count.ticks <- ifelse(one.route2$pause >
                                         0, one.route2$count.ticks + one.route2$pause,
                                       one.route2$count.ticks)
      ticks <- sum(one.route2$count.ticks)
      if (!((ticks - 1)%%48) == 0)
        stop(paste0("ID ", ids[i], " - Tick number is wrong"))
    }
    row.nonas <- which(!is.na(one.route$x) & !is.na(one.route$y) &
                         !is.na(one.route$speed) & !is.na(one.route$pause))
    subset.nonas <- one.route[row.nonas, ]
    if (nrow(subset.nonas) < nrow(one.route)) {
      stop("NA values in ship route")
    }
    one.route$x <- ifelse(one.route$x == bb[1, 1], one.route$x +
                            0.01, one.route$x)
    one.route$x <- ifelse(one.route$x == bb[1, 2], one.route$x -
                            0.01, one.route$x)
    one.route$y <- ifelse(one.route$y == bb[2, 1], one.route$y +
                            0.01, one.route$y)
    one.route$y <- ifelse(one.route$y == bb[2, 2], one.route$y -
                            0.01, one.route$y)
    all.routes[[i]] <- one.route
  }
  names(all.routes) <- paste("Route", ids, sep = "_")
  routes(all.cropped.DS) <- all.routes
  all.ships <- unique(all.cropped.tracks[, c("id", "type",
                                             "length")])
  names(all.ships)[1] <- "name"
  all.ships$route <- paste0("Route_", unique(all.cropped.tracks$id))
  ships(all.cropped.DS) <- all.ships
  validObject(all.cropped.DS)
  return(all.cropped.DS)
} # end of ais.to.DeponsShips



#'Identify and parameterize stationary active ships in a DeponsShips object
#'
#'@description
#'Identifies ship positions in a DeponsShips object where the ship is stationary
#'(pausing) but potentially still actively using its engine (bollard pushing or
#'using dynamic positioning system), and if desired assigns a suitable non-zero
#'speed to ensure noise generation at that time point. Candidates may be found
#'either among all ships that are at a minimum distance from shore, or among
#'those that are close to specific structures of interest, such as wind turbines.
#'
#'@details
#'When a DeponsShips object is created using [ais.to.DeponsShips()], positions
#'are interpolated at 30-minute intervals (ticks). If a ship's position does not
#'change during sequential ticks, these ticks are combined into a pause of the
#'appropriate duration, with a movement speed of 0. However, in some cases, an
#'unmoving ship is actually using its engine to hold position, such as a crew
#'transfer vessel performing a bollard push against a turbine pile, or an
#'offshore supply vessel using a dynamic position system (DPS). Under these
#'circumstances, the ship should emit noise to affect porpoise agents.
#'This function attempts to identify and rewrite such pausing instances in an
#'existing DeponsShips object. A pause is converted into an active stationary
#'position by assigning a non-zero speed and thus noise emission. Note that
#'assigning a speed does not translate into movement, as movement in the model
#'is only derived from position changes, and speed is only used to drive noise
#'calculation.
#'
#'The intended functionality is to first run the function using 'action = "check"'
#'to return a table of candidate instances. After this has been inspected and
#'thinned as desired by the user, the function is run again using 'action = "replace"'
#'while providing the table as 'candidates', which returns a DeponsShips object
#'where the identified candidate pauses have been replaced with speed values.
#'No testing criteria (distcrit, landscape, stucture_locations, start_day, start_times)
#'are required for a "replace" run, as the instances provided as 'candidates'
#'are then modified without further checks.
#'
#'Only ships with type "Other" or "Government/Research" (following the key
#'in Table 1 in MacGillivray & de Jong 2021) are tested, as these categories
#'contain the survey, construction and crew transfer ships that are the primary
#'candidate types. Passenger, recreational, fishing and cargo vessels are assumed
#'to not or rarely use DPS and are omitted. However, the "Other" category also
#'contains vessels that hold position for extended periods without using DPS,
#'such as jack-up rigs and platforms; also, ship type codes provided in AIS data
#'are frequently unreliable. We therefore strongly suggest that the user should
#'carefully scrutinize the candidates table produced in a "check" run, look up
#'vessels by their MMSI code, and remove any false positives from the table before
#'processing it in a "replace" run.
#'
#'The inserted speed values are 7.4 knots for "Other" and 8 knots for "Government/Research", based on the class reference speeds in MacGillivray & de Jong (2021).
#'
#'When 'distcrit = "shore"', pause instances are additionally tested against the following criteria: 1) not in a cell (400x400 m) directly adjacent to land, to exclude berthed ships; 2) not in a cell at the map boundary, as [ais.to.DeponsShips()] will create inactive (pausing) placeholder positions at the point of entry if a ship enters the map with a delay after the object's start, or at the point of exit if it leaves before the end of the object's duration; 3) not in the first or last position of the ship's track (same reason).
#'
#'When candidates are identified based on proximity to a list of structures, a maximum distance of 97.72 m is allowed, based on an estimate of mean AIS positioning error (Jankowski et al. 2021).
#'
#'@param x DeponsShips object
#'@param action Character. If "check" (default), returns a data frame of pause positions that are candidates for stationary activity based on the selected criteria. If "replace" and a candidates data frame is provided, returns a DeponsShip object where the pauses identified in the data frame have been converted to stationary active status (i.e., a non-zero speed has been assigned)
#'@param candidates A data frame of pause positions that are candidates for stationary activity. Required if 'action = "replace"'. Generated by using 'action = "check"'
#'@param distcrit Character. Main criterion for finding candidates for stationary activity. If "shore" (default), all ship positions in open water are eligible, subject to a number of secondary criteria (see Details). In this case, a DeponsRaster must be provided that allows determination of distance from land (see below). If any other value or NA, only ship positions close to specified structure locations (such as turbine piles) are eligible, and these locations must be provided via 'structure_locations' (see below). In this case, a start day for the ship records and individual start times for the structure locations may also be provided to allow simulation of an ongoing construction process (see below)
#'@param landscape A DeponsRaster where land areas are indicated as NA (e.g., the prey map for the simulation). Required if 'distcrit = "shore"' to determine distance of candidate positions from land
#'@param structure_locations A data frame with columns "id", "x" (numerical) and "y" (numerical), and one row for each structure that is to be used as a proximity criterion for finding candidates. Required if distcrit != 'shore'
#'@param start_day A character string or POSIX object of the form 'YYYY-MM-DD HH:MM:SS'. Defines the start time of x. Optional; can be provided together with start_times if distcrit != 'shore', to allow checking whether structures under construction are present at a given time point
#'@param start_times A data frame with columns "time" (character string or POSIX of format 'YYYY-MM-DD HH:MM:SS') and "id", and one row for each structure that is to be used as a proximity criterion for finding candidates. Defines time from which onward the structure is present. Optional; can be provided together with start_day if distcrit != 'shore', to allow checking whether structures under construction are present at a given time point
#'@param verbose Logical (default False). If True, writes a summary of each candidate to the console during "check" runs
#'
#'@returns
#'If 'action = "check"' (default), returns a data frame with columns "route_number", "ship_name", "ship_type", "route_pos" (position number along route), and "pauses" (number of pauses at this position), with one row for each position that is a candidate for stationary activity based on the selected criteria. If "replace" and a candidates data frame is provided, returns a DeponsShip object where the pauses identified in the data frame have been converted to stationary active status (i.e., a non-zero speed has been assigned).
#'
#'@section References:
#'MacGillivray, A., & de Jong, C (2021). A reference spectrum model for estimating source levels of marine shipping based on Automated Identification System data. Journal of Marince Science and Engineering, 9(4), 369. doi:10.3390/jmse9040369"
#'
#'Jankowski, D, Lamm A, & Hahn, A (2021). Determination of AIS position accuracy and evaluation of reconstruction methods for maritime observation data. IFAC-PapersOnLine, 54(16), 97-104. doi:10.1016/j.ifacol.2021.10.079
#'
#'@examples
#'\dontrun{
#'data(shipdata)
#'data(bathymetry)
#'candidates <- make.stationary.ships(shipdata,
#'                                    landscape = bathymetry,
#'                                    verbose = T)
#'shipdata.updated <- make.stationary.ships(shipdata,
#'                                          action = "replace",
#'                                          candidates = candidates,
#'                                          landscape = bathymetry)}
#'
#'@seealso \code{\link{ais.to.DeponsShips}} for creation of DeponsShips objects (including calculated speeds) from AIS data
make.stationary.ships <- function(x,
                                  action = "check",
                                  candidates = NULL,
                                  distcrit = "shore",
                                  landscape = NULL,
                                  structure_locations = NULL,
                                  start_day = NA,
                                  start_times = NULL,
                                  verbose = F) {

  if (!inherits(x, "DeponsShips")) {
    stop("'x' must be a DeponsShips object")
  }

  neighbors <- function(routepos) { # define function to test if any neighboring cell is land. Return TRUE if no land as neighbor
    cell <- raster::cellFromXY(landscape, routepos)
    cellrow <- raster::rowFromCell(landscape, cell)
    cellcol <- raster::colFromCell(landscape, cell)
    for (xpos in -1:1) {
      if ((cellrow + xpos) %in% c(0, nrow(landscape) + 1)) next
      for (ypos in -1:1) {
        if ((cellcol + ypos) %in% c(0, ncol(landscape) + 1)) next
        if (is.na(landscape[cellrow + xpos, cellcol + ypos])) return(FALSE)
      }
    }
    return(TRUE)
  }

  if (action == "check") { # check mode: returns table of candidate instances
    if (distcrit == "shore") {
      if (!inherits(landscape, "DeponsRaster")) {
        stop("'landscape' must be a DeponsRaster")
      }
      ext <- as.numeric(unname(landscape@ext)) # get extent from DeponsRaster, create basic raster with same extent
      ext <- matrix(ext, nrow=2, ncol=2)
      landscape <- raster::raster(landscape@data)
      raster::extent(landscape) <- ext
    }

    if (distcrit != "shore") {
      if (!inherits(structure_locations, "data.frame")) {
        stop("'structure_locations' must be a data frame with columns 'id', 'x' (num) and 'y' (num)")
      }
      if (!all(c("id", "x", "y") %in% names(structure_locations))) {
        stop("'structure_locations' must be a data frame with columns 'id', 'x' (num) and 'y' (num)")
      }
      if (!is.numeric(structure_locations$x) || !is.numeric(structure_locations$y)) {
        stop("'structure_locations' must be a data frame with columns 'id', 'x' (num) and 'y' (num)")
      }
      if ((!is.na(start_day) && is.null(start_times)) || (is.na(start_day) && !is.null(start_times))) {
        stop("To check against times when structures are established, both 'start_day' (string or POSIXct of format 'YYYY-MM-DD HH:MM:SS') and 'start_times' (data frame with columns 'time' [same format as 'start_day'] and 'id') must be provided")
      }
      if (!is.na(start_day)) {
        start_day <- try(as.POSIXct(start_day, tz = "UTC"))
        if (!("POSIXct" %in% class(start_day))) {
          stop("'start_day' must be of the form 'YYYY-MM-DD HH:MM:SS' for conversion to POSIXct")
        }
      }
      if (!is.null(start_times)) {
        if (!inherits(start_times, "data.frame")) {
          stop("'start_times' must be a data frame with columns 'time' (string or POSIXct of format 'YYYY-MM-DD HH:MM:SS') and 'id'")
        }
        if (!all(c("time", "id") %in% names(start_times))) {
          stop("'start_times' must be a data frame with columns 'time' (string or POSIXct of format 'YYYY-MM-DD HH:MM:SS') and 'id'")
        }
        test <- try(as.POSIXct(start_times$time[1], tz = "UTC"))
        if (!("POSIXct" %in% class(test))) {
          stop("column 'time' in 'start_times' must be of the form 'YYYY-MM-DD HH:MM:SS' for conversion to POSIXct")
        }
      }
    }

    recording.table <- data.frame()

    for (i2 in 1:length(x@routes$name)) {

      if (x@ships$type[i2] %in% c("Other", "Government/Research")) {  # test 1: correct ship type
        shipmatrix <- as.matrix(x@routes$route[[i2]][1:2]) # get all route positions

        if (distcrit == "shore") { # check by criterion: distance to shoreline

          good.pos <- which(unname(apply(shipmatrix, 1, neighbors))) # vector of route positions that have no land neighbors
          if (length(good.pos > 0)) {
            for (i3 in 1:length(good.pos)) { # test 2: no land neighbors
              pausestring <- x@routes$route[[i2]]$pause
              # test group 3:
              if (pausestring[good.pos[i3]] > 0 && # is pausing
                  !(good.pos[i3] %in% c(1, nrow(shipmatrix))) &&  # AND is not the first or last position in the route (as these will often be long-time "ghost" parking positions for ships for which there are not yet/no more data)
                  !(shipmatrix[good.pos[i3], 1] < raster::extent(landscape)[1] + 400) && # AND is not in a cell at the map boundary (same reason)
                  !(shipmatrix[good.pos[i3], 1] > raster::extent(landscape)[2] - 400) &&
                  !(shipmatrix[good.pos[i3], 2] < raster::extent(landscape)[3] + 400) &&
                  !(shipmatrix[good.pos[i3], 2] > raster::extent(landscape)[4] - 400)) {

                new.records <- as.data.frame(rbind(c(i2,
                                                     x@ships$name[i2],
                                                     x@ships$type[i2],
                                                     good.pos[i3],
                                                     pausestring[good.pos[i3]])))
                names(new.records) <- c("route_number", "ship_name", "ship_type", "route_pos", "pauses")
                recording.table <- rbind(recording.table, new.records)

                if (verbose) {
                  message(paste0("Pauses to replace: ship ", i2,
                                 " (", x@ships$name[i2], ", ", x@ships$type[i2], "), ",
                                 "route position ", good.pos[i3], ", ", pausestring[good.pos[i3]], " pauses"))
                  message(paste0("Coordinates: ", shipmatrix[good.pos[i3],1], ", ", shipmatrix[good.pos[i3],2]))
                }
              }
            }
          }
        }

        else { # check by criterion: proximity to list of structures

          # calculate matrix of distances between each route position and each structure position
          distmatrix <- apply(shipmatrix, 1, function(x) apply(structure_locations[,3:4], 1, function(y) sqrt(crossprod(x-y))))

          near.pos <- which(distmatrix <= 97.72, arr.ind = T) # test 2: close enough to structure. Resultant array indices correspond to row (containing number of structure) and column (containing route position number) of instances that satisfy distance requirement

          if (nrow(near.pos > 0)) {
            for (i3 in 1:nrow(near.pos)) {

              structure_id <- near.pos[i3,1]
              routepos_id <- near.pos[i3,2]

              pausestring <- x@routes$route[[i2]]$pause
              sub.pausestring <- pausestring[1:routepos_id]
              ticks.at.pos <- sum(sub.pausestring) + length(sub.pausestring)

              checkmark <- 1
              if (pausestring[routepos_id] == 0) checkmark <- 0 # test 3: is pausing

              if (!is.na(start_day) && !is.null(start_times)) { # test 4: if time data given and comparison entry for time both present and after structure presence start time

                if (!(structure_locations$id[structure_id] %in% start_times$id)) checkmark <- 0 else {
                  the.start_time <- as.POSIXct(start_times$time[start_times$id == structure_locations$id[structure_id]], tz ="UTC")
                  if (start_day + ticks.at.pos*30*60 <= the.start_time) checkmark <- 0
                }
              }

              if (checkmark == 1) { # if still a valid instance after tests, modify to remove pauses and replace with stationary speed positions

                new.records <- as.data.frame(rbind(c(start_day + ticks.at.pos*30*60,
                                                     i2,
                                                     x@ships$name[i2],
                                                     x@ships$type[i2],
                                                     routepos_id,
                                                     pausestring[routepos_id],
                                                     structure_locations$id[structure_id])))
                names(new.records) <- c("time", "route_number", "ship_name", "ship_type", "route_pos", "pauses", "structure")
                recording.table <- rbind(recording.table, new.records)

                if (verbose) {
                  message(paste0("Pauses to replace: ship ", i2,
                                 " (", x@ships$name[i2], ", ", x@ships$type[i2], "), ",
                                 "route position ", routepos_id, ", ", pausestring[routepos_id], " pauses"))
                  message(paste0("Structure: ", structure_locations$id[structure_id]))
                }
              }
            }
          }
        }
      }
    }
    if (nrow(recording.table) == 0) {
      message("No candidates found")
      return()
    }
    recording.table[,4] <- as.numeric(unlist(recording.table[,4]))
    recording.table[,5] <- as.numeric(unlist(recording.table[,5]))
    return(recording.table)

  } else { # replace mode: returns updated DeponsShips object

    if (!inherits(candidates, "data.frame")) {
      stop("'candidates' must be a data frame including columns 'route_number', 'route_pos', 'pauses', and 'ship_type', with a row for each instance to be replaced")
    }
    if (!all(c("route_number", "route_pos", "pauses", "ship_type") %in% names(candidates))) {
      stop("'candidates' must be a data frame including columns 'route_number', 'route_pos', 'pauses', and 'ship_type', with a row for each instance to be replaced")
    }

    recording.table <- candidates
    new.x <- x

    recording.table$route_number <- as.numeric(recording.table$route_number)
    recording.table$route_pos <- as.numeric(recording.table$route_pos)
    recording.table$pauses <- as.numeric(recording.table$pauses)

    the.routes <- unique(recording.table$route_number)

    for (i2 in 1:length(the.routes)) {
      one.route <- recording.table[recording.table$route_number %in% the.routes[i2],]
      route.as.list <- split(x@routes$route[[the.routes[i2]]], seq(nrow(x@routes$route[[the.routes[i2]]]))) # create list in which each entry is one route position

      for (i3 in 1:nrow(one.route)) {
        slice <- x@routes$route[[one.route$route_number[i3]]][one.route$route_pos[i3],]
        new.slice <- slice
        if (one.route$ship_type[1] == "Government/Research") new.slice$speed <- 8 else new.slice$speed <- 7.4
        new.slice$pause <- 0
        new.slice <- do.call("rbind", replicate(one.route$pauses[i3] + 1, new.slice, simplify = F)) # pauses converted to stationary speed positions, plus add on one to represent the original position
        new.slice$speed[nrow(new.slice)] <- slice$speed # set speed of last position back to original (denoting the speed at which the ship eventually leaves the position)

        route.as.list[[one.route$route_pos[i3]]] <- new.slice # insert created positions into list-form route
      }
      new.route <- do.call(rbind, route.as.list) # convert into data frame again
      rownames(new.route) <- NULL
      new.x@routes$route[[the.routes[i2]]] <- new.route
    }
    return(new.x)
  }
} # end of 'make.stationary.ships'

