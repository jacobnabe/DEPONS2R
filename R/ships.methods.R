



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
#' 'name', 'type', 'length', 'route' 'tickStart', and 'tickEnd'. Info can be
#' extracted using the \code{\link{ships}} function.
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
#' @param fname Name of the file (character) where ship routes and ships
#' are defined.
#' @param title Optional character string with the name of the simulation
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


#' @title Writing DEPONS ship files
#' @aliases write,DeponsShips-method
#' @aliases write.DeponsShips
#' @description Function  for writing a json-file for controlling
#' how ship agents behave in DEPONS. Ships move along pre-defined routes in 30-min
#' time steps. The routes are defined by the fix-points provided in the
#' json file, and the geographic projection is assumed to match that of the
#' landscape. The projection is not stored as part of the json file.
#' @param x Name of the DeponsShips object to be exported
#' @param file Name of the output file (character)
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
  if(class(value)!= 'data.frame') stop("'value' must be a data.frame")
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
  if(class(value)!= 'data.frame') stop("'value' must be a data.frame")
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
  if (class(value) != "list") stop("'value' must be a list with one element per ship route")
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
#' @description Crop one or more ship tracks to the extent of a landscape
#' and convert the data into a \code{DeponsShips-class} object. The in the
#' 'pause' in the \code{DeponsShips} object corresponds to the number of half-hour
#' intervals where ships do not move, e.g. when in a port.
#' @param data data.frame with ship positions and the times at which the
#' positions were recorded
#' @param landsc A \code{DeponsRaster} object corresponding to the
#' landscape that the ships move in. It is assumed that the spatial projection
#' of the ship positions corresponds to that of the DeponsRaster object
#' @param title Title of the output object
#' @return Returns a \code{DeponsShips} object
#' @seealso \code{\link{aisdata}} can be used as input to as.DeponsShips
#' @export ais.to.DeponsShips
#' @examples
#' data(aisdata)
#' plot(aisdata$x, aisdata$y, type="n", asp=1)
#' ids <- sort(unique(aisdata$id))
#' my.colors <- rainbow(length(ids))
#' for (i in length(ids)) {
#'   id <- ids[i]
#'   points(aisdata$x[aisdata$id==id], aisdata$y[aisdata$id==id],
#'      cex=0.6, col=my.colors[i])
#' }
#' data(bathymetry)
#' plot(bathymetry, add=TRUE)
#' depons.ais <- ais.to.DeponsShips(aisdata, bathymetry)
#' the.routes <- routes(depons.ais)
#' for (i in 1:length(ids)) {
#'   points(the.routes[[i]]$x, the.routes[[i]]$y,
#'          cex=0.6, pch=16, col=my.colors[i])
#' }
# setMethod("as", signature("data.frame", "DeponsRaster"), function(data, landsc, title="NA") {
ais.to.DeponsShips <- function(data, landsc, title="NA") {
  if(class(data)!="data.frame")
    stop("data must be a data.frame with the variables 'id', 'time', 'speed', 'type', 'length', 'x', and 'y'")
  if(!all(names(data) %in% c('id', 'time', 'speed', 'type', 'length', 'x', 'y')))
    stop("data must be a data.frame with the variables 'id', 'time', 'speed', 'type', 'length', 'x', and 'y'")
  if(!all(c('id', 'time', 'speed', 'type', 'length', 'x', 'y') %in% names(data)))
    stop("data must be a data.frame with the variables 'id', 'time', 'speed', 'type', 'length', 'x', and 'y'")
  if(!(class(data$speed)=="numeric")) stop("'speed' must be numeric")
  if(!(class(data$length)=="numeric" || class(data$length)=="integer")) stop("'length' must be numeric")
  if(!(class(data$x)=="numeric")) stop("'x' must be numeric")
  if(!(class(data$y)=="numeric")) stop("'y' must be numeric")
  if(!(class(landsc)=="DeponsRaster")) stop("'landsc' must be a DeponsRaster object")
  message(paste0("Conversion assumes that 'x' and 'y' coordinates of ships use the '",
                 crs(landsc), "' projection"))
  time <- try(as.POSIXlt(data$time, tz="UTC"))
  if(!("POSIXlt" %in% class(time)))
    stop("'time' must be of the form 'YYYY-MM-DD HH:MM:SS' for conversion to POSIXlt")
  data$time <- time
  bb <- bbox(landsc)
  # For each track in 'data': find places where the track crosses bb and make linear intrapolation to
  # place new waypoint at the edge of the landscape -- then remove waypoints outside the edge from the route
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
  for (i in 1:length(ids)) {
    id <- ids[i]
    one.track <- all.cropped.tracks[all.cropped.tracks$id==id,]
    one.track<-one.track[order(one.track$time),]
    dx <- one.track$x[2:length(one.track$x)] - one.track$x[1:(length(one.track$x)-1)]
    dy <- one.track$y[2:length(one.track$y)] - one.track$y[1:(length(one.track$y)-1)]
    dt <- difftime(one.track$time[2:length(one.track$time)], one.track$time[1:(length(one.track$time)-1)],
                   units="secs")
    dist <- sqrt(dx^2 + dy^2)
    speed <- dist/as.numeric(dt)
    # convert to km per hour
    speed <- speed * 60 * 60 / 1000
    # convert to knots
    speed <- speed/1.85200
    # repeat last know speed for last buoy
    speed <- c(speed, speed[length(speed)])

    # Calculate duration of pauses & collapse rows where ship not moving:
    # Set 0.1 knots as a threshold for moving
    new_speeds<-ifelse(speed<=0.1, 0, speed)

    # Label recurring 0s as 1s & add lox/time information
    recurringZero<-data.frame(new_speeds)
    recurringZero$recurringSpeed<-ifelse(recurringZero$new_speeds==0, 1, 0)
    recurringZero$x<-one.track$x
    recurringZero$y<-one.track$y
    recurringZero$time<-one.track$time

    # Add a pause id so that pause duration can be summed by pause id
    seq_length<-rle(recurringZero$recurringSpeed)$lengths # Calculate duration of pause ids
    NoIds<-length(seq_length)
    recurringZero$pause_no<-rep(1:NoIds, seq_length)
    recurringZero$duration<-c(as.numeric(difftime(recurringZero$time[2:nrow(recurringZero)], recurringZero$time[1:nrow(recurringZero)-1], units=c("mins"))), 0)
    recurringZero$duration<-ifelse(recurringZero$recurringSpeed==0, 0, recurringZero$duration)

    # Sum recurring zeros to calculate duration of pauses  &
    # Find out what the next non-zero speed is and re-label this row
    pauses<-aggregate(recurringZero$duration, list(recurringZero$pause_no), FUN=sum)
    recurringZero$pauseTime<-rep(pauses$x/30, seq_length) # divide by 30 to get tick fraction
    lead_lag <- function(v, n) {
      if (n > 0) c(rep(NA, n), head(v, length(v) - n))
      else c(tail(v, length(v) - abs(n)), rep(NA, abs(n)))
    }
    recurringZero$new_recurringSpeed<-ifelse(recurringZero$recurringSpeed==0, lead_lag(recurringZero$recurringSpeed, +1), recurringZero$recurringSpeed)
    recurringZero$new_pauseno<-ifelse(recurringZero$recurringSpeed==0, lead_lag(recurringZero$pause_no, +1), recurringZero$pause_no)

    # Only collapse dataset if there are pauses
    if (max(recurringZero$pauseTime)>0) {

      # Collapse recurring zeros & average x/y lox per pause as AIS coordinates can jitter around
      pauses<-recurringZero[recurringZero$new_recurringSpeed==1,]
      max_speed<-aggregate(pauses$new_speeds, list(pauses$new_pauseno), FUN=max)
      new_x<-aggregate(pauses$x, list(pauses$new_pauseno), FUN=mean)
      new_y<-aggregate(pauses$y, list(pauses$new_pauseno), FUN=mean)
      new_time<-aggregate(pauses$time, list(pauses$new_pauseno), FUN=min)
      new_pauseTime<-aggregate(pauses$pauseTime, list(pauses$new_pauseno), FUN=max)

      # Create new data frame with these values per pause id
      pauses_collapsed<-data.frame(max_speed$x, rep(1), new_x$x, new_y$x, as.POSIXct(new_time$x), max_speed$Group.1,
                                   new_pauseTime$x, rep(1), max_speed$Group.1)
      colnames(pauses_collapsed)<-c("new_speeds", "recurringSpeed", "x", "y", "time", "pause_no", "pauseTime",
                                    "new_recurringSpeed", "new_pauseno")

      # Join with the rest of the dataset & arrange by time
      movingperiods<-recurringZero[recurringZero$new_recurringSpeed==0 |is.na(recurringZero$new_recurringSpeed) ,]
      movingperiods<-movingperiods[-c(7)]
      pauses_joined<-rbind(movingperiods, pauses_collapsed)
      pauses_joined<-pauses_joined[order(pauses_joined$time),]

    } else {

      pauses_joined<-recurringZero

    }

    # Save route characteristics
    one.route <- data.frame("x"=pauses_joined$x, "y"=pauses_joined$y, "speed"=pauses_joined$new_speeds, "pause"=pauses_joined$pauseTime)
    names(one.route)[4] <- "pause"
    all.routes[[i]] <- one.route
  }
  names(all.routes) <- paste("Route", ids, sep="_")
  routes(all.cropped.DS) <- all.routes
  # Now, add the ships
  all.ships <- all.cropped.tracks[, c("id", "type", "length")]
  names(all.ships)[1] <- "name"
  all.ships$route <- paste0("Route_", all.cropped.tracks$id)
  ships(all.cropped.DS) <- all.ships
  validObject(all.cropped.DS)
  return(all.cropped.DS)
}





