# DEPONS2R helper functions (related to multiple classes)

# devtools::document()  # Make rd files based on roxygen comments.
# devtools::install_deps()
# devtools::test(filter=NULL)
# devtools::build_manual()
# devtools::check(cleanup=FALSE) # check timings of examples: read log
# devtools::check


#' @title  Package for analyzing DEPONS simulation output
#' @name DEPONS2R
#' @description  Methods for analyzing population dynamics and movement tracks
#' simulated using the DEPONS model (v.3.2; \url{https://www.depons.eu}), for
#' manipulating input raster files, shipping routes and for analyzing sound
#' propagated from ships.
#'
#' The classes used in DEPONS2R include:
#' \itemize{
#' \item \code{\linkS4class{DeponsTrack}} movement tracks, read from "RandomPorpoise.XXX.csv" files
#' \item \code{\linkS4class{DeponsDyn}} population dynamics data, from "Statistics.XXX.csv" files
#' \item \code{\linkS4class{DeponsBlockdyn}} data from "PorpoisePerBlock.XXX.csv" files
#' \item \code{\linkS4class{DeponsShips}} data from "ships.json" files or from AIS data
#' \item \code{\linkS4class{DeponsRaster}} stores raster landscape files used in DEPONS
#' }
#' Here the \code{DeponsDyn} data include both changes in population size and energetics
#' through time for the entire landscape, whereas \code{DeponsBlockdyn} data include
#' variations in population size in different parts (or 'blocks') of the landscape.
#' XXX is the date and time when the simulation was finished.
NULL


#' @title Get simulation date
#' @name get.simtime
#' @description Returns the date and time (a \code{\link{POSIXlt}} object) when a specific
#' simulation was finished, obtained from the date stored as part of the file name.
#' The date format is system dependent, but the function attempts to extract the data
#' assuming that either the English or the local language is used.
#' @param fname Character string with name of the file to extract the simulation
#' date from, including the path
#' @param tz Time zone
#' @return Returns a \code{POSIXlt} object
#' @seealso \code{\link{get.latest.sim}}
#' @export get.simtime
get.simtime <- function (fname = NULL, tz = "UTC") {
  fn <- gsub("..", "_", fname, fixed = TRUE)
  ncf <- nchar(fn)
  ifelse (grepl("Sept", fn, fixed=T),
    time.string <- substr(fn, ncf - 24, ncf - 4),
    time.string <- substr(fn, ncf - 23, ncf - 4)) # accommodate "Sept" bug in Java
  time.string <- gsub("_", ":", time.string)
  template.months <- substr(100 + (1:12), 2, 3)
  template.dates <- paste0("2000-", template.months, "-01")
  system.months <- months(as.POSIXlt(template.dates, tz = "GMT"),
                          abbreviate = TRUE)
  time.string <- gsub(paste0("Jan|jan|", system.months[1]), "01", time.string)
  time.string <- gsub(paste0("Feb|feb|", system.months[2]), "02", time.string)
  time.string <- gsub(paste0("Mar|mar|", system.months[3]), "03", time.string)
  time.string <- gsub(paste0("Apr|apr|", system.months[4]), "04", time.string)
  time.string <- gsub(paste0("May|may|", system.months[5]), "05", time.string)
  time.string <- gsub(paste0("Jun|jun|", system.months[6]), "06", time.string)
  time.string <- gsub(paste0("Jul|jul|", system.months[7]), "07", time.string)
  time.string <- gsub(paste0("Aug|aug|", system.months[8]), "08", time.string)
  time.string <- gsub(paste0("Sept|sept|", system.months[9]), "09", time.string) # accommodate "Sept" bug in Java. Substituting longer option first prevents partial substitution for shorter option
  time.string <- gsub(paste0("Sep|sep|", system.months[9]), "09", time.string)
  time.string <- gsub(paste0("Oct|oct|", system.months[10]), "10", time.string)
  time.string <- gsub(paste0("Nov|nov|", system.months[11]), "11", time.string)
  time.string <- gsub(paste0("Dec|dec|", system.months[12]), "12", time.string)
  substr(time.string, 5, 5) <- "-"
  substr(time.string, 8, 8) <- "-"
  substr(time.string, 11, 11) <- " "
  time.posix <- as.POSIXlt(time.string, tz = tz)
  if (any(inherits(time.posix, "POSIXlt")))
    return(time.posix)
  stop(paste0("Could not extract date correctly from ", fname),
       ". Got ", time.string)
}



#' @title Get name of newest file
#' @name get.latest.sim
#' @description Returns the name of the newest simulation output of a particular
#' type within the specified directory. The date and time are extracted from the
#' file name.
#' @param type Type of simulation output to check; can be one of:
#' "dyn" (for looking in "Statistics.XX.csv" files),
#' "blockdyn" (for looking in "PorpoisePerBlock.XX.csv" files)
#' "track" (for looking in "RandomPorpoise.XX.csv" files).
#' @param dir Directory to look for simulation output in (character string)
#' @seealso \code{\link{read.DeponsBlockdyn}} for example.
#' @return Character string with the name of the most recent simulation output
#' file.
#' @export get.latest.sim
get.latest.sim <- function(type="dyn", dir) {
  if (!(type %in% c("dyn", "blockdyn", "track"))) {
    stop ("type must be either 'dyn', 'blockdyn', or 'track'")
  }
  the.dir <- dir
  if(type=="dyn") typenm <- "Statistics"
  if(type=="blockdyn") typenm <- "PorpoisePerBlock"
  if(type=="track") typenm <- "RandomPorpoise"
  outfiles <- grep(typenm, dir(the.dir))
  if(length(outfiles)==0) stop(paste0("No '", typenm, "' output found in directory"))
  fnms <- dir(the.dir)[outfiles]
  fnms2 <- as.list(fnms)
  ftime <- lapply(fnms2, FUN="get.simtime")
  for(i in 1:length(ftime)) ftime[[i]] <- format(ftime[[i]])
  outfile.nos <- data.frame("file.no"=outfiles, "time.str"=unlist(ftime))
  newest.sim <- sort(outfile.nos$time.str, decreasing=TRUE, index.return=TRUE)
  no.of.newest.sim <- outfile.nos$file.no[newest.sim$ix][1]
  latest.sim <- dir(the.dir)[no.of.newest.sim]
  return(latest.sim)
}


#' @title Make wind farm construction scenario
#' @name make.windfarms
#' @description Produce a hypothetical wind farm construction scenario, specifying
#' the position and timing of individual piling events, as well as the sound
#' source level. All wind farms are assumed to consist of the same number of
#' turbines, laid out in a rectangular grid. The start and end tick (i.e. the
#' number of half-hour intervals since simulation start) is generated based on
#' provided values for the time required for each piling and the time between
#' piling events.
#' @param area.file Name of the raster file (e.g. in .asc or .tif format)
#' specifying where the wind farms should be constructed.
#' @param area.def Value in \code{area.file} for the areas were wind farms can
#' be located
#' @param n.wf Number of wind farms to construct
#' @param n.turb Total number of turbines to construct
#' @param turb.dist Distance between turbines within a wind farm (meters)
#' @param min.wf.dist Minimum distance between wind farms (meters)
#' @param impact Sound source level (dB); sound emitted from turbines during
#' construction, i.e. from tickStart to tickEnd (including both start and end)
#' @param constr.start The tick at which construction of the first turbine starts.
#' @param constr.end The tick at which construction of the very last turbine in
#' the last wind farm ends.
#' @param constr.time The time it takes to construct a single wind turbine
#' (number of ticks).
#' @param constr.break Break between individual pilings within a wind farm,
#' counted in number of half-hour 'ticks'.
#' @param iterate Number of times to try finding a spot for a new wind farm that
#' is sufficiently far from the nearest neighbouring wind farm (>min.wf.dist).
#' The number also defines the number of random positions to search through.
#' @param verbose Logical; whether messages should be printed to console.
#' @param wf.coords Possible location of the south-western corner of the wind
#' farms. Defaults to the text "random", but can also be a data frame with
#' coordinates in the columns x and y.
#' @details The parameters \code{constr.start}, \code{constr.end}, \code{constr.time},
#' and \code{constr.break} are truncated to nearest integer value. Construction
#' of wind farms starts in WF001 at tick \code{constr.start}. Each turbine
#' foundation is piled over a period of \code{constr.time}, followed by a
#' noise-free period of \code{constr.break}. Several pile driving operations may
#' take place at the same time, to ensure that the last piling ends before \code{constr.end}.
#'
#' When exported, the dataframe should be specified as a tab-delimited .txt file placed in
#' folder /data/wind-farms. It will contain the columns 'id', 'x' (meters), 'y' (meters),
#' 'impact' (decibel), 'start' and 'end' (tick).
#' @return Dataframe specifying the position of each turbine in a wind farm,
#' along with the start time and end time for pile driving of the turbine
#' foundation and the sound source level during pile driving. Can be
#' exported as a text file and used for controlling DEPONS simulations.
#' @export make.windfarms
make.windfarms <- function(area.file, area.def, n.wf, n.turb, turb.dist,
                           min.wf.dist, impact, constr.start, constr.end,
                           constr.time, constr.break, iterate=10000, verbose=FALSE,
                           wf.coords="random") {
  # Check parameters
  ncf <- nchar(area.file)
  af.ext <- substr(area.file, ncf-2, ncf)
  if(af.ext != "tif" && af.ext != "asc") stop("'area.file' does not appear to be a raster")
  # Get extent of wind farm zone
  wf.area <- raster::raster(area.file)
  wf.area[,] <- ifelse(wf.area[,]==area.def, area.def, NA)
  wf.area <- raster::trim(wf.area)
  wf.ext <- raster::extent(wf.area)
  # Select coords for bottom-left corner of all wind farms
  get.start.pos <- function(the.wf.ext=wf.ext, the.wf.area=wf.area) {
    x <- stats::runif(iterate, min=the.wf.ext[1], max=the.wf.ext[2])
    y <- stats::runif(iterate, min=the.wf.ext[3], max=the.wf.ext[4])
    xy.pos <- data.frame(x,y)
    xy.val <- raster::extract(the.wf.area, xy.pos)
    # Use only pos within the wf area
    sel.rows <- which(xy.val==area.def)
    xy.pos <- xy.pos[sel.rows, ]
    row.names(xy.pos) <- NULL
    return(xy.pos)
  }
  get.turb.pos <- function(the.n.turb=n.turb, the.n.wf=n.wf, start.pos) {
    n.turb.per.wf <- ceiling(the.n.turb / the.n.wf)
    n.cols <- floor(sqrt(n.turb.per.wf))
    n.rows <- ceiling(n.turb.per.wf/n.cols)
    x.vals <- start.pos$x + seq(from=0, to=(n.cols-1)*turb.dist, by=turb.dist)
    x.vals <- rep(x.vals, n.rows)
    y.vals <- start.pos$y + seq(from=0, to=(n.rows-1)*turb.dist, by=turb.dist)
    y.vals <- rep(y.vals, each=n.cols)
    turb.pos <- data.frame("x"=x.vals, "y"=y.vals)
    turb.pos <- turb.pos[1:n.turb.per.wf ,]
    return(turb.pos)
  }
  # Make df with possible start pos; shrink list every time a wf is built
  # to make sure that each value is only used once
  if(is.character(wf.coords)) {
    if (wf.coords=="random") start.pos <- get.start.pos()
    else warning("Only the character string 'random' is supported")
  }
  if (!is.character(wf.coords)) {
    if (!inherits(wf.coords,"data.frame")) stop("wf.coords must be a data frame")
    xy.val <- raster::extract(wf.area, wf.coords)
    sel.rows <- which(xy.val==area.def)
    start.pos <- wf.coords[sel.rows, ]
    # points(start.pos, col="red", cex=0.2)
    row.names(start.pos) <- NULL
  }
  if(verbose) message(paste("Number of potential wind farm sites:", length(start.pos[,1])))
  all.wfs <- data.frame()
  wf.no <- 1
  k <- 0 # safety break
  while (wf.no <= n.wf) {
    cont.trying <- TRUE
    j <- 0  # safety break
    while(cont.trying) { # keep trying till all turbines are within wf.area
      if(nrow(start.pos)==0) stop(paste("No possible start pos for wind farm", wf.no))
      pos.to.use <- sample(1:length(start.pos[,1]), 1)
      turb.pos <- get.turb.pos(n.turb, n.wf, start.pos[pos.to.use,])
      # Remove 'pos.to.use' from 'start.pos'
      new.rows <- which(!(1:length(start.pos[,1]) %in% pos.to.use))
      start.pos <- start.pos[new.rows ,]
      rownames(start.pos) <- NULL
      # Check that turb pos are all withing wind farm area
      xy.val <- raster::extract(wf.area, turb.pos)
      # Check that all turbs are within wf.area
      if(all(!is.na(xy.val))) cont.trying <- FALSE
      # Check distance to existing wind farms
      xy.centre <- data.frame("x"=mean(turb.pos$x), "y"=mean(turb.pos$y))
      dist.centre.to.corner <- sqrt((turb.pos$x[1]-xy.centre$x)^2 +
                                      (turb.pos$y[1]-xy.centre$y)^2)
      if(!exists("all.wfs")) break
      if(nrow(all.wfs)==0) break # don't check dist to existing if there aren't any
      dist.to.existing <- sqrt((all.wfs$x-xy.centre$x)^2 +
                                 (all.wfs$y-xy.centre$y)^2) - dist.centre.to.corner
      if(min(dist.to.existing) < min.wf.dist) cont.trying <- TRUE
      j <- j+1
      if(j==iterate) stop(paste("Failed to generate wind farm", wf.no))
    }
    # Add id columns to wf file
    t <- row(turb.pos)[,1]
    turb.no <- substr(as.character(t + 1000), 2, 5)
    the.wf.no <- substr(as.character(wf.no + 1000), 2, 5)
    turb.names <- paste0("WF", the.wf.no, "_", turb.no)
    turb.pos <- cbind(wf.no, t, "id"=turb.names, turb.pos)
    names(turb.pos) <- c("wf", "t", "id", "x.coordinate", "y.coordinate")
    # points(turb.pos, cex=0.8, col="blue")
    all.wfs <- rbind(all.wfs, turb.pos)
    rm(turb.pos, xy.val, pos.to.use)
    if(verbose) message(paste("Wind farm", wf.no))
    wf.no <- wf.no+1
    k <- k+1
    if (k > iterate) stop(paste("Couldn't assign wind farm", wf.no))
  }
  # Add sound source level (=impact) to the wf data frame
  all.wfs <- cbind(all.wfs, impact)
  # Add piling schedule
  constr.start <- floor(constr.start)
  constr.end <- floor(constr.end)
  constr.time <- floor(constr.time)
  constr.break <- floor(constr.break)
  all.wfs$tickStart <- NA
  all.wfs$tickEnd <- NA
  # Add start and end time for one turbine at a time
  current.tick <- constr.start
  for (w in 1:max(all.wfs$wf)) {
    sel.wf <- all.wfs[all.wfs$wf==w,]
    expected.end.tick <- current.tick + max(sel.wf$t)*(constr.time+constr.break)
    # If construction is expected to last beyond after desired end time, introduce
    # a new pile driving machine to work from start of construction
    if(expected.end.tick > constr.end) current.tick <- constr.start
    for (tt in 1:max(sel.wf$t)) {
      sel.row <- which(all.wfs$wf==w & all.wfs$t==tt)
      all.wfs[sel.row,]$tickStart <- current.tick
      all.wfs[sel.row,]$tickEnd <- current.tick+constr.time
      current.tick <- current.tick + constr.time + constr.break
    }
  }
  all.wfs <- all.wfs[1:n.turb ,] # make sure total n turbs isn't too big
  return(all.wfs[, 3:8])
}



#' @title Convert tick number to time object
#' @name tick.to.time
#' @description Converts the number of ticks since the start of the simulation
#' to a specific date while taking into account that DEPONS assumes that there
#' are 360 days in a simulation year.
#' @param tick Numeric, or numeric vector; tick number
#' @param timestep Numeric; length of each simulation time step, in minutes.
#' Defaults to 30 minutes.
#' @param origin Character. The first day of the period that the simulation represents,
#' format: 'yyyy-mm-dd'.
#' @param tz Character. Valid time zone code (default UTC).
#' @param ... Optional parameters
#' @note The function assumes that there are 30 days in each month, except in
#' January, February and March with 31, 28 and 31 days, respectively.
#' @return object of class \code{\link{as.POSIXlt}}
#' @export tick.to.time
#' @seealso \code{\link{time.to.tick}} is the inverse of this function, converting dates to ticks
tick.to.time <- function(tick, timestep=30, origin="2010-01-01", tz = "UTC", ...) {
  old <- options()
  on.exit(options(old))  # Reset user options on exit
  if(min(tick)<0) stop("tick should be positive and numeric")
  if(!is.numeric(timestep)) stop("'timestep' should be numeric")
  if(!is.character(origin)) stop("'origin' should be character, in the format 'yyyy-mm-dd'")
  doy <- 1:360
  # Make months and days in sim year
  mm <- rep(NA, length(doy))
  mm <- ifelse(doy>=1 & doy<=31, 1, mm)
  mm <- ifelse(doy>=32 & doy<=59, 2, mm)
  mm <- ifelse(doy>=60 & doy<=90, 3, mm)
  mm <- ifelse(doy>=91, ceiling(doy/30), mm)
  dd <- rep(NA, length(doy))
  dd <- ifelse(doy>=1 & doy<=31, doy, dd)
  dd <- ifelse(doy>=32 & doy<=59, doy-31, dd)
  dd <- ifelse(doy>=60 & doy<=90, doy-59, dd)
  dd <- ifelse(doy>=91, (doy%%30), dd)
  dd <- ifelse(dd==0, 30, dd)
  # Find doy of origin
  start.y <- as.numeric(substr(origin, 1, 4))
  start.m <- as.numeric(substr(origin, 6, 7))
  start.d <- as.numeric(substr(origin, 9, 10))
  start.doy <- which(mm==start.m & dd==start.d)
  # Date that the sim is supposed to correspond to
  sim.day <- ceiling((tick + 1)/((24 * 60)/timestep))
  min.vector.lgt <- max(sim.day)+start.doy
  mm2 <- rep(mm, ceiling(min.vector.lgt/360))
  dd2 <- rep(dd, ceiling(min.vector.lgt/360))
  sim.month <- mm2[sim.day + start.doy - 1]
  sim.day.of.month <- dd2[sim.day + start.doy - 1]
  sim.year <- start.y+trunc((sim.day  + start.doy - 2)/360)
  sim.date <- paste(sim.year, substr(sim.month+100, 2, 3), substr(sim.day.of.month+100, 2, 3), sep="-")
  sim.date <- as.POSIXlt(sim.date, tz=tz)
  start.hms <- as.POSIXlt(origin)$hour * 3600 + as.POSIXlt(origin)$min * 60 + as.POSIXlt(origin)$sec
  sim.second.of.day <- (tick*timestep*60) %% (24*60*60)
  out <- sim.date+sim.second.of.day
  return(out)
}





#'Convert date to tick number
#'
#'@description
#'Convert a date to the number of ticks since simulation start while taking
#'into account that DEPONS assumes that there are 360 days in a simulation year.
#'
#'@usage
#'time.to.tick(time, timestep = 30, origin = "2010-01-01", tz = "UTC", ...)
#'
#'@details
#'Times are rounded down to the current 30-minute interval during conversion.
#'The function assumes that there are 30 days in each month, except in January,
#'February and March with 31, 28 and 31 days, respectively. Provided dates that
#'fall on days that are not accommodated (February 29, and the 31st day of the
#'months May, July, August, October, and December) are returned as NA.
#'
#'The function may be used to, e.g., convert recorded piling dates to ticks for
#'use in wind farm scenarios (see \code{\link{make.windfarms}} for construction
#'of hypothetical scenarios from parametric inputs).
#'
#'@param time Character, or character vector, of the form 'YYYY-MM-DD'
#'(or 'YYYY-MM-DD HH:MM:SS'), or equivalent POSIX object. Date(s) to be converted
#'to ticks.
#'@param timestep Numeric (default 30). Length of each simulation time step in
#'minutes.
#'@param origin Character of the form 'YYYY-MM-DD' or equivalent POSIX object
#'(default "2010-01-01"). Start date of simulation.
#'@param tz Character. Valid time zone code (default UTC).
#'@param ... Optional parameters.
#'
#'@returns
#'Numeric vector of tick numbers.
#'
#'@examples
#'\dontrun{
#'#Uses date column of AIS data.
#'#Times are in 30-minute intervals, and converting back yields the same times
#'data(aisdata)
#'ticks <- time.to.tick(aisdata$time, origin = "2015-12-20")
#'times_reconverted <- tick.to.time(ticks, origin = "2015-12-20")
#'
#'#Uses dates at other intervals.
#'#Converting back yields times rounded down to the current 30-minute interval
#'times <- c("2016-12-20 00:10:00",
#'           "2016-12-20 02:45:30",
#'           "2016-12-20 05:01:05",
#'           "2016-12-22 01:30:00")
#'ticks <- time.to.tick(times, origin = "2015-12-20")
#'times_reconverted <- tick.to.time(ticks, origin = "2015-12-20")}
#'
#'@seealso \code{\link{tick.to.time}} is the inverse of this function, converting
#'ticks to dates
#' @export time.to.tick
time.to.tick <- function (time, timestep = 30, origin = "2010-01-01", tz = "UTC", ...) {

  old <- options()
  on.exit(options(old))
  origin <- try(as.POSIXlt(origin, tz = "UTC"))
  if (!("POSIXlt" %in% class(origin))) {
    stop("'origin' must be of the form 'YYYY-MM-DD' for conversion to POSIX")
  }
  if (!is.numeric(timestep))
    stop("'timestep' should be numeric")

  origin <- as.POSIXlt(origin, tz = tz)
  monlng <- as.list(c(31,28,31, rep(30, times=9))) # list of expected month lengths
  names(monlng) <- seq(1:12)-1

  # function to step through months of appropriate length and add up ticks
  count.ticks.from.origin <- function(origin, target) {
    if (origin > target) stop ("origin is later than time")
    cur.or <- origin
    cur.count <- 0
    if (cur.or$year < target$year || cur.or$mon < target$mon) { # at start, if not in target month & year yet, bottom up to next month to enable simple month-length additions in the following
      new.or <- cur.or
      new.or$mon <- new.or$mon + 1
      new.or$mday <- 1
      new.or$hour <- 0
      new.or$min <- 0
      new.or$sec <- 0
      difference <- as.numeric(difftime(new.or, cur.or, units = "secs"))
      cur.diff <- (monlng[[as.character(cur.or$mon)]] - (cur.or$mday - 1)) * (24 * 60 * 60) # account for skipped time, observing expected duration of MODEL month
      cur.count <- cur.count + cur.diff
      cur.or <- new.or
    }
    breaks <- 0
    while (breaks == 0) { # then proceed through remaining months
      # reset format to roll over added months
      cur.or <- as.POSIXlt(as.POSIXct(cur.or, tz = tz), tz = tz)
      if (cur.or > target) stop (paste0("tick accumulation overshot target date. Failed at ", time[instance]))
      cur.diff <- as.numeric(difftime(target, cur.or, units = "secs"))
      if (cur.or$year == target$year && cur.or$mon == target$mon) { # if reached same month and same year, add remainder secs to count
        cur.count <- cur.count + cur.diff
        breaks <- 1 # and exit
      } else {
        # else add one months' (of correct length) worth of seconds to moving origin and count
        # but MODEL month's worth of seconds to count
        cur.count <- cur.count + monlng[[as.character(cur.or$mon)]] * 24 * 60 * 60
        cur.or$mon <- cur.or$mon + 1
      } # and repeat
    }
    ticks <- floor(cur.count / (60 * timestep))
    return (ticks)
  }

  time <- as.character(time)
  time.converted <- time
  for (instance in 1:length(time)) {
    test <- try(as.POSIXlt(time[instance], tz = tz))
    if (!("POSIXlt" %in% class(test))) {
      stop(paste0("'time' values must be of the form 'YYYY-MM-DD' (optionally 'YYYY-MM-DD HH:MM:SS') for conversion to POSIX. Failed at ", time[instance]))
    }
    target <- as.POSIXlt(time[instance], tz = tz)
    if (target$mday > monlng[[target$mon + 1]]) {
      # if a date (start or end) is on a day beyond expected month length, set
      # instance to NA and continue to next instance
      time.converted[instance] <- NA
      message(paste0("time set to NA due to date outside accommodated month length: ", time[instance]))
      next
    } else {
      time.converted[instance] <- count.ticks.from.origin (origin, target) # else process ticks for date
    }
  }
  return(as.numeric(time.converted))
}






setGeneric("make.clip.poly", function(bbox, ...){})

#' @name make.clip.poly
#' @title Make clipping polygon from bbox
#' @description Makes a polygon from a bounding box to use for
#' clipping the coastline, or other SpatialPolygons objects
#' @aliases make.clip.poly
#' @aliases make.clip.poly,matrix-method
#' @param bbox 2x2 matrix
#' @param crs CRS object defining the projection of the SpatialPolygons object
#' to be clipped
#' @seealso \code{\link{bbox}} for creation of bbox matrix from DeponsRaster
#' @exportMethod make.clip.poly
#' @return \code{SpatialPolygons} object
#' @examples
#' data(bathymetry)
#' bbox <- cbind("min"=c(549517, 6155000), "max"=c(636000, 6210000))
#' rownames(bbox) <- c("x", "y")
#' clip.poly <- make.clip.poly(bbox, crs(bathymetry))
setMethod("make.clip.poly", signature("matrix"),
          function(bbox, crs) {
            if(!inherits(crs,"CRS")) stop("crs must be a 'CRS' object")
            if(dim(bbox)[1]!=2 || dim(bbox)[2]!=2) stop("bbox must be a 2x2 matrix")
            if(bbox["x", "min"] >= bbox["x", "max"]) stop("Ensure that bbox[1,1] < bbox[1,2]")
            if(bbox["y", "min"] >= bbox["y", "max"]) stop("Ensure that bbox[2,1] < bbox[2,2]")
            bbox.x <- c(bbox["x", "min"], bbox["x", "max"], bbox["x", "max"], bbox["x", "min"],
                        bbox["x", "min"])
            bbox.y <- c(bbox["y", "min"], bbox["y", "min"], bbox["y", "max"], bbox["y", "max"],
                        bbox["y", "min"])
            sp <- sp::Polygon(cbind(bbox.x, bbox.y))
            sps <- sp::Polygons(list(sp), "ID")
            clip.poly <- sp::SpatialPolygons(list(sps), proj4string = crs)
            return(clip.poly)
          }
)



#' @name find.nearest.piling
#' @title Find nearest active piling from simulated CPOD blocks
#' @description For each row of simulated porpoise data, this function identifies whether any
#' piling event was active at that time and, if so, assigns the nearest active
#' piling based on Euclidean distance between the simulated location and the piling
#' location. If more than one piling is active for a given row, the function keeps the
#' closest active piling only. It adds the ID of the nearest active piling, the distance to that
#' piling, and the piling start time to the input simulation table.
#' This function is intended for simulated PorpoisePerBlock data that have already
#' been reformatted to match the structure of CPOD-derived data used in the
#' Gemini piling-response workflow.
#' @param cpod.coord A data frame containing CPOD station coordinates, including
#'   station ID and projected `x`/`y` coordinates.
#' @param sim.data A dataframe of PorpoisePerBlock outputs transformed into columns "station",
#' "time", "pod", "dpm", "nall", "good.data".
#' @param piling.data A data frame containing piling event information, including
#'   piling ID, start and stop times, and projected `x`/`y` coordinates.
#' @param tz Character string giving the time zone used for piling start and stop
#'   times. Default is `"Europe/Amsterdam"`.
#' @return A data frame equal to `sim.data` joined with station coordinates and with
#'   three additional columns: `nearest.piling`, `np.dist`, and `np.starttime`.
#' @export find.nearest.piling
find.nearest.piling <- function(cpod.coord, sim.data, piling.data, tz="Eupore/Amsterdam") {
cpod.coord$station <- as.numeric(sub(".*?(\\d+)$", "\\1", as.character(cpod.coord$station)))
sim.data2 <- merge(sim.data, cpod.coord)
sim.data2$nearest.piling <- "NA"
sim.data2$np.dist <- NA
sim.data2$np.starttime <- NA
message(paste("Number of rows:", dim(sim.data2)[1]))
# piling data in model was in UTC so need to convert to Europe Ams to get same as sim output data
attributes(piling.data$start.datetime)$tzone <- tz
attributes(piling.data$stop.datetime)$tzone <- tz
for(i in 1:dim(sim.data2)[1]) {
  if(i %% 10000 == 0) message(paste("Row", i))
  active.piling.lines <- which((piling.data$start.datetime <= sim.data2$time[i]) &
                                 (sim.data2$time[i] <= piling.data$stop.datetime))
  if(length(active.piling.lines)>0) {
    sim.data2$nearest.piling[i] <- as.character(piling.data$id[active.piling.lines[1]])
    dd <- matrix(c(sim.data2$x[i], sim.data2$y[i], piling.data$x[active.piling.lines[1]],
                   piling.data$y[active.piling.lines[1]]), nrow=2, byrow=TRUE)
    sim.data2$np.dist[i] <- stats::dist(dd)
    sim.data2$np.starttime[i] <- as.character(piling.data$start.datetime[active.piling.lines[1]])
    if(length(active.piling.lines)>1) { # i.e. if there's more than one active piling for this pod line
      for (j in 2:length(active.piling.lines)) {
        dd2 <- matrix(c(sim.data2$x[i], sim.data2$y[i], piling.data$x[active.piling.lines[j]],
                        piling.data$y[active.piling.lines[j]]), nrow=2, byrow=TRUE)
        if (stats::dist(dd2) < stats::dist(dd)) {
          sim.data2$nearest.piling[i] <- as.character(piling.data$id[active.piling.lines[j]])
          sim.data2$np.dist[i] <- stats::dist(dd2)
          sim.data2$np.starttime[i] <- as.character(piling.data$start.datetime[active.piling.lines[j]])
        }
      }
    }
  }
}
return(sim.data2)
}

#' @name get.pods.at.dist.sim.dat
#' @title Assign hours since piling for detections within a distance interval
#' @description Identifies simulated porpoise detections associated with piling events occurring
#' within a specified distance interval from block location.
#'
#' For each pod-piling combination, the function labels rows belonging to that
#' piling event and computes `hrs.since.piling`, including a short window before
#' piling and a recovery window after piling has stopped.
#'
#' This function is used to prepare simulation output for aggregation of porpoise
#' detections before, during, and after piling within given distance bands.
#' @param min.dist Numeric. Lower distance threshold in meters.
#' @param max.dist Numeric. Upper distance threshold in meters.
#' @param sim.data2 A data frame containing simulated porpoise data already
#' annotated with nearest piling information, typically the output of
#' [find.nearest.piling()].
#' @param tz Time zone.
#' @return A data frame with two added columns:
#'   \describe{
#'     \item{hrs.since.piling}{Integer hours relative to the end of piling,
#'     including a short pre-piling and post-piling window.}
#'     \item{np}{The piling ID associated with the row, or `"NA"` if the row is
#'     not assigned to a piling event in the selected distance class.}
#'   }
#' @export get.pods.at.dist.sim.dat
get.pods.at.dist.sim.dat <- function(min.dist, max.dist, sim.data2, tz) {
  sim.data3<-sim.data2
  # Find pods that are at some point min.dist-max.dist m from a piling
  the.lines <- which(sim.data3$np.dist>min.dist & sim.data3$np.dist<max.dist)
  the.pods <- sort(unique(sim.data3$pod[the.lines]))
  the.pilings <- sort(unique(as.character(sim.data3$nearest.piling[the.lines])))
  sim.data3$hrs.since.piling <- NA
  sim.data3$np.starttime <- as.POSIXct(sim.data3$np.starttime, format= "%Y-%m-%d %H:%M:%S", tz)
  sim.data3$np <- "NA"
  # For every combination of pod and piling, fill in hours since start of piling (also for
  # the 5 hours before piling started).
  # NOTE that consecutive pilings are sometimes < 6 hrs apart
  for (po in the.pods) {
    message(paste("Distance: ", min.dist, "-", max.dist, ", CPOD: ", po, sep=""))
    for (pil in the.pilings) {
      # message(pil)
      lns <- which(sim.data3$pod==po & sim.data3$nearest.piling==pil)
      if(length(lns)==0) {
        warning(paste("No data for comb. of pod", po, "and piling", pil))
        next
      }
      if(sim.data3$np.dist[lns][1]>max.dist || sim.data3$np.dist[lns][1]<min.dist) next
      sim.data3$np[lns] <- pil
      sim.data3$hrs.since.piling[lns] <- floor(as.numeric(difftime(sim.data3$time[lns], sim.data3$time[max(lns)+1], units="hours")))
      last.pile.time <- sim.data3$time[max(lns)+1]
      # Jump to next piling if followed directly by another piling:
      if(!is.na(sim.data3$np.dist[max(lns)+1])) {
        if(sim.data3$np.dist[max(lns)+1]<max.dist) next
      }
      # Move forward in time <25 hrs to look at recovery
      for (lno in (max(lns)+1):length(sim.data3[,1])) {
        if(sim.data3$pod[lno] != po) break
        if(!is.na(sim.data3$np.dist[lno])) {
          if (sim.data3$np.dist[lno]<max.dist) break # stop if there's another piling
        }
        ttt <- floor(as.numeric(difftime(sim.data3$time[lno], last.pile.time, units="hours")))
        if(ttt>=13) break
        if(!is.na(sim.data3$hrs.since.piling[lno])) {
          warning(paste("Piling ", pil, " is <25 hrs from the following (line=", lno, ")", sep=""))
          break
        }
        sim.data3$hrs.since.piling[lno] <- ttt
        sim.data3$np[lno] <- pil
      }
    }
    for (pil in the.pilings) {
      # Move back in time <6 hrs
      lns <- which(sim.data3$pod==po & sim.data3$nearest.piling==pil)
      if(length(lns)==0) next
      if(sim.data3$np.dist[lns][1]>max.dist || sim.data3$np.dist[lns][1]<min.dist) next
      last.pile.time <- sim.data3$time[max(lns)+1]
      for (lno in (min(lns)-1):1) {
        if(sim.data3$pod[lno] != po) break
        ttt <- floor(as.numeric(difftime(sim.data3$time[lno], last.pile.time, units="hours")))
        if(ttt<=-4) break
        if(!is.na(sim.data3$hrs.since.piling[lno]) && sim.data3$hrs.since.piling[lno]<=12) break
        sim.data3$hrs.since.piling[lno] <- ttt
        sim.data3$np[lno] <- pil
      }
    }
  }
  return(sim.data3)
}

#' @name aggregate.dpm.sim.dat
#' @title Aggregate simulated porpoise detections by piling event and hour
#' @description Aggregates simulated porpoise detections per minute (`dpm`) to
#' hourly values for each station, pod, piling event, and hour since piling.
#'
#' The function is applied after simulation outputs have been matched to piling
#' events and assigned a value of `hrs.since.piling`. For each combination of
#' station, pod, piling event, and hour since piling, it computes the total
#' number of porpoise, the average monitoring effort (dummy value), the fraction
#' of minutes during which piling was active, minutes of monitoring per hour (time step is
#' of 30min in DEPONS, so it is 2min/h), and whether all contributing rows were
#' flagged as good data (as in the real CPOD data).
#'
#' This is used to create the hourly input tables required to compare simulated
#' piling responses with observed CPOD data.
#' @param sim.data3 A data frame containing piling assignments and hour offsets,
#'   typically the output of [get.pods.at.dist.sim.dat()].
#'   @return A data frame with one row per station, pod, piling event, and
#'   `hrs.since.piling`, containing:
#'   \describe{
#'     \item{sum.dpm}{Sum of detections per minute across rows in the group.}
#'     \item{mean.nall}{Mean monitoring effort.}
#'     \item{fraction.piling}{Fraction of contributing rows during which piling
#'     was active.}
#'     \item{minutes.per.hr}{Number of minutes recorded in the hour.}
#'     \item{good.data}{Logical indicator for flagging as good data.}
#'   }
#' @export aggregate.dpm.sim.dat
aggregate.dpm.sim.dat <- function(sim.data3) {
  ttt <- sim.data3[sim.data3$np!="NA"  ,]
  ttt$is.piling <- ifelse(is.na(ttt$np.dist), 0, 1)
  sim.data4 <- aggregate(ttt$dpm, by=list(ttt$station, ttt$pod, ttt$np, ttt$hrs.since.piling), FUN="sum", drop=FALSE)
  names(sim.data4) <- c("station", "pod", "np", "hrs.since.piling", "sum.dpm")
  mean.nall <- aggregate(ttt$nall, by=list(ttt$station, ttt$pod, ttt$np, ttt$hrs.since.piling),
                         FUN="mean", drop=FALSE)
  fraction.piling <- aggregate(ttt$is.piling, by=list(ttt$station, ttt$pod, ttt$np, ttt$hrs.since.piling),
                               FUN="mean", drop=FALSE)
  minutes.per.hr <- aggregate(ttt$nall, by=list(ttt$station, ttt$pod, ttt$np, ttt$hrs.since.piling),
                              FUN="length", drop=FALSE)
  minutes.per.hr <- ifelse(is.na(mean.nall[,5]), NA, minutes.per.hr[,5])
  good.data <- aggregate(ttt$good.data, by=list(ttt$station, ttt$pod, ttt$np, ttt$hrs.since.piling),
                         FUN="all", drop=FALSE)
  sim.data4 <- cbind(sim.data4, "mean.nall"=mean.nall[,5], "fraction.piling"=fraction.piling[,5],
                      "minutes.per.hr"= minutes.per.hr,"good.data"=good.data[,5])
  ii <- order(sim.data4$station, sim.data4$pod, sim.data4$np, sim.data4$hrs.since.piling)
  sim.data4 <- sim.data4[ii,]
  sim.data4$sum.dpm <- ifelse(is.na(sim.data4$mean.nall), NA, sim.data4$sum.dpm)
  return(sim.data4)
}

#' @name summarise.aggregated.dpm.dat
#' @title Summarises aggregated simulated detections before and after piling
#' @description Summarises an aggregated DPM table to produce the mean porpoise
#' detection rate per hour since piling across all piling events and pods within
#' a distance class.
#'
#' The function takes the event-level output from [aggregate.dpm.sim.dat()] and
#' collapses it across piling events and pods, returning a single response curve
#' by `hrs.since.piling`. For each hour bin, it calculates the mean detection
#' rate and its standard error.
#'
#' This is used to generate the hourly response curves that are compared between
#' simulated and observed data during piling-response calibration.
#' @param ttt A data frame of aggregated simulated detections, typically produced
#'   by [aggregate.dpm.sim.dat()].
#' @param label Character string used as the distance-class label in the plot.
#' @export summarise.aggregated.dpm.dat
summarise.aggregated.dpm.dat <- function(ttt, label) {
  # Plot averages per hr across all pilings and pods at selected distance
  length.na.rm <- function(x) length(x[!is.na(x)])
  mean.na.rm <- function(x) mean(x, na.rm=TRUE)
  sd.na.rm <- function(x) stats::sd(x, na.rm=TRUE)
  mean.dpm <- tapply(ttt$sum.dpm, ttt$hrs.since.piling, FUN="mean.na.rm")
  names(ttt)[which(names(ttt)=="minutes.w.dpm.data")] <- "minutes.per.hr"
  mean.minutes.per.hr <- tapply(ttt$minutes.per.hr, ttt$hrs.since.piling, FUN="mean.na.rm")
  mean.dpm <- mean.dpm * 60 / mean.minutes.per.hr
  sd.dpm <- tapply(ttt$sum.dpm, ttt$hrs.since.piling, FUN="sd.na.rm")
  mean.dpm <- ifelse(is.na(sd.dpm), NA, mean.dpm)
  n.dpm <- tapply(ttt$sum.dpm, ttt$hrs.since.piling, FUN="length.na.rm")
  se.dpm <- sd.dpm/sqrt(n.dpm)
  hrs.since.p <- as.numeric(names(mean.dpm))
  # There's nearly always data for all of hr -3, so piling took place during all of that hour.
  # Seal scare was on in the 30 min before piling, i.e. usually in hr. -4 or -5
  return(data.frame("hrs.since.piling"=hrs.since.p, "mean.dpm"=100*mean.dpm/60, "se.dpm"=100*se.dpm/60))
}
