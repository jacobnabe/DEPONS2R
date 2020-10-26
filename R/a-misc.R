# DEPONS2R helper functions (related to multiple classes)

# devtools::document()  # Make rd files based on roxygen comments.
# devtools::build_manual()
# devtools::check(cleanup=FALSE) # check timings of examples: read log

#' @title  Package for analyzing DEPONS simulation output
#' @name DEPONS2R
#' @description Classes and methods for analyzing and plotting movement tracks
#' and population dynamics simulated using the DEPONS model (\url{http://www.depons.eu}).
#'
#' The types of simulated data include:
#' \itemize{
#' \item \code{\linkS4class{DeponsTrack}} movement tracks, read from "RandomPorpoise.XXX.csv" files
#' \item \code{\linkS4class{DeponsDyn}} population dynamics data, from "Statistics.XXX.csv" files
#' \item \code{\linkS4class{DeponsBlockdyn}} data from "PorpoisePerBlock.XXX.csv" files
#' }
#' Here the \code{DeponsDyn} data include both changes in population size and energetics
#' through time for the entire landscape, whereas \code{DeponsBlockdyn} data include
#' variations in population size in different parts (or 'blocks') of the landscape.
#' XXX is the date and time when the simulation was finished.
NULL


#' @title Get simulation date
#' @name get.simtime
#' @description Returns the date and time when a specific simulation was finished
#' (a \code{\link{POSIXlt}} object)
#' @param fname Character string with name of the file to extract the simulation
#' date from, including the path
#' @seealso \code{\link{get.latest.sim}}
#' @export get.simtime
get.simtime <- function(fname=NULL) {
  ncf <- nchar(fname)
  tmp <- substr(fname, ncf-23, ncf-4)
  tmp <- gsub("_", ":", tmp)
  tmp <- gsub("Jan", "01", tmp)
  tmp <- gsub("Feb", "02", tmp)
  tmp <- gsub("Mar", "03", tmp)
  tmp <- gsub("Apr", "04", tmp)
  tmp <- gsub("May", "05", tmp)
  tmp <- gsub("Jun", "06", tmp)
  tmp <- gsub("Jul", "07", tmp)
  tmp <- gsub("Aug", "08", tmp)
  tmp <- gsub("Sep", "09", tmp)
  tmp <- gsub("Oct", "10", tmp)
  tmp <- gsub("Nov", "11", tmp)
  tmp <- gsub("11", "12", tmp)
  substr(tmp, 5, 5) <- "-"
  substr(tmp, 8, 8) <- "-"
  substr(tmp, 11, 11) <- " "
  tmp <- (as.POSIXlt(tmp))
  if (any(class(tmp)=="POSIXlt")) {
    date <- tmp
  } else {
    date <- (as.POSIXlt(date))
  }
  return(date)
}


#' @title Get name of newest file
#' @name get.latest.sim
#' @description Returns the name of the newest simulation output of a particular
#' type within the specified directory. The date and time are extracted from the
#' file name.
#' @param type Type of simulation output to check; can be one of:
#' "dyn" (for looking in "Statistics.XX.csv" files),
#' "blockdyn" (for looking in "PorpoisePerBlock.XX.csv" files)
#' "track" (for looking in "RandomPorpoise.XX.csv" files)
#' @param dir Directory to look for simulation output in (character string)
#' @seealso \code{\link{read.DeponsBlockdyn}} for example.
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
  for(i in 1:length(ftime)) ftime[[i]] <- as.character(ftime[[i]])
  outfile.nos <- data.frame("file.no"=outfiles, "time.str"=unlist(ftime))
  newest.sim <- sort(outfile.nos$time.str, decreasing=TRUE, index.return=TRUE)
  no.of.newest.sim <- outfile.nos$file.no[newest.sim$ix][1]
  latest.sim <- dir(the.dir)[no.of.newest.sim]
  return(latest.sim)
}


#' @title Make wind farm scenario
#' @name make.windfarms
#' @description Produce a hypothetical wind farm construction scenario, specifying
#' the position and timing of individual piling events, as well as the sound
#' source level. All wind farms are assumed to consist of the same number of
#' turbines, laid out in a rectangular grid. The start and end tick (i.e. the
#' number of half-hour intervals since simulation start) is generated based on
#' provided values for the time it required for each piling and the time between
#' piling events.
#' @param area.file Name of the raster file specifying where the wind farms
#' should be constructed.
#' @param area.def Value in \code{area.file} for the areas were wind farms can
#' be located
#' @param n.wf Number of wind farms to construct
#' @param n.turb Total number of turbines to construct
#' @param turb.dist Distance between turbines within a wind farm (meters)
#' @param min.wf.dist Minimum distance between wind farms (meters)
#' @param impact Sound source level (dB); sound emitted from turbines during
#' construction, i.e. from tick.start to tick.end (including both start and end)
#' @param constr.start The tick at which construction of the first turbine starts.
#' @param constr.end The tick at which construction of the very last turbine in
#' the last wind farm ends.
#' @param constr.time The time it takes to construct a single wind turbine
#' (number of ticks).
#' @param constr.break Break between individual pilings within a wind farm,
#' counted in number of half-hour 'ticks'.
#' @note The parameters \code{constr.start}, \code{constr.end}, \code{constr.time},
#' and \code{constr.break} are truncated to nearest integer value. Construction
#' of wind farms starts in WF001 at tick \code{constr.start}. Each turbine
#' foundation is piled over a period of \code{constr.time}, followed by a
#' noise-free period of \code{constr.break}. Several pile driving operations may
#' take place at the same time, to ensure that the last piling ends before \code{constr.end}.
#' @export make.windfarms
make.windfarms <- function(area.file, area.def, n.wf, n.turb, turb.dist,
                           min.wf.dist, impact, constr.start, constr.end,
                           constr.time, constr.break) {
  # Check parameters
  ncf <- nchar(area.file)
  af.ext <- substr(area.file, ncf-2, ncf)
  if(af.ext != "tif" && af.ext != "asc") stop("'area.file' does not appear to be a raster")
  # Get extent of wind farm zone
  wf.area <- raster::raster(area.file)
  wf.area[wf.area[,]!=area.def] <- NA
  wf.area <- raster::trim(wf.area)
  wf.ext <- raster::extent(wf.area)
  # Select coords for bottom-left corner of wind farm
  get.start.pos <- function(the.wf.ext=wf.ext, the.wf.area=wf.area) {
    i <- 1000
    while(i>0) {
      x.pos <- stats::runif(1, min=the.wf.ext[1], max=the.wf.ext[2])
      y.pos <- stats::runif(1, min=the.wf.ext[3], max=the.wf.ext[4])
      the.pos <- data.frame("x"=x.pos, "y"=y.pos)
      xy.val <- raster::extract(the.wf.area, the.pos)
      if(!is.na(xy.val)) return(the.pos)
      i <- i-1
    }
  }
  get.turb.pos <- function(n.turb=n.turb, n.wf=n.wf, start.pos) {
    n.turb.per.wf <- ceiling(n.turb / n.wf)
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
  make.one.wf <- function(wf.no) {
    cont.trying <- TRUE
    j <- 0  # safety break
    while(cont.trying) {
      start.pos <- get.start.pos()
      turb.pos <- get.turb.pos(n.turb, n.wf, start.pos)
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
                                 (all.wfs$y-xy.centre$y)^2) + dist.centre.to.corner
      if(min(dist.to.existing) < min.wf.dist) cont.trying <- TRUE
      j <- j+1
      if(j==10000) stop(paste("Failed to generate wind farm", wf.no))
    }
    t <- row(turb.pos)[,1]
    turb.no <- substr(as.character(t + 1000), 2, 5)
    wf.n <- wf.no
    wf.no <- substr(as.character(wf.no + 1000), 2, 5)
    turb.names <- paste0("WF", wf.no, "_", turb.no)
    turb.pos <- cbind(wf.n, t, "id"=turb.names, turb.pos)
    names(turb.pos) <- c("wf", "t", "id", "x.coordinate", "y.coordinate")
    return(turb.pos)
  }
  ## Run the code above
  all.wfs <- data.frame()
  for (i in 1:n.wf) {
    one.wf <- make.one.wf(wf.no=i)
    all.wfs <- rbind(all.wfs, one.wf)
  }
  all.wfs <- all.wfs[1:n.turb ,] # make sure total n turbs isn't too big
  # Add sound source level (=impact) to the wf data frame
  all.wfs <- cbind(all.wfs, impact)
  # Add piling schedule
  constr.start <- floor(constr.start)
  constr.end <- floor(constr.end)
  constr.time <- floor(constr.time)
  constr.break <- floor(constr.break)
  all.wfs$tick.start <- NA
  all.wfs$tick.end <- NA
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
      all.wfs[sel.row,]$tick.start <- current.tick
      all.wfs[sel.row,]$tick.end <- current.tick+constr.time
      current.tick <- current.tick + constr.time + constr.break
    }
  }
  return(all.wfs[, 3:8])
}


## Test 'make.windfarms'
# area.file <- "wfAreas.tif"
# area.def <- 255
# n.wf <- 10
# n.turb <- 99
# turb.dist <- 500
# min.wf.dist <- 10000
# impact <-  300
# constr.start <- 361*48
# constr.end <- constr.start + 5*360*48  # stop 5 yrs after start
# constr.time <- 4
# constr.break <- 48

# ?make.windfarms
# wfs <- make.windfarms(area.file="wfAreas.tif", area.def=255, n.wf=14,
#   n.turb=420, turb.dist=300, min.wf.dist=10000, impact=250,
#   constr.start=10*360*48+1, constr.end=15*360*48, constr.time=4, constr.break=48)
# dim(wfs)


#' @title Convert tick number to date
#' @name tick.to.time
#' @description Converts the number of ticks since the start of the simulation
#' to a specific date while taking into account that DEPONS assumes that there
#' is 30 days per month. Returns an object of class \code{\link{as.POSIXlt}}
#' @note The conversion from ticks to date is problematic in February, with <30
#' days. Here time is forced to 'stands still'.
#' @param tick Numeric; tick number
#' @param ... Time zone (tz) and other parameters
#' @export tick.to.time
tick.to.time <- function(tick, ...) {
  old <- options()
  on.exit(options(old))  # Reset user options on exit
  minute <- tick*30
  hour <- floor(minute/60)
  day <- floor(hour/24)
  month <- floor(day/30)
  year <- floor(month/12)
  options(scipen=999) # Avoid sci notation
  mi <- substr(as.character((minute %% 60)+1000000), 6, 7)
  hh <- substr(as.character((hour %% 24)+1000000), 6, 7)
  dd <- substr(as.character((day %% 30)+1000000+1), 6, 7)
  mm <- substr(as.character((month %% 12)+1000000+1), 6, 7)
  yy <- substr(as.character(year+1000000), 4, 7)
  str <- paste0(yy, "-", mm, "-", dd, " ", hh, ":", mi, ":00")
  # Handle February
  if((mm=="02") && ((day %% 30 +1) >28)) {
    str <- paste0(yy, "-", mm, "-28 23:59:00")
  }
    if(!hasArg(tz)) {
      tz <- ""
    } else {
      tz <- as.character(list(...)[["tz"]])
      tz <- tz
    }
  out <- try(as.POSIXlt(str, tz=tz), silent=TRUE)
  if(any(class(out)=="try-error")) {
    stop(paste(" Error in conversion of tick =", tick))
  }
  return(out)
}


for (i in 1:100000) toto <- tick.to.time(i)
