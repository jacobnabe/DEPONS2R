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
