# DEPONS2R helper functions (related to multiple classes)

# roxygen2::update_collate('.')
# devtools::check()    # r cmd check
# roxygen2::roxygenize()  # for creating namespace

# devtools::document()  # Make rd files based on roxygen comments.

#' @title Get simulation date
#' @name get.simdate
#' @description Returns the date and time when a specific simulation was finished
#' (a \code{\link{POSIXlt}} object)
#' @param fname Character string with name of the file to extract the simulation
#' date from, including the path
#' @export get.simdate
get.simdate <- function(fname=NULL) {
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


#' @title Get output
#' @name get.latest.sim
#' @description Returns the name of the newest simulation output within the
#' specified directory
#' @param type Type of simulation output to check; can be one of:
#' "dyn" (for looking in "Statistics.XX.csv" files)
#' "blockdyn" (for looking in "PorpoisePerBlock.XX.csv" files)
#' "track" (for looking in "RandomPorpoise.XX.csv" files)
#' @param dir Directory to look for simulation output in (character string)
#' @export get.latest.sim
get.latest.sim <- function(type="dyn", dir) {
  if (!(type %in% c("dyn", "blockdyn", "track"))) {
    stop ("type must be either 'dyn', 'blockdyn', or 'track'")
  }
  the.dir <- dir
  outfiles <- grep(type, dir(the.dir))
  if(length(outfiles)==0) stop("No sim output of type 'Statistics' found in dir")
  fnms <- dir(the.dir)[outfiles]
  fnms2 <- as.list(fnms)
  ftime <- lapply(fnms2, FUN="get.simdate")
  for(i in 1:length(ftime)) ftime[[i]] <- as.character(ftime[[i]])
  outfile.nos <- data.frame("file.no"=outfiles, "time.str"=unlist(ftime))
  newest.sim <- sort(outfile.nos$time.str, decreasing=TRUE, index.return=TRUE)
  no.of.newest.sim <- outfile.nos$file.no[newest.sim$ix][1]
  latest.sim <- dir(the.dir)[no.of.newest.sim]
  return(latest.sim)
}


# Notes regarding development of the DEPONS2R package

# Enable manipulation and plotting of ship tracks
# Reproduce population effects plots from paper


# Collate controls the order in which R files are sourced. This only matters if
# your code has side-effects; most commonly because you’re using S4.




#####


# MAKE track FILE
# fname <- "/Applications/DEPONS 2.1/DEPONS/RandomPorpoise.2020.Jul.31.09_43_10.csv"
# fname <- "/Applications/DEPONS 2.1/DEPONS/RandomPorpoise.2020.Aug.19.11_28_36.csv"
# file.exists(fname)
# porpoisetrack <- read.DeponsTrack(fname, title="Porpoise track simulated with DEPONS 2.1", landscape="Kattegat",
#                                 crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
# )
# porpoisetrack
# save(porpoisetrack, file="porpoisetrack.RData", compress="xz")



#####


# MAKE bathymetry FILE
# fname <- "/Applications/DEPONS 2.1/DEPONS/data/Kattegat/bathy.asc"
# file.exists(fname)
# bathymetry <- read.DeponsRaster(fname,
#                                 crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs",
#                                 type="bathymetry", landscape="Kattegat"
# )
# bathymetry
# save(bathymetry, file="bathymetry.RData", compress="xz")


# MAKE bathymetry FILE

# fname <- "/Applications/DEPONS 2.1/DEPONS/data/NorthSea/bathy.asc"
# bathymetry <- read.DeponsRaster(fname,
#                                 crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs",
#                                 type="bathymetry", landscape="North Sea"
# )
# save(bathymetry, file="bathymetry.RData", compress="xz")


#######


# MAKE porpoisedyn FILE

# porpoisedyn <- read.csv("Statistics.2020.Aug.27.10_55_36.csv", sep=";")
# fname <- "Statistics.2020.Aug.27.10_55_36.csv"
# porpoisedyn <- read.DeponsDyn(fname, simstart="2010-01-01")
# save(porpoisedyn, file="porpoisedyn.RData")


####

# MAKE North Sea blockraster

# the.crs <- crs(coastline)
# templ <- read.DeponsRaster("blocks.asc", type="blocks", crs=the.crs, landscape="North Sea")
# plot(templ)
# x <- c(3700000, 4000000)
# y <- c(3400000, 3700000)
# points(x,y)
# new.blocks <- list()
# new.blocks[[1]] <- cbind(x,y)
# make.blocksraster(template = templ, new.blocks, plot=TRUE)
# plot(coastline, add=TRUE)
# make.blocksraster(template = templ, new.blocks, fname="NS2blocks.asc")

# dir()
# blockdyn <- read.csv("PorpoisePerBlock.2020.Sep.02.20_24_17.csv", sep=",")
# head(blockdyn)
# dim(blockdyn)
