
# Author: Jacob Nabe-Nielsen
# Date: 27 August 2020
# Licence GPL v3
# Description: Methods and classes for reading and summarizing DEPONS raster
#   objects


#' @title  DeponsDyn-class
#' @description Stores objects containing population dynamics output and energetic
#' output simulated using the DEPONS model.
#' @slot title Character. Name of the object or simulation
#' @slot landscape Character. Identifier for the landscape used in the DEPONS
#' simulations. The landscapes 'DanTysk', 'Gemini', 'Kattegat', 'North Sea',
#' 'Homogeneous', and 'User defined' are distributed with the DEPONS model.
#' @slot simtime \code{\link{POSIXlt}} object with the date and time when the
#' simulation was finished. This is read from the name of the imput file.
#' @slot startday POSIXlt object with the first day of the simulation, i.e.
#' the first day in the period that the simulations are intended to represent in
#' the real world.
#' @slot dyn Data frame with simulation output.
#' @details The following columns are included in the simulation output data
#' frame: 'tick', which indicates the number of half-hourly time steps since the
#' start of the simulation; 'count', which indicates the population size at a
#' given time; 'anim.e', showing the average amount of energy stored by simulated
#' animals; 'lands.e', which shows the total amount of energy in the landscape,
#' and 'real.time' which shows the time relative to 'startday'.
#' @exportClass DeponsDyn
#' @examples a.DeponsDyn <- new("DeponsDyn")
#' a.DeponsDyn
#' @note DeponsDyn-objects are usually read in from csv files produced during
#' DEPONS simulations. These files are named 'Statistics.XXX.csv', where XXX
#' indicates the date and time when the simulation was finished.
#' @seealso \code{\link[DEPONS2R]{plot.DeponsDyn}} and
#' \code{\link[DEPONS2R]{read.DeponsDyn}}.
setClass(Class="DeponsDyn",
         slots=list(title="character", landscape="character", simtime="POSIXlt",
                    startday="POSIXlt", dyn="data.frame")
)


setMethod("initialize", "DeponsDyn",
          function(.Object) {
            .Object@title <- "NA"
            .Object@landscape <- "NA"
            .Object@simtime <- as.POSIXlt(NA)
            .Object@startday <- as.POSIXlt(NA)
            .Object@dyn <- data.frame("tick"=NA, "count"=NA, "anim.e"=NA, "lands.e"=NA,
                                       "real.time"=NA)
            return((.Object))
          }
)

#' @name summary
#' @title Summary
#' @rdname summary
#' @aliases summary,DeponsDyn-method
#' @return table summarizing the DeponsBlockdyn object
#' @exportMethod summary
setMethod("summary", "DeponsDyn",
          function(object) {
            cat("class:    \t", "DeponsDyn \n")
            cat("title:    \t", object@title, "\n")
            cat("landscape:\t", object@landscape, "\n")
            cat("simtime:  \t", as.character(object@simtime), "\n\n")
            summary(object@dyn)
          }
)


#' @title Reading DEPONS simulation output
#' @description Function  for reading simulation output produced by DEPONS.
#'
#' @param fname Name of the file (character) that contains number of
#' animals for each time step during the simulation, along with their energy and
#' the amount of food in the landscape. The name includes the path to the directory
#' if this is not the current working directory.
#' @param title Optional character string giving name of simulation
#' @param landscape The landscape used in the simulation
#' @param simtime Optional character string with the date and time when the
#' simulation finished (format yyyy-mm-dd). If not provided this is obtained
#' from name of input file
#' @param startday The start of the period that the  simulation represents, i.e.
#' the real-world equivalent of 'tick 1' (character string of the
#' form 'yyyy-mm-dd', or POSIXlt)
#' @param timestep Time step used in the model, in minutes. Defaults to 30 in
#' DEPONS.
#' @param tz Time zone.
#' @seealso See \code{\link{DeponsDyn-class}} for details on what is stored in
#' the output object.
#' @return \code{DeponsDyn} object containing simulation output
#' @examples \dontrun{
#' dyn.file <- "/Applications/DEPONS 2.1/DEPONS/Statistics.2020.Sep.02.20_24_17.csv"
#' file.exists(dyn.file)
#' porpoisedyn <- read.DeponsDyn(dyn.file, startday=as.POSIXlt("2010-01-01"))
#' porpoisedyn
#' }
#' @export read.DeponsDyn
read.DeponsDyn <- function(fname, title="NA", landscape="NA", simtime="NA",
                           startday="NA", timestep=30, tz="GMT") {
  if (simtime=="NA")  simtime <- get.simtime(fname)
  if(!is.character(startday)) stop("'startday' must be a character string")
  if (startday=="NA" || is.na(startday))  startday <- NA
  all.data <- new("DeponsDyn")
  all.data@title <- title
  all.data@landscape <- landscape
  all.data@simtime <- as.POSIXlt(simtime)
  all.data@startday <- as.POSIXlt(startday, tz=tz)
  the.data <- utils::read.csv(fname, sep=";")
  if(!all(names(the.data)==c("tick", "PorpoiseCount", "FoodEnergyLevel", "PorpoiseEnergyLevel"))) {
    stop("The column names in the input data should be 'tick', 'PorpoiseCount', 'FoodEnergyLevel', and 'PorpoiseEnergyLevel'")
  }
  names(the.data) <- c("tick", "count", "lands.e", "anim.e")
  the.time <- tick.to.time(the.data$tick, origin=startday, timestep=timestep)
  the.data$real.time <- as.POSIXlt(the.time)
  all.data@dyn <- the.data
  return(all.data)
}



#' @title Plot a DeponsDyn object
#' @description Plot population dynamics simulated with DEPONS
#' @aliases plot.DeponsDyn
#' @param x DeponsDyn object
#' @param y Not used
#' @param dilute Integer. Plot only one in every 'dilute' values. Defaults to
#' 5, which yields a plot of the first simulated value and one in every five of
#' the following values.
#' @param plot.energy If set to TRUE it plots the amount of energy stored in
#' simulated and in the landscape in addition to the population count
#' @param plot.legend If set to TRUE, a legend is plotted
#' @param ... Optional plotting parameters
#' @examples
#' data("porpoisedyn")
#'
#' # Plot for specific range of years
#' rg <- c(as.POSIXlt("2011-01-01"), as.POSIXlt("2018-12-31"))
#' plot(porpoisedyn, xlim=as.POSIXct(rg), plot.energy=TRUE)
#'
#' \dontrun{
#' # Read data from default DEPONS simulation directory:
#' sim.dir <- "/Applications/DEPONS 2.1/DEPONS"
#' new.sim.name <- get.latest.sim(dir=sim.dir)
#' new.sim.out <- read.DeponsDyn(fname=paste(sim.dir, new.sim.name, sep="/"))
#' plot(new.sim.out)
#' }
setMethod("plot", signature("DeponsDyn", "missing"),
          function(x, y, dilute=5, plot.energy=TRUE, plot.legend=TRUE, ...)  {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            if (!(dilute %% 1 == 0)) stop("'dilute' must be an integer")
            use.row <- x@dyn$tick %% dilute == 0
            use.row[1] <- TRUE
            graphics::par(mar=c(4.2, 4.2, 4, 4.2))
            if(!hasArg(ylab)) {
              ylab <- "count"
            } else {
              ylab <- as.character(list(...)[["ylab"]])
            }
            if(!hasArg(lwd)) {
              lwd <- 2
            } else {
              lwd <- as.character(list(...)[["lwd"]])
            }
            if(!hasArg(lty)) {
              lty <- 1
            } else {
              lty <- as.character(list(...)[["lty"]])
            }
            if(!hasArg(col)) {
              col <- "blue"
            } else {
              col <- as.character(list(...)[["col"]])
            }
            if(!hasArg(type)) {
              type <- "l"
            } else {
              type <- as.character(list(...)[["type"]])
            }
            if(!hasArg(axes)) {
              axes <- TRUE
            } else {
              axes <- list(...)[["axes"]]
            }
            if(!hasArg("ylim")) {
              ylim <- c(min(x@dyn$count), max(x@dyn$count))
            } else {
              ylim <- list(...)[["ylim"]]
            }
            if(!hasArg("main")) {
              main <- "DEPONS simulation output"
            } else {
              main <- x@title
            }
            # Make plot with either date or tick on x-axis
            if (!is.na(x@startday)) {
              if(!hasArg("xlim")) {
                xlim <- NULL
              } else {
                xlim <- list(...)[["xlim"]]
              }
              if(!hasArg("xlab")) {
                xlab <- "time"
              } else {
                xlab <- list(...)[["xlab"]]
              }
              plot(x@dyn$real.time[use.row], x@dyn$count[use.row],
                   xlab=xlab, ylab=ylab, main=main, col=col, type=type,
                   xlim=xlim, ylim=ylim, axes=axes, lwd=lwd, lty=lty)
            } else {
              if(!hasArg("xlim")) {
                xlim <- c(min(x@dyn$tick), max(x@dyn$tick))
              } else {
                xlim <- list(...)[["xlim"]]
              }
              if(!hasArg("xlab")) {
                xlab <- "tick"
              } else {
                xlab <- list(...)[["xlab"]]
              }
              plot(x@dyn$tick[use.row], x@dyn$count[use.row],
                   xlab=xlab, ylab=ylab, main=main, col=col, type=type,
                   xlim=xlim, ylim=ylim, axes=axes, lwd=lwd, lty=lty)
            }
            if(plot.energy) {
              # Scale energetics to match max(count) - use for extra y-axis
              max.count <- max(max(x@dyn$count, na.rm=TRUE), ylim[2])
              min.count <- min(min(x@dyn$count, na.rm=TRUE), ylim[1])
              diff.count <- max.count - min.count
              max.e <- max(max(x@dyn$anim.e, na.rm=TRUE), max(x@dyn$lands.e, na.rm=TRUE))
              min.e <- min(min(x@dyn$anim.e, na.rm=TRUE), min(x@dyn$lands.e, na.rm=TRUE))
              diff.e <- max.e - min.e
              sc <- diff.count / diff.e
              if (!is.na(x@startday)) {
                graphics::lines(x@dyn$real.time[use.row], ylim[1] + sc*(x@dyn$anim.e[use.row]-min.e),
                                col="red", lwd=lwd)
                graphics::lines(x@dyn$real.time[use.row], ylim[1] + sc*(x@dyn$lands.e[use.row]-min.e),
                                col="grey", lwd=lwd)
              } else {
                graphics::lines(x@dyn$tick[use.row], sc*x@dyn$anim.e[use.row],
                                col="red", lwd=lwd)
                graphics::lines(x@dyn$tick[use.row], sc*x@dyn$lands.e[use.row],
                                col="grey", lwd=lwd)

              }
              the.labels <- seq(round(min.e/500)*500, round(max.e/100)*100, 500)
              the.at <- (the.labels - min.e) * sc + ylim[1]
              graphics::axis(4, at=the.at, labels=the.labels)
              graphics::mtext("energy level", side=4, line=2.6)
              if (plot.legend) graphics::legend("bottomright", fill=c(col, "red", "grey"),
                                                legend=c("Population count", "Animal energy level", "Landscape energy"))
            }
          }
)


