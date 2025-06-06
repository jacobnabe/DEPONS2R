## Methods used for calibrating DEPONS to make simulated porpoise movements and
## responses to disturbances match those observed in nature, and functions for
## reading in files produced by DEPONS when running in batch mode.
## Written by Axelle Cordier 2024-2025.

#' @title Plot distribution of turning angles, step lengths and speed of tracked simulated porpoises.
#'
#' @param depons_track Object of class `DeponsTrack` produced by either read.DeponsTrack or read.DeponsTrackBatch
#' @return Plot histograms of turning angles, step length and speed.
#' It also stores those metrics of the animal in a dataframe.
#' @import adehabitatLT
#' @export
#'
calib_01 <- function(depons_track) {
  # library(adehabitatLT)

  track <- depons_track@tracks[[1]]
  if (is.null(track) || nrow(as.data.frame(track)) < 2) {
    stop("No data in the DeponsTrack object.")
  }

  track_df <- as.data.frame(track)
  #dummy datetime column for ltraj
  track_df$datetime <- as.POSIXct(seq(0, by = 1800, length.out = nrow(track_df)), origin = "2010-01-01", tz = "UTC")
  ltraj <- as.ltraj(
    xy = track_df[, c("x", "y")],
    date = track_df$datetime,
    id = "DeponsTrack")
  traj <- ld(ltraj)
  traj$turning_angle <- traj$rel.angle * (180 / pi)
  traj$speed <- traj$dist / traj$dt

  graphics::par(mfrow = c(1, 3))
  hist(traj$dist, main = "Step lengths", xlab = "Step Length (m)", col = "lightgreen", border = "white")
  hist(traj$turning_angle, main = "Turning angles", xlab = "Turning Angle (degrees)", col = "lightblue", border = "white", xlim = c(-180, 180))
  hist(traj$speed, breaks=20,main = "Speed", xlab = "Speed (m/s)", col = "lightcoral", border = "white")

  graphics::par(mfrow = c(1, 1))
  return(traj)
}


#' @title Calculate calibration metrics of previously filtered tracks (fine-scale or large-scale)
#'
#' @param track_cleaned A dataframe of the filtered track (either fine scale of large scale).
#' @param option A character string, either `"fine"` or `"large"`, indicating which type of movement (fine-scale or large-scale)
#' metrics to return.`"fine"` returns home range, mean net squared displacement, and mean residence time. `"large"` returns home range, 
#' maximum net squared displacement, and sinuosity.
#' @return A dataframe storing either fine scale metrics (home range (HR, km2), mean net squared displacement (NSD, km2) 
#' and mean residence time (RT, days)) or large scale metrics (HR, max NSD and sinuosity index).
#' @import adehabitatHR
#' @export
#' @examples
#' # filtering fine-scale movements on porpoise tracks
#' 
#'data(porpoisetrack)
#'track <-as.data.frame(porpoisetrack)
#'track$year <- track$tick / 17280
#'track$yearRound <- floor(track$year) + 1
#'track$tickNew <- ave(track$tick, track$Id, track$yearRound, FUN = seq_along)
#'track$day <- track$tickNew / 48
#'track$dayRound <- floor(track$day) + 1
#'track$date <- as.POSIXct("2014-01-01 00:00:00", tz = "GMT") + (track$tick * 1800)
#'track_fine <- track[track$DispersalMode == 0, ]
#' 
# extract only noon positions
#'noon_tracks <- track_fine[format(track_fine$date, "%H:%M:%S") == "12:00:00", ]
#' 
#' # identify 30 consecutive days with noon data
#'all_days <- sort(unique(as.Date(noon_tracks$date)))
#' consecutive_days <- NULL
#' 
#'for (i in 1:(length(all_days) - 29)) {
#'if (all(diff(all_days[i:(i + 29)]) == 1)) {
#'  consecutive_days <- all_days[i:(i + 29)]
#'  break
#'}
#'}
#'# filter data to only the 30 consecutive days
#'filtered_tracks <- noon_tracks[as.Date(noon_tracks$date) %in% consecutive_days, ]
#'
#'calib_02(filtered_tracks, option = "fine")


calib_02 <- function(track_cleaned, option) {
  
  coordinates(track_cleaned) <- ~ x + y
  
  # Home Range
  track <- track_cleaned[, c("Id")] # kernelUD function only accepts Id, x and y
  cellsize <- 400
  
  null.grid <- expand.grid(x = seq(min(coordinates(track)[, 1]) - 300000, 
                                   max(coordinates(track)[, 1]) + 300000, by = cellsize),
                           y = seq(min(coordinates(track)[, 2]) - 300000, 
                                   max(coordinates(track)[, 2]) + 300000, by = cellsize))
  coordinates(null.grid) <- ~ x + y
  gridded(null.grid) <- TRUE
  kernel.ref <- kernelUD(track, h = "href", grid = null.grid)
  kernel.poly <- getverticeshr(kernel.ref, percent = 95, unin = c("m"), unout = "km2") 
  HRsize <- data.frame(kernel.poly$id, kernel.poly$area)
  colnames(HRsize) <- c("ID", "HRarea")
  
  # NSD mean and max
  first_x <- coordinates(track_cleaned)[, 1][[1]]
  first_y <- coordinates(track_cleaned)[, 2][[1]]
  
  track_cleaned$dist <- sqrt((coordinates(track_cleaned)[, 1] - first_x)^2 + 
                               (coordinates(track_cleaned)[, 2] - first_y)^2)/1000 # in km
  track_cleaned$nsd <- track_cleaned$dist^2
  
  meanNSD <- mean(track_cleaned$nsd)
  maxNSD <- max(track_cleaned$nsd)
  
  ## fine scale
  if(option == "fine") {
    
    # Residence Time
    radius <- 5000  # in meters
    
    ltraj <- as.ltraj(xy = coordinates(track_cleaned), date = track_cleaned$date, id = track_cleaned$Id)
    
    rt <- residenceTime(ltraj, radius, maxt=60*60*24*30) 
    mean_rt <- mean(rt[[1]][["RT.5000"]], na.rm = TRUE)/(24*60*60) 
    fine_metrics <- data.frame(HR = HRsize$HRarea, meanNSD = meanNSD, Rt=mean_rt)
    
    return(fine_metrics)
  }
  
  ## large scale
  if(option == "large") {
    
    # Sinuosity 
    traj <- as.ltraj(xy = coordinates(track_cleaned), date = track_cleaned$date, id= track_cleaned$Id)
    traj <- ld(traj)
    
    p <- mean(traj$dist, na.rm=TRUE)
    b <- stats::sd(traj$dist, na.rm=TRUE)/p
    c <- mean(cos(traj$rel.angle), na.rm=TRUE)
    
    # according to Benhamou (2004)
    sinuosity <- 2/sqrt(p * (((1 + c)/(1 - c)) + b^2))
    
    large_metrics <- data.frame(HR = HRsize$HRarea, maxNSD = maxNSD, sinuosity = sinuosity)
    return(large_metrics)
  }
}

#' @title Plotting of simulated fine or large-scale metrics against argos data
#'
#' @param sim_metrics A dataframe of movement metrics obtained with the calib_02 function
#' @param option A character string, either `"fine"` or `"large"`, indicating which set of Argos metrics (fine-scale or large-scale) 
#' to plot against simulated ones.
#' @return a plot of real (Argos) vs simulated metrics
#' @export
#'
plot_calib02 <- function(sim.metrics, option) {

  data("argosmetrics")

  par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))
  argos.metrics<-argos.metrics[[option]]
  metrics <-c(colnames(argos.metrics[2:4]))

  for (i in seq_along(metrics)) {
    metric <- metrics[i]
    argos <- argos.metrics[[metric]]
    sim <- sim.metrics[[metric]]

    if (nrow(sim_metrics) == 1) {
      boxplot(argos,ylab = metric, col = "#d73027", boxwex = 0.1)
      points(x = 1, y = sim[1], col = "#ef8a62", pch = 19)
      legend("topright", c("Argos", "Sim"), border="black", fill = c("#d73027", "#ef8a62"))

    } else {
      boxplot(list(Argos = argos, Sim = sim),
              ylab = metric,
              col = c("#d73027", "#ef8a62"), boxwex = 0.1)
    }
  }
}

#' @title Read and merges DEPONS Batchmap and Statistics Files
#'
#' @description Reads batch map files and statistics files from a specified directory
#' and returns a list of `DeponsDyn` objects and parameter values.
#'
#' @param dir Character string specifying the directory path containing the `Batchmap` and `Statistics` files.
#' @param par Character vector specifying the column names to extract from the batch map file for each run.
#' #' These parameters are then stored in the `Parameters` list.
#' @param title Optional character string
#' @param landscape Character string. Name of the simulation landscape. Default is "NA".
#' @param simtime Optional character string with the date and time when the
#' simulation finished (format yyyy-mm-dd).
#' @param startday The start of the period that the  simulation represents, i.e.
#' the real-world equivalent of 'tick 1' (character string of the
#' form 'yyyy-mm-dd', or POSIXlt)
#' @param timestep Time step used in the model, in minutes. Defaults to 30 in
#' DEPONS.
#' @param tz Timezone.
#' @return A list of `DeponsDyn` objects and parameter values associated with run id
#'
#' @examples \dontrun{
#' # Specify the directory containing Batchmap and Statistics files
#' dir_path <- "path/to/batchdata"
#'
#' # Specify parameters to extract from Batchmap files
#' par <- c("parameter1", "parameter2")
#'
#' # Run the function
#' results <- read.DeponsBatch(
#'   dir = dir_path,
#'   par = par,
#'   startday = "2010-01-01"
#' )
#'}
#'@export

read.DeponsDynBatch <- function(dir, par, title = "NA", landscape = "NA", simtime = "NA",
                                startday = "NA", timestep = 30, tz = "UTC") {
  if (missing(par)) {
    stop("Specify parameters to select using the 'par' argument")
  }

  # get batch map filename and statistics filename
  popres <- list.files(dir, full.names = TRUE)
  popres_batchmap <- popres[grepl("^.*/Statistics.*batch_param.*\\.csv$", popres)]
  popres_stats <- popres[grepl("^.*/Statistics.*\\.csv$", popres) & !grepl("batch_param", popres)]

  if (length(popres_batchmap) != length(popres_stats)) {
    stop("The number of batch map and statistics files must be the same.")
  }
  if (simtime == "NA")
    simtime <- get.simtime(popres_stats)
  if (!is.character(startday))
    stop("'startday' must be a character string")
  if (startday == "NA" || is.na(startday))
    startday <- NA

  sample_popmap <- utils::read.csv(popres_batchmap[1], sep = ";")
  if (!all(par %in% colnames(sample_popmap))) {
    missing_pars <- par[!par %in% colnames(sample_popmap)] #check if par exist
    stop(paste("The following parameters do not exist in the batch map file:",
               paste(missing_pars, collapse = ", ")))
  }

  # initialise output
  depons_dyn <- list()
  parameter_values <- data.frame(par = character(), value = numeric(),
                                 runID = character(), stringsAsFactors = FALSE)

  for (i in seq_along(popres_batchmap)) {
    cat("Processing simulation", i, "/", length(popres_batchmap), "\n")

    popmap <- utils::read.csv(popres_batchmap[i], sep = ";") #read parameter map
    popmap$Sim <- i
    if (!"run" %in% colnames(popmap)) {
      stop("The batch map file must contain a 'run' column.")
    }

    popstats <- utils::read.csv(popres_stats[i], sep = ";") #read stats output
    popstats$Sim <- i

    unique_runs <- unique(popstats$run) #get runs

    for (run_id in unique_runs) {
      cat("Processing run", run_id, "of simulation", i, "\n")

      run_stats <- subset(popstats, popstats$run == run_id)
      run_map <- subset(popmap, popmap$run == run_id)

      if (nrow(run_map) == 0) {
        warning(paste("No matching run in batch map for run", run_id, "in simulation", i))
        next
      }
      merged_run <- merge(run_stats, run_map, by = c("Sim", "run"),
                          all.x = TRUE)  # merge stats with parameters

      #transform into DeponsDyn object (similar to read.DeponsDyn)
      all.data <- new("DeponsDyn")
      all.data@title <- title
      all.data@landscape <- landscape
      all.data@simtime <- as.POSIXlt(simtime)
      all.data@startday <- as.POSIXlt(startday, tz = tz)

      dyn_data <- merged_run[, c("tick", "PorpoiseCount", "FoodEnergyLevel",
                                 "PorpoiseEnergyLevel"), drop = FALSE]
      colnames(dyn_data) <- c("tick", "count", "lands.e", "anim.e")
      the.time <- tick.to.time(dyn_data$tick, origin = startday, timestep = timestep)
      dyn_data$real.time <- as.POSIXlt(the.time, tz=tz)
      all.data@dyn <- dyn_data

      # add to the DeponsDyn list
      run_name <- paste0("Run_", run_id)
      depons_dyn[[run_name]] <- all.data


      # extract parameter values
      run_params <- as.list(run_map[1, par, drop = FALSE])
      run_param_df <- data.frame(
        runID = run_name,
        t(run_params),
        stringsAsFactors = FALSE
      )
      parameter_values <- rbind(parameter_values, run_param_df)
    }
  }

  return(list(DeponsDyn = depons_dyn, Parameters = parameter_values))
}



#' @title Read and Process DEPONS Batchmap and Statistics Files
#'
#' @description Reads batch map files and random porpoise files from a specified directory, merges
#' them for each run, and returns a list of `DeponsTrack` objects and parameter values
#' @param dir  Character string specifying the directory path containing the `Batchmap` and `RandomPorpoise` files
#' @param par Character vector specifying the column names to extract from the batch map file for each run.
#' These parameters are then stored in the `Parameters` list
#' @param title Optional character string giving name of simulation
#' @param landscape Character string. Name of the simulation landscape
#' @param simtime Character sting with date of simulation (format yyyy-mm-dd).
#' If not provided this is obtained from name of input file
#' @param crs Character, coordinate reference system (map projection)
#' @param tz Time zone used in simulations. Defaults to UTC/GMT
#' @return A list of `DeponsTrack` objects and parameter values associated with run id
#'
#' @examples \dontrun{
#' # Specify the directory containing Batchmap and Statistics files
#' dir_path <- "path/to/batchdata"
#'
#' # Specify parameters to extract from Batchmap files
#' par <- c("parameter1", "parameter2")
#'
#' # Run the function
#' results <- read.DeponsBatch(
#'   dir = dir_path,
#'   par = par,
#'   crs = "+proj=longlat +datum=WGS84"
#' )
#'}
#'@export

read.DeponsTrackBatch <- function(dir, par, title = "NA", landscape = "NA", simtime = "NA",
                                  crs = as.character(NA), tz = "UTC") {
  if (missing(par)) {
    stop("Specify parameters to select using the 'par' argument")
  }

  # get batch map filename and random porpoise filename
  popres <- list.files(dir, full.names = TRUE)
  popres_batchmap <- popres[grepl("^.*/RandomPorpoise.*batch_param.*\\.csv$", popres)]
  popres_tracks <- popres[grepl("^.*/RandomPorpoise.*\\.csv$", popres) & !grepl("batch_param", popres)]

  if (length(popres_batchmap) != length(popres_tracks)) {
    stop("The number of batch map and track files must be the same.")
  }
  if (simtime == "NA")
    simtime <- get.simtime(popres_tracks)

  # initialise output
  depons_objects <- list()
  parameter_values <- data.frame(par = character(), value = numeric(), runID = character(), stringsAsFactors = FALSE)

  for (i in seq_along(popres_tracks)) {
    cat("Processing file pair", i, "/", length(popres_tracks), "\n")
    raw.data <- utils::read.csv(popres_tracks[i], sep = ";")
    unique_runs <- unique(raw.data$run)

    porpoisetrack <- utils::read.csv(popres_batchmap[i], sep = ";")
    if (!"run" %in% colnames(porpoisetrack)) {
      stop("The batch map file must contain a 'run' column.")
    }

    for (run_id in unique_runs) {
      cat("Processing run:", run_id, "\n")
      run_tracks <- subset(raw.data, raw.data$run == run_id)
      run_map <- subset(porpoisetrack, porpoisetrack$run == run_id)
      tracks <- list()

      ids <- sort(unique(run_tracks$Id))
      for (j in seq_along(ids)) {
        id <- ids[j]
        one_track <- run_tracks[run_tracks$Id == id, ]
        ot_coords <- one_track[, c("UtmX", "UtmY")]
        colnames(ot_coords) <- c("x", "y")
        ot_data <- one_track[, c("tick", "Id", "EnergyLevel", "DeterStrength",
                                 "DispersalMode", "PSMActive", "PSMTargetUtmX",
                                 "PSMTargetUtmY")]
        track <- sp::SpatialPointsDataFrame(ot_coords, ot_data,
                                            proj4string = sp::CRS(crs))
        tracks[[j]] <- track
      }

      # make them depons object
      depons_obj <- new("DeponsTrack")
      depons_obj@title <- title
      depons_obj@landscape <- landscape
      depons_obj@simtime <- if ("POSIXlt" %in% class(simtime)) simtime else as.POSIXlt(simtime, tz = tz)
      depons_obj@crs <- crs
      depons_obj@tracks <- tracks

      # add to the DeponsTrack list
      run_name <- paste0("Run_", run_id)
      depons_objects[[run_name]] <- depons_obj

      # extract parameter values
      run_params <- as.list(run_map[1, par, drop = FALSE])
      run_param_df <- data.frame(
        runID = run_name,
        t(run_params),
        stringsAsFactors = FALSE
      )
      parameter_values <- rbind(parameter_values, run_param_df)

    }
  }

  return(list(DeponsTrack = depons_objects, Parameters = parameter_values))
}
