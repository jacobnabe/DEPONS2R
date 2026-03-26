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
#' maximum net squared displacement, sinuosity and cumulative distance moved.
#' @param h A character string or number representing the smoothing parameter in the `kernelUD` function
#' from the `adehabitatHR` package. For the NorthSea, h should be set to "href" and
#' h should be "15000" (15 km) in the Kattegat.
#' @return A dataframe storing either fine scale metrics (home range (HR, km2), mean net squared displacement (NSD, km2)
#' and mean residence time (RT, days)) or large scale metrics (HR, max NSD, sinuosity index and cumulative distance moved (km)).
#' @import adehabitatHR
#' @export
#' @examples \dontrun{
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
#'calib_02(filtered_tracks, option = "fine", h="href")
#'}


calib_02 <- function(track_cleaned, option, h) {

  coordinates(track_cleaned) <- ~ x + y

  # Home Range
  track <- track_cleaned[, c("Id")] # kernelUD function only accepts Id, x and y
  grid_cells <- 400
  extent_factor <-5

  kernel.ref <- kernelUD(track, h = h, grid = grid_cells, extent = extent_factor)
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

    # cumulative distance in km
    cumdist <- sum(traj$dist)/1000

    large_metrics <- data.frame(HR = HRsize$HRarea, maxNSD = maxNSD, sinuosity = sinuosity, cumDist=cumdist)
    return(large_metrics)
  }
}

#' @title Plotting of simulated fine or large-scale metrics against argos data
#'
#' @param sim.metrics A dataframe of movement metrics obtained with the calib_02 function
#' @param option A character string, either `"fine"` or `"large"`, indicating which set of Argos metrics (fine-scale or large-scale)
#' to plot against simulated ones.
#' @param landscape A character strong, either `"NorthSea"` or `"Kattegat"`, indicating
#' metrics from which landscape to plot against simulated ones.
#' @return a plot of real (Argos) vs simulated metrics
#' @export
#'
plot_calib02 <- function(sim.metrics, option, landscape) {

  graphics::par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))
  argos.metrics<-DEPONS2R::argosmetrics[[landscape]][[option]]
  metrics <-c(colnames(argos.metrics[2:max(col(argos.metrics))]))

  for (i in seq_along(metrics)) {
    metric <- metrics[i]
    argos <- argos.metrics[[metric]]
    sim <- sim.metrics[[metric]]

    if (nrow(sim.metrics) == 1) {
      boxplot(argos,ylab = metric, col = "#d73027", boxwex = 0.1)
      points(x = 1, y = sim[1], col = "#ef8a62", pch = 19)
      graphics::legend("topright", c("Argos", "Sim"), border="black", fill = c("#d73027", "#ef8a62"))

    } else {
      boxplot(list(Argos = argos, Sim = sim),
              ylab = metric,
              col = c("#d73027", "#ef8a62"), boxwex = 0.1)
    }
  }
}

#' @title Create piling-response table from simulated porpoise counts
#' @description Converts a simulation summary table into the aggregated
#' DPM-vs-hours-since-piling table used for response to piling calibration.
#' The function matches simulated porpoise counts to CPOD stations and pods,
#' assigns each record to the nearest piling event (found by the helper function
#' `find.nearest.piling`), groups records into distance intervals from piling (
#' using the helper function `get.pods.at.dist.sim.dat`), and aggregates detections
#' by piling event and hour since piling (using the helper function `aggregate.dpm.sim.dat`).
#' The output has the same general structure as the observed DPM data used for
#' comparison with simulations.
#' The default distance interval classes are 1.5-3 km, 3-6 km, 6-9 km, 9-12 km,
#' 12-15 km and 15-18 km (modify parameters min.dists and max.dists to get different
#' intervals).
#' @param sim.data Data frame with at least columns `block`, `datetime`, `PorpoiseCount`.
#' @param cpod.data A data frame of observed CPOD data containing at least
#' the columns `station`, `time`, and `pod`. For each station, this table is
#' used to identify which CPOD was active and when pod switches occurred, so
#' that simulated records can be matched to the correct pod before aggregation.
#' @param cpod.coord CPOD coordinates table. Should contain at least columns `station`,
#' `x`, `y`. Station numbers should correspond to block numbers in the "block.asc" file
#' used to count porpoises per block in DEPONS.
#' @param piling.data Piling data table. Should contain at least columns `id`,
#' `x`, `y`, `start.time`,`stop.time`. The `start.time` and `stop.time` columns should be
#' date-time objects or character strings convertible to date-time format,
#' expressed in the same time zone as `sim.dat$datetime`.
#' @param min.dists Numeric vector of lower distance bounds in meters. This is to create
#' an interval of distance from piling, and dpm is aggregated per distance interval.
#' @param max.dists Numeric vector of upper distance bounds in meters.
#' @param c.val Simulation parameter value for `c`.
#' @param RT.val Simulation parameter value for `RT`.
#' @param tz Time zone.
#' @param bad.data A data frame with columns `date.start`, `date.end` (Date
#' objects), and `station` (numeric). One row per exclusion. Use `NULL`
#' (default) to treat all data as good.
#' @return A data frame with aggregated DPM-vs-hour, with the same format as
#' observed DPM data.
#' @examples \dontrun{
#' load(gemini.obs.data)
#' bad.data <- data.frame(date.start = as.Date("2010-06-20"),
#' date.end = as.Date("2010-06-25"),
#' station = c(7, 8))
#'
#' dpm.sim <- calib_03_make_sim_dpm(sim.data = sim.data,
#' cpod.data = gemini.obs.data$cpod.data,
#' cpod.coord = gemini.obs.data$cpod.coord,
#' piling.data = gemini.obs.data$piling.data,
#' c.val = gemini.sim.data$sim.data$c,
#' RT.val = gemini.sim.data$sim.data$RT,
#' tz = "Europe/Amsterdam",
#' bad.data = bad.data)
#'}
#' @export calib_03_make_sim_dpm
calib_03_make_sim_dpm <- function(sim.data, cpod.data, cpod.coord, piling.data,
                                      min.dists = c(1500, 3000, 6000, 9000, 12000, 15000),
                                      max.dists = c(3000, 6000, 9000, 12000, 15000, 18000),
                                      c.val, RT.val, tz, bad.data) {

  extract.table <- data.frame(min.dists = min.dists, max.dists = max.dists)
  extract.table$lab <- paste0(min.dists / 1000, "-", max.dists / 1000, " km")

  sim.data$station <- as.numeric(sim.data$block)
  sim.data$time <- as.POSIXct(sim.data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = tz)

  cpod.lookup <- cpod.data[, c("station", "time", "pod")]
  cpod.lookup <- cpod.lookup[!is.na(cpod.lookup$pod), ]
  cpod.lookup$time <- as.POSIXct(cpod.lookup$time, tz = tz)
  cpod.lookup <- cpod.lookup[order(cpod.lookup$station, cpod.lookup$time), ]

  sim.data$pod <- NA_real_

  for (st in sort(unique(cpod.lookup$station))) {
    cp_st <- cpod.lookup[cpod.lookup$station == st, ]
    sim_st <- sim.data$station == st

    if (!any(sim_st)) next

    pod_ids <- unique(cp_st$pod)

    if (length(pod_ids) == 1) {
      sim.data$pod[sim_st & sim.data$time <= max(cp_st$time)] <- pod_ids[1]
    } else {
      pod1 <- pod_ids[1]
      pod2 <- pod_ids[2]
      switch_time <- min(cp_st$time[cp_st$pod == pod2], na.rm = TRUE)

      sim.data$pod[sim_st & sim.data$time < switch_time]  <- pod1
      sim.data$pod[sim_st & sim.data$time >= switch_time] <- pod2
    }
  }

  sim.data <- sim.data[!is.na(sim.data$pod), ]

  sim.data$dpm <- sim.data$PorpoiseCount
  sim.data$nall <- 500
  sim.data$good.data <- TRUE
  if (!is.null(bad.data)) {
    for (i in seq_len(nrow(bad.data))) {
      sim.data$good.data[
        as.Date(sim.data$time) >= bad.data$date.start[i] &
          as.Date(sim.data$time) <= bad.data$date.end[i] &
          sim.data$station == bad.data$station[i]
      ] <- FALSE
    }
  }

  sim.data <- sim.data[, c("station", "time", "pod", "dpm", "nall", "good.data")]

  sim.data2 <- find.nearest.piling(cpod.coord, sim.data, piling.data)

  extract.list <- vector("list", length = nrow(extract.table))
  names(extract.list) <- extract.table$lab

  for (j in seq_len(nrow(extract.table))) {
    tmp <- get.pods.at.dist.sim.dat(extract.table$min.dists[j],
      extract.table$max.dists[j], sim.data2, tz=tz)
    extract.list[[j]] <- tmp[tmp$np != "NA", ]
  }

  sim.dpm <- data.frame()

  for (j in seq_along(extract.list)) {
    sim.data4 <- aggregate.dpm.sim.dat(extract.list[[j]])
    sim.data5 <- sim.data4[!is.na(sim.data4$sum.dpm), ]

    tt <- which(names(sim.data5) == "minutes.per.hr")
    names(sim.data5)[tt] <- "minutes.w.dpm.data"

    sim.data5$dist.from.nearest.piling <- names(extract.list)[j]
    sim.dpm <- rbind(sim.dpm, sim.data5)
  }

  sim.dpm$c  <- c.val
  sim.dpm$RT <- RT.val

  sim.dpm
}

#' @title Compute SSD for one response to piling simulation
#' @description Computes the sum of squared differences (SSD) between observed
#' and simulated porpoise detection responses to piling, separately for each
#' distance class and summed across classes.
#'
#' Observed and simulated aggregated DPM data are first summarised by hour since
#' piling. Simulated values are then scaled to have the same overall mean and
#' standard deviation as the observed values before SSD is calculated.
#'
#' @param sim.dpm Data frame returned by `calib_03_make_dpm_simdat()`.
#' @param obs.dpm Observed DPM-vs-hour data.
#' @param filter.good.data Logical. If `TRUE`, rows flagged as
#' `good.data == FALSE` are excluded from both `sim.dpm` and `obs.dpm`
#' before computing the SSD. Defaulft is `TRUE`.
#' @param plot Logical. If `TRUE`, the function plots the observed and scaled simulated
#' response curves for each distance class
#' @return A list with `sum.ssd` and per-distance SSD table.
#' @examples \dontrun{
#' load(gemini.obs.data)
#' load(gemini.sim.data)
#'
#' calib_03_ssd(sim.dpm=gemini.sim.data$dpm.sim,
#' obs.dpm = gemini.obs.data$dpm.vs.hr, plot=T)
#'}
#' @export calib_03_ssd
calib_03_ssd <- function(sim.dpm, obs.dpm, filter.good.data = TRUE, plot = FALSE) {

  if (filter.good.data) {
    sim.dpm <- sim.dpm[sim.dpm$good.data, ]
    obs.dpm <- obs.dpm[obs.dpm$good.data, ]
  }

  dist_classes <- unique(as.character(obs.dpm$dist.from.nearest.piling))
  sum.ssd <- 0
  ssd_by_dist <- data.frame()

  obs.all <- obs.dpm[obs.dpm$dist.from.nearest.piling %in% dist_classes, ]
  sim.all <- sim.dpm[sim.dpm$dist.from.nearest.piling %in% dist_classes, ]

  s <- data.frame()

  for (sel.dist in dist_classes) {
    obs2 <- obs.all[obs.all$dist.from.nearest.piling == sel.dist, ]
    obs.sum <- summarise.aggregated.dpm.dat(obs2, label = sel.dist)
    obs.sum2 <- data.frame(obs.sum[obs.sum$hrs.since.piling >= 0, ], sel.dist = sel.dist)

    sim2 <- sim.all[sim.all$dist.from.nearest.piling == sel.dist, ]
    sim.sum <- summarise.aggregated.dpm.dat(sim2, label = sel.dist)
    sim.sum2 <- data.frame(sim.sum[sim.sum$hrs.since.piling >= 0, ], sel.dist = sel.dist)

    if (any(obs.sum2$hrs.since.piling != sim.sum2$hrs.since.piling)) {
      stop("Mismatch in hrs.since.piling between observed and simulated data.")
    }

    all.data2 <- cbind(sim.sum2,obs.dpm = as.numeric(obs.sum2$mean.dpm),
      obs.se.dpm = as.numeric(obs.sum2$se.dpm))

    s <- rbind(s, all.data2)
  }

  s$s.sim.dpm <- scale(s$mean.dpm, scale = TRUE)
  s$s.sim.dpm <- as.numeric(s$s.sim.dpm) * stats::sd(s$obs.dpm) + mean(s$obs.dpm)

  for (sel.dist in dist_classes) {
    sel.dist.data <- s[s$sel.dist == sel.dist, ]
    if (nrow(sel.dist.data) == 0) next

    ssd <- sum((sel.dist.data$s.sim.dpm - sel.dist.data$obs.dpm)^2)

    sum.ssd <- sum.ssd + ssd
    ssd_by_dist <- rbind(ssd_by_dist, data.frame(dist.from.nearest.piling = sel.dist, ssd = ssd))
  }

  if (plot) {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par), add = TRUE)

    n_dist <- length(dist_classes)
    ncol <- 2
    nrow <- ceiling(n_dist / ncol)

    graphics::par(mfrow = c(nrow, ncol), mar = c(2, 2, 2, 1), oma = c(4, 3, 0.5, 0.5))

    y_max <- max(c(s$obs.dpm + s$obs.se.dpm, s$s.sim.dpm), na.rm = TRUE)
    y_min <- min(c(s$obs.dpm - s$obs.se.dpm, s$s.sim.dpm), na.rm = TRUE)

    for (sel.dist in dist_classes) {
      sel.dist.data <- s[s$sel.dist == sel.dist, ]
      sel.ssd <- ssd_by_dist$ssd[ssd_by_dist$dist.from.nearest.piling == sel.dist]

      graphics::plot(sel.dist.data$hrs.since.piling, sel.dist.data$obs.dpm, pch = 16,
        xlab = "Hrs since piling stopped", ylab = "Relative porpoise density",
        main = sel.dist, ylim = c(y_min, y_max))
      graphics::lines(sel.dist.data$hrs.since.piling, sel.dist.data$obs.dpm)

      graphics::arrows(sel.dist.data$hrs.since.piling, sel.dist.data$obs.dpm - sel.dist.data$obs.se.dpm,
        sel.dist.data$hrs.since.piling, sel.dist.data$obs.dpm + sel.dist.data$obs.se.dpm,
        length = 0.02, angle = 90, code = 3)

      graphics::points(sel.dist.data$hrs.since.piling, sel.dist.data$s.sim.dpm, col = "red")
      graphics::lines(sel.dist.data$hrs.since.piling, sel.dist.data$s.sim.dpm, col = "red")

      graphics::text(x = max(sel.dist.data$hrs.since.piling, na.rm = TRUE) * 0.8,
        y = y_min + 0.05 * (y_max - y_min), labels = paste("SSD:", round(sel.ssd, 2)))

      if (sel.dist == dist_classes[1]) {
        graphics::legend("topleft", legend = c("Observed", "Sim (scaled)"),
          col = c("black", "red"), lty = 1, pch = 16,bty = "n")
      }
    }

    graphics::mtext("Hrs since piling stopped", side = 1, line = 1, outer = TRUE)
    graphics::mtext("Relative porpoise density", side = 2, line = 1, outer = TRUE)
  }

  list(sum.ssd = sum.ssd, ssd_by_dist = ssd_by_dist)
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

  sample_popmap <- utils::read.csv(popres_batchmap[1])
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

    popmap <- utils::read.csv(popres_batchmap[i]) #read parameter map
    popmap$Sim <- i
    if (!"run" %in% colnames(popmap)) {
      stop("The batch map file must contain a 'run' column.")
    }

    popstats <- utils::read.csv(popres_stats[i]) #read stats output
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
    raw.data <- utils::read.csv(popres_tracks[i])
    unique_runs <- unique(raw.data$run)

    porpoisetrack <- utils::read.csv(popres_batchmap[i])
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
