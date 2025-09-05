
# Author: Jacob Nabe-Nielsen
# Date: 1 August 2020 (v. 0.9)
# Licence GPL v3
# Description: Methods and classes for reading and summarizing DEPONS raster
#   objects


#' @title  DeponsRaster-class
#' @description Stores objects containing raster landscapes used as input in
#' DEPONS simulations.
#' @slot type Character. Identifies the kind of data stored in the raster; should
#' be 'food', 'patches', bathymetry', 'dtc', 'salinity', 'blocks' or 'NA'.
#' @slot landscape Character  Identifier for the landscape used in the DEPONS
#' simulations. The landscapes 'DanTysk', 'Gemini', 'Kattegat', 'North Sea',
#' 'Homogeneous', and 'User defined' are distributed with the DEPONS model.
#' @slot crs Object of class "CRS", i.e. the coordinate reference system. This
#'is provided as a proj4string text string.
#' @slot header Data frame with data on number of columns and rows in the
#' input raster, the coordinates of the lower left corner, the size of each
#' grid cell and the integer value used to represent missing data.
#' @slot ext Data frame with the extent of the landscape.
#' @slot data The actual data values for each of the grid cells.
#' @exportClass DeponsRaster
#' @examples a.deponsraster <- new("DeponsRaster")
#' a.deponsraster
#' @note DeponsRaster-objects are typically read in from ascii raster files that
#' have been used for DEPONS simulations.
#' @seealso \code{\link[DEPONS2R]{plot.DeponsRaster}}, \code{\link[DEPONS2R]{read.DeponsRaster}} and
#' \code{\link[DEPONS2R]{make.blocksraster}}. \code{\link{bathymetry}} is an example
#' of a \code{DeponsRaster}-object.
#' @import raster
#' @import methods
#' @import sp
setClass(Class="DeponsRaster",
  slots=list(type="character", landscape="character", crs="character",
  header="data.frame", ext="data.frame", data="matrix")
)


setMethod("initialize", "DeponsRaster",
          function(.Object, type="NA", landscape="NA",
                   crs="NA", ncols, nrows, xllcorner,
                   yllcorner, cellsize=400,
                   nodata_value) {
            .Object@type <- type
            .Object@landscape <- landscape
            .Object@crs <- crs
            if(missing(ncols)) ncols <- 2
            if(missing(nrows)) nrows <- 2
            if(missing(xllcorner)) xllcorner <- 2
            if(missing(yllcorner)) yllcorner <- 2
            if(missing(cellsize)) cellsize <- 400
            if(cellsize!=400) base::warning("Cellsize must be 400 m")
            if(missing(nodata_value)) nodata_value <- 2
            .Object@header <- data.frame("var"=c("ncols", "nrows", "xllcorner",
                                                 "yllcorner", "cellsize",
                                                 "nodata_value"),
                                         "value"=c(ncols, nrows, xllcorner,
                                                   yllcorner, cellsize,
                                                   nodata_value)
            )
            .Object@ext <- data.frame("xleft"=xllcorner, "ybottom"=yllcorner,
                                      "xright"=xllcorner+cellsize*ncols,
                                      "ytop"=yllcorner+cellsize*nrows)
            .Object@data <- base::matrix(rep(nodata_value, nrows*ncols),
                                         nrow=nrows, ncol=ncols)
            .Object
          }
)

#' @name summary
#' @title Summary
#' @rdname summary
#' @aliases summary,DeponsRaster-method
#' @return list summarizing the DeponsRaster object
#' @exportMethod summary
setMethod("summary", "DeponsRaster",
          function(object) {
            cat("class: \t", "DeponsRaster \n")
            cat("type:  \t", object@type, "\n")
            cat("landsc:\t", object@landscape, "\n")
            cat("crs:   \t", object@crs, "\n")
            cat("extent:\t ", object@ext$xleft, ", ", object@ext$ybottom, ", ",
                object@ext$xright, ", ", object@ext$ytop,
                " (xleft, ybottom, xright, ytop) \n", sep="")
            cat("dim:   \t", as.character(nrow(object@data), ""))
            cat(" x", as.character(ncol(object@data)))
            cat(" (nrow, ncol) \n")
            dim <- data.frame("nrow"=nrow(object@data), "ncol"=ncol(object@data))
            out <- list(
              "type" <- object@type,
              "landscape" <- object@landscape,
              "crs" <- object@crs,
              "extent" <- object@ext,
              "dim" <- dim
            )
            return(invisible(out))
          }
)


#' @title Reading DEPONS raster files
#' @description Function  for reading raster files that have been used in DEPONS
#'   simulations. DEPONS rasters define amount of food available for simulated
#'   animals, spatial distribution of food patches, bathymetry, and distance to
#'   coast (dtc). The 'blocks' raster enables the user to count animals in
#'   specific parts of the landscape during simulations. See Nabe-Nielsen et al.
#'   (2018) for details regarding these files. In DEPONS 2.0 the salinity raster
#'   file was introduced; see TRACE document for details:
#'   \url{https://github.com/jacobnabe/DEPONS}
#'
#' @param fname Filename (character), including the path to the DEPONS raster
#'   file.
#' @param type The kind of data stored in the raster; c('food', 'patches',
#'   'bathymetry', 'dtc', 'salinity', 'blocks').
#' @param landscape Identifier for the landscape used in the DEPONS simulations;
#'   typically set to 'North Sea'.
#' @param crs CRS-object providing the map projection (see \link[sp]{CRS}).
#'
#' @return Returns a DeponsRaster object. The object inherits slots from the
#'   "RasterLayer" class, including "title", which is used for storing the file
#'   name.
#' @seealso  \code{\link{DeponsRaster-class}}
#' @references Nabe-Nielsen, J., van Beest, F. M., Grimm, V., Sibly, R. M.,
#' Teilmann, J., & Thompson, P. M. (2018). Predicting the impacts of
#' anthropogenic disturbances on marine populations. Conservation Letters,
#' 11(5), e12563. \doi{10.1111/conl.12563}
#' @export read.DeponsRaster
read.DeponsRaster <- function(fname, type="NA", landscape="NA",
                              crs="NA") {
  good.tps <- c("food", "patches", "bathymetry", "dtc", "temperature", "salinity", "blocks", "NA")
  if (!type %in% good.tps) stop(paste("type =", type, "not allowed"))
  if (as.character(class(crs))=="CRS") crs <- as.character(crs)
  header <- utils::read.table(fname, nrows=6, header=FALSE)
  names(header) <- c("var", "value")
  header$var <- tolower(header$var)
  n.rows <- header$value[header$var=="nrows"]
  n.cols <- header$value[header$var=="ncols"]
  xl <- header$value[header$var=="xllcorner"]
  yl <- header$value[header$var=="yllcorner"]
  sz <- header$value[header$var=="cellsize"]
  na.val <- header$value[header$var=="nodata_value"]
  obj2 <- new("DeponsRaster", type=type, landscape=landscape, crs=crs,
              ncols=n.cols, nrows=n.rows, xllcorner=xl, yllcorner=yl,
              cellsize=sz, nodata_value=na.val)
  obj2@type <- type
  obj2@landscape <- landscape
  obj2@crs <- crs
  obj2@header <- header
  obj2@ext <- data.frame("xleft"=xl, "ybottom"=yl, "xright"=xl + n.cols*sz,
                    "ytop"=yl + n.rows*sz)
  data <- scan(fname, skip=6)
  data <- ifelse(data==na.val, NA, data)
  if (!all(dim(data)==c(n.rows, n.cols))) base::stop("Data doesn't match header in input")
  obj2@data <- matrix(data, nrow=n.rows, ncol=n.cols, byrow=TRUE)
  return(obj2)
}


setGeneric("plot")

#' @title Plot a DeponsRaster object
#' @description Plot the values in a DeponsRaster object. Porpoisetracks or
#' other kinds of lines, poits etc. can be drawn on top of the plot by adding
#' @import raster
#' @import methods
#' @import sp
#' @aliases plot.DeponsRaster
#' @aliases plot,DeponsRaster,ANY-method
#' @aliases plot,DeponsRaster,DeponsTrack-method
#' @param x \code{DeponsRaster} object
#' @param y A \code{DeponsTrack} object or missing
#' @param trackToPlot Integer indicating which track to plot if the DeponsTrack
#' object contains more than one track. Ignored if \code{y} is missing
#' @param col A color palette, i.e. a vector of n contiguous colors. Reasonable
#' defaults are provided.
#' @param ... Other optional plotting parameters, including 'axes', 'legend',
#' and 'main'.
#' @seealso See method for \code{\link[raster]{plot}} in the \code{raster}
#' package for plotting parameters and \code{\link{plot.DeponsTrack}} for
#' plotting of DeponsRasters cropped to the extent of tracks.
#' @return No return value, called for side effects
#' @importFrom terra crs
#' @importFrom grDevices rainbow
#' @exportMethod plot
setMethod("plot", signature("DeponsRaster", "ANY"),
          function(x, y, col, trackToPlot=1, ...)  {
            # Define plotting parameters
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            dots <- list(...)
            maxpixels <- 500000
            if("maxpixels" %in% names(dots)) maxpixels <- dots$maxpixels
            alpha <- NULL
            if("alpha" %in% names(dots)) alpha <- dots$alpha
            colNA <- NA
            if("colNA" %in% names(dots)) colNA <- dots$colNA
            add <- FALSE
            if("add" %in% names(dots)) add <- dots$add
            useRaster <- TRUE
            if("useRaster" %in% names(dots)) useRaster <- dots$useRaster
            ext <- NULL
            if("ext" %in% names(dots)) ext <- dots$ext
            interpolate <- FALSE
            if("interpolate" %in% names(dots)) interpolate <- dots$interpolate
            nr <- NA
            nc <- NA
            interpolate <- FALSE
            if("interpolate" %in% names(dots)) interpolate <- dots$interpolate
            addfun <- NULL
            if("addfun" %in% names(dots)) addfun <- dots$addfun
            maxnl <- 16
            if("maxnl" %in% names(dots)) maxnl <- dots$maxnl
            npretty <- 0
            if("npretty" %in% names(dots)) npretty <- dots$npretty
            axes <- TRUE
            if("axes" %in% names(dots)) axes <- dots$axes
            legend <- TRUE
            if("legend" %in% names(dots)) legend <- dots$legend
            # Define colours specific or 'type'
            main <- paste(x@landscape, x@type, sep=" - ")
            if("main" %in% names(dots)) main <- dots$main
            # Define colours specific or 'type'
            if(missing(col) && x@type=="bathymetry") {
              tmp.col <- grDevices::rainbow(1000)[501:800]
              col <- c(tmp.col[1:100], rep(tmp.col[101:250], each=5))
            }
            # Use {raster}-package for plotting
            if(x@crs=="NA") {
              crs2 <- terra::crs(as.character(NA))
            } else {
              crs2 <- terra::crs(x@crs)
            }
            rdata <- raster::raster(x=x@data, xmn=x@ext$xleft, xmx=x@ext$xright,
                                    ymn=x@ext$ybottom, ymx=x@ext$ytop,
                                    crs=crs2)
            raster::plot(rdata, col=col, main=main,
                         alpha=alpha, add=add, ext=ext, axes=axes, legend=legend)
            return(invisible(NULL))
          }
)


make.br <- function(template, blocks=NA, blockvals=NULL, NAvalue=-9999,
                    plot=FALSE, fname=NULL, overwrite=FALSE) {
  # the template is another DeponsRaster file with same size and resolution
  if(!inherits(blocks, "list")) stop("Please a list of 'blocks' to update")
  good.class <- function(elem) {
    any(inherits(elem, "matrix")) || any(class(elem)=="SpatialPolygons")
  }
  if(!all(sapply(blocks, FUN=good.class))) {
    stop("All elements in 'blocks' must be of class 'matrix' or
                   'SpatialPolygons'")
  }
  if(missing(blockvals)) blockvals <- 1:(length(blocks)+1)
  if(!inherits(blockvals,"integer")) stop("'blockvals' should be an vector of integers'")
  if(length(blockvals) != length(blocks)+1) stop("Please provide a value for the
      background plus a value for each element in 'blocks'")
  template@data[] <- blockvals[1]  # Set background value
  # Convert each element in the blocks list to a SpatialPolygons element outlining
  # the new block. If it was a matrix of coordinates, use the bounding box to
  # define the new block. The numbers assigned to these blocks are taken from
  # 'blockvals'.
  new.blocks <- list()
  templ.r <- raster::raster(template@data, template@ext$xleft, template@ext$xright,
                           template@ext$ybottom, template@ext$ytop, crs=template@crs)
  templ.r[] <- blockvals[1]
    for(i in 1:length(blocks)) {
      if(any(inherits(blocks[[i]],"SpatialPolygons"))) {
        crs.b <- as.character(blocks[[i]]@proj4string)
        crs.t <- as.character(crs(template))
        if(!(is.na(crs.b)==is.na(crs.t))) {
          warning(paste("CRS of block", i,"and template do not match"))
        } else if(crs.b != crs.t) {
          warning(paste("CRS of block", i,"and template do not match"))
        }
        new.blocks[[i]] <- blocks[[i]]
      }
      if(any(inherits(blocks[[i]],"matrix"))) {
        if(ncol(blocks[[i]])!=2) stop("Matrices defining blocks must have 2 columns")
        bb <- sp::SpatialPoints(blocks[[i]])
        bb <- sp::bbox(bb)
        bb.matr.x <- c(bb[1,1], bb[1,2], bb[1,2], bb[1,1])
        if(min(bb.matr.x)<template@ext$xleft) warning(paste("xmin < raster extent in block", i))
        if(max(bb.matr.x)>template@ext$xright) warning(paste("xmax > raster extent in block", i))
        bb.matr.y <- c(bb[2,1], bb[2,1], bb[2,2], bb[2,2])
        if(min(bb.matr.y)<template@ext$ybottom) warning(paste("ymin < raster extent in block", i))
        if(max(bb.matr.y)>template@ext$ytop) warning(paste("ymax > raster extent in block", i))
        bb.coords <- cbind("x"=bb.matr.x, "y"=bb.matr.y)
        srl <- list(sp::Polygon(bb.coords))
        Srl <- list(sp::Polygons(srl, ID=as.vector(as.character(i))))
        new.blocks[[i]] <- SpatialPolygons(Srl, proj4string=sp::CRS(template@crs))  # ASSUMES correct proj
      }
      out <- raster::rasterize(new.blocks[[i]], templ.r)
      templ.r[!is.na(out)] <- blockvals[i+1]
    }
    # Write the RasterLayer to a file, or plot
    if(plot) plot(templ.r)
    if(!is.null(fname) && !missing(fname)) {
      nc <- nchar(fname)
      if(substr(fname, nc-3, nc)!=".asc") warning("File names should end with '.asc")
      raster::writeRaster(templ.r, filename=fname, format="ascii", overwrite=overwrite)
    }
  invisible(templ.r)
}


setGeneric("make.blocksraster", make.br)


#' @name make.blocksraster
#' @title Makes new file with blocks
#' @description Produces a DeponsRaster object of type='blocks' for use in DEPONS
#' simulations. This allows animals to be counted within specific regions (blocks)
#' of the landscape during the simulation. The new blocks can be specified as
#' either matrices or SpatialPolygons objects. For matrices, the blocks are
#' defined as the smallest rectangle that includes all the specified positions.
#' @aliases make.blocksraster,DeponsRaster-method
#' @param template DeponsRaster object used as template for new blocks file
#' @param blocks list of areas to be used for new blocks. Each item in 'blocks'
#' should be a matrix (with two columns, corresponding to x- and y-coordinates)
#' or a SpatialPolygons object
#' @param blockvals Vector of integer values defining the labels of the new blocks.
#' The first value defines the background value, so the length of 'blockvals'
#' should equal the number of blocks plus 1
#' @param NAvalue Value used for missing data in the output object
#' @param plot If TRUE, the raster block is plotted
#' @param fname Name of the output raster file (character string ending with
#' '.asc'). No file is written to disk if fname is not provided.
#' @param overwrite Whether to replace existing file.
#' @return \code{RasterLayer} object defining different subregions of the
#' landscape where animals should be counted.
#' @note The blocks file should not be modified when running DEPONS
#' simulations using the 'Kattegat' landscape. In this landscape the simulated
#' animals use the blocks file for navigation. Also note that blocks are added
#' to the new blocks raster in the order they are file in the order in which
#' they are listed in 'blocks', so the order mattes if the blocks overlap.
#' @examples
#' #Load file to use as template for new blocks file
#' data("bathymetry")
#'
#' # Make list of blocks to create
#' new.blocks <- list()
#' x <- runif(8, 700000, 760000)
#' y <- runif(8, 6200000, 6300000)
#' new.blocks[[1]] <- cbind(x,y)
#' x <- c(600000, 635000, 670000, 635000)
#' y <- c(6150000, 6200000, 6150000, 6100000)
#' library(sp)
#' srl <- list(Polygon(cbind(x,y)))
#' Srl <- list(Polygons(srl, ID=as.vector("p")))
#' new.blocks[[2]] <- SpatialPolygons(Srl, proj4string=crs(bathymetry))
#'
#' make.blocksraster(bathymetry, new.blocks, plot=TRUE)
#' points(new.blocks[[1]])
#' plot(new.blocks[[2]], add=TRUE)
#'
#' \donttest{
#' the.dir <- tempdir()
#' make.blocksraster(bathymetry, new.blocks, fname=paste0(the.dir, "/test.asc"))
#' }
#' @exportMethod make.blocksraster
setMethod("make.blocksraster", signature("DeponsRaster"), make.br)


setGeneric("bbox", function(obj){})

#' @name bbox
#' @rdname bbox
#' @aliases bbox,DeponsRaster-method
#' @exportMethod bbox
setMethod("bbox", signature("DeponsRaster"),
          function(obj) {
            x <- c(obj@ext$xleft, obj@ext$xright)
            y <- c(obj@ext$ybottom, obj@ext$ytop)
            extremes <- sp::SpatialPoints(cbind(x, y), proj4string=sp::CRS(obj@crs))
            return(sp::bbox(extremes))
          }
)



setGeneric("as.DeponsRaster", function(x){})

#' @title Convert a RasterLayer into a DeponsRaster
#' @name as.DeponsRaster
#' @aliases as.DeponsRaster,RasterLayer-method
#' @description Converts a \code{\link[raster:Raster-class]{RasterLayer}} into a \code{DeponsRaster}.
#' @details Maintains CRS (if defined) and value of NA cells. Currently DEPONS requires a fixed
#' cell size of 400 x 400 m, and cell size is set to this value.
#' @param x A \code{RasterLayer}
#' @return A \code{DeponsRaster}
#' @seealso \code{\link{as.raster}} for converting a DeponsRaster into a RasterLayer
#' @examples
#' data(bathymetry)
#' bathymetry_RasterLayer <- as.raster(bathymetry)
#' bathymetry <- as.DeponsRaster(bathymetry_RasterLayer)
#' @exportMethod as.DeponsRaster
setMethod("as.DeponsRaster", signature("RasterLayer"),
          function(x) {
            y <- new("DeponsRaster",
                     crs = ifelse((!is.na(x@srs) && x@srs != ""), x@srs, "NA"),
                     ncols = x@ncols,
                     nrows = x@nrows,
                     xllcorner = x@extent@xmin,
                     yllcorner = x@extent@ymin,
                     cellsize = 400,
                     nodata_value = x@file@nodatavalue)
            y@data <- matrix(raster::values(x), ncol = y@header[1,2], nrow = y@header[2,2], byrow=T)
            return(y)
          }
)



setGeneric("as.raster", function(x, ...){})

#' @title Convert a DeponsRaster into a RasterLayer
#' @name as.raster
#' @aliases as.raster,DeponsRaster-method
#' @description Converts a \code{DeponsRaster} into a \code{\link[raster:Raster-class]{RasterLayer}} for easier manipulation with common R tools.
#' @details Maintains cell size / resolution, CRS (if defined) and value of NA cells.
#' @param x A \code{DeponsRaster}
#' @return A \code{RasterLayer}
#' @seealso \code{\link{as.DeponsRaster}} for converting a RasterLayer into a DeponsRaster
#' @examples
#' data(bathymetry)
#' y <- as.raster(bathymetry)
#' @exportMethod as.raster

setMethod("as.raster", signature("DeponsRaster"),
          function(x) {
            y <- raster::raster(x@data)
            raster::extent(y) <- matrix(as.numeric(unname(x@ext)), nrow=2, ncol=2)
            if (!is.na(x@crs) && x@crs != "NA") raster::crs(y) <- x@crs
            raster::NAvalue(y) <- x@header[6,2]
            return(y)
          }
)


#'@name interpolate.maps
#'@title Interpolate missing entries in a series of maps
#'@description
#'Interpolates values for missing entries in a series of maps that is to be read by DEPONS. This is intended for map series
#'that in their entirety are assumed to follow a cyclical pattern (e.g., monthly prey or temperature maps, where the complete series covers one year)
#'and therefore uses an interpolation that fits a single-cycle sine curve that seamlessly connects the last to the first map in the series.
#'
#'@details
#'The complete series of maps, consisting of existing and missing maps, is arranged as a chronological stack. For each raster cell
#'in the stack, the prediction function is fit to the existing values, and the missing values are predicted from the function.
#'The fitted function is of the form ‘\eqn{\alpha} * sine(x) + \eqn{\beta} * cosine(x)’, where \eqn{\alpha} & \eqn{\beta} are estimated using a basic linear model. 
#'This is equivalent to ‘Amplitude * sine(x + Phase)’, but is more straightforward to estimate. The resulting curve satisfies the assumption that
#'the data described in the map series should have a single series maximum and minimum, and should loop around seamlessly at end of series.
#'
#'The function requires at least 3 existing maps in the series to interpolate from. Number of total entries and missing entries is arbitrary.
#'
#'The function writes a map file for each map in the series, where existing maps are unchanged and formerly missing maps consist of
#'interpolated values. Created rasters are written in .asc format and named "interpolated_x.asc", where x is the number in the series.
#'The coordinate system is inherited. NA values are set to -9999.
#'
#'If a map is specified in 'supplement', it is created by interpolation in the first round, then
#'the original data are supplemented with this map, and the fitting and interpolation are carried out again for the complete series.
#'This can be helpful if the existing data describe the shape of the full series poorly. For example, if only the first three month of a
#'twelve-month series of maps are given, a sine curve fitted to these may have no existing data describing a reasonable annual maximum or minimum,
#'and estimated values may therefore be unrealistic. In this case it may be useful to estimate a first fit with 'flatten = T' (default),
#'spike the data with the month that is expected to contain the minimum (which has been limited to 0 now), and fit the curve again, providing
#'the function with a more realistic existing minimum to shoot for and thus constraining the amplitude of the curve.
#'
#'@param map.dir Character vector. The directory containing the source files and to which the output will be written.
#'@param map.files Character vector. Complete map series in chronological order, where existing maps are given as file names
#'and missing maps are given as 'NA'. All file formats supported by \code{\link[raster:raster]{raster::raster}} are recognized.
#'@param flatten Logical. Default TRUE. If true, negative interpolated values are set to 0. This prevents the generation of interpolated data
#'that make no sense in context (negative food or temperature values). Since the fitted sine curve is likely to create some negative predictions,
#'it is suggested to always flatten the output.
#'@param supplement Numerical. Default NA. Can be used to specify one missing map (position number in map.files) that is to be used to 'spike'
#'the data for a second round of fitting and prediction (see Details).
#'
#'@returns
#'No return value (called for side effects)
#'
#'@examples
#'\dontrun{
#'## generate incomplete series of map files
#'map1 <- raster::raster(matrix(nrow = 100, ncol = 100))
#'for (i in 10:1) {
#'  map1[1:(i*10),1:(i*10)] <- 10 + i
#'}
#'map3 <- map1 + 10
#'map7 <- map1 - 5
#'maps <- replicate(12, NA, simplify = F)
#'maps[c(1,3,7)] <- c(map1, map3, map7)
#'par(mfrow=c(3,4), mar=c(0,0,0,0))
#'for (i in 1:12) {
#'  if (is.na(maps[i])) plot(0, type='n', axes=FALSE, ann=FALSE)
#'  else raster::plot(maps[[i]], col=rev(rainbow(40, start=0, end=1)),
#'  breaks=c(0:40), legend = F, axes = F)
#'}
#'raster::writeRaster(map1, "map1.asc", format = "ascii", overwrite = T)
#'raster::writeRaster(map3, "map3.asc", format = "ascii", overwrite = T)
#'raster::writeRaster(map7, "map7.asc", format = "ascii", overwrite = T)
#'
#'## interpolate missing maps
#'interpolate.maps(map.dir = getwd(),
#'                 map.files = c("map1.asc", NA, "map3.asc", NA, NA, NA,
#'                 "map7.asc", NA, NA, NA, NA, NA))
#'
#'par(mfrow=c(3,4), mar=c(0,0,0,0))
#'for (i in 1:12) {
#'  the.map <- paste0("interpolated_", i, ".asc")
#'  raster::plot(raster::raster(the.map), col=rev(rainbow(40, start=0, end=1)),
#'  breaks=c(0:40), legend = F, axes = F)
#'  unlink(the.map)
#'}
#'unlink(c("map1.asc", "map3.asc", "map7.asc"))
#'}
#'@export interpolate.maps

interpolate.maps <- function(map.dir, map.files, flatten = T, supplement = NA) {

  if (!dir.exists(map.dir)) stop("Directory does not exist")
  if (length(stats::na.omit(map.files)) < 3) stop("At least 3 existing maps are needed for interpolation")
  if (!is.na(supplement)) {
    if(!is.na(map.files(supplement)) || (supplement > length(map.files))) {
      stop("Map used as supplement in 2nd round must be one that is created by interpolation in first round
        - i.e., an NA position in 'map.files'")
    }
  }

  oldwd <- getwd()
  setwd(map.dir)

  # get raster pars from one existing map
  example.file <- raster::raster(stats::na.omit(map.files)[1])
  xnrow <- example.file@nrows
  xncol <- example.file@ncols
  xcrs <- example.file@srs

  # for each month, if data present, load from file and store as matrix in list, otherwise make empty matrix of same dimensions
  file.list <- list()
  months <- length(map.files)
  for (i in 1:months) {
    if (!is.na(map.files[i])) {
      xmatrix <- as.matrix(raster(map.files[i]))
    } else {
      xmatrix <- matrix(nrow = xnrow, ncol = xncol)
    }
    file.list[[i]] <- xmatrix
  }

  # for each x/y coordinate (cell), make a stack (pillar) of cells through all required months, fit curve through the present data, and predict missing data
  map.interpolation <- function(source) {
    file.list.filled <- source
    progressBar <- utils::txtProgressBar(min = 0, max = xnrow, style = 3)
    for (the.row in 1:xnrow) {
      for (the.col in 1:xncol) {

        pillar <- as.numeric(rep(NA, times=months))

        #for each present month, get the cell value at the current row/col coordinate and store in pillar
        for (month in 1:months) {
          pillar[month] <- source[[month]][the.row, the.col]
        }
        if (any(!is.na(pillar))) {
          pillar <- data.frame(c(1:months), unlist(pillar))
          colnames(pillar) <- c("month", "value")
          # fit the sine curve to the pillar and predict missing values from the fitted function
          if (length(which(!is.na(pillar$value))) > 2) {  # only fit curve if there are at least 3 months with present data for this cell
            curve <- stats::lm(pillar$value ~ sin((2 * pi / months) * pillar$month) + cos((2 * pi / months) * pillar$month))
            predicted <- predict(curve, pillar)
          } else predicted <- as.numeric(rep(NA, times=months))

          # fill NAs in the pillar with predicted values
          pillar$value[which(is.na(pillar$value))] <- predicted[which(is.na(pillar$value))]

          # fill in the cell in each map in the list copy (file.list.filled) with pillar values
          for (month in 1:months)  {
            file.list.filled[[month]][the.row, the.col] <- pillar$value[month]
          }
        }
      }
      utils::setTxtProgressBar(progressBar, value = the.row)
    }

    if (flatten) { # if flatten = T, set negative interpolated values to 0
      for (month in 1:months) {
        file.list.filled[[month]][file.list.filled[[month]] < 0] <- 0
      }
    }
    return(file.list.filled)
  }

  message("Interpolating...")
  file.list.filled <- map.interpolation(file.list) # base interpolation
  # if required: second interpolation using original maps "spiked" with one interpolated month
  if (!is.na(supplement)) {
    file.list[[supplement]] <- file.list.filled[[supplement]]
    message("Interpolating again with supplemented data...")
    file.list.filled <- map.interpolation(file.list)
  }

  # write maps
  message("\nWriting maps...")
  progressBar <- utils::txtProgressBar(min = 0, max = months, style = 3)
  for (month in 1:months) {
    new.matrix <- file.list.filled[[month]]
    new.matrix[is.na(new.matrix)] <- -9999
    new.map <- raster::raster(new.matrix, crs = xcrs)
    extent(new.map) <- raster::extent(example.file)
    raster::writeRaster(new.map, paste0("interpolated_",month,".asc"), NAflag = -9999, format = "ascii", overwrite = T)
    utils::setTxtProgressBar(progressBar, value = month)
  }

  setwd(oldwd)
}
