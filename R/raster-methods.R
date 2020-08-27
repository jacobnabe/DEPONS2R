
# Author: Jacob Nabe-Nielsen
# Date: 1 August 2020
# Version 0.9
# Licence GPL v3
# Description: Methods and classes for reading and summarizing DEPONS raster
#   objects


#' @title  DeponsRaster-class and methods
#' @description Classes and methods for manipulating and plotting DEPONS raster
#' landscapes.
#' @slot type Character. Identifies the kind of data stored in the raster; should
#' be 'food', 'patches', bathymetry', 'dtc', 'salinity', 'blocks' or 'NA'.
#' @slot landscape Character  Identifier for the landscape used in the DEPONS
#' simulations. The landscapes 'DanTysk', 'Gemini', 'Kattegat', 'North Sea',
#' 'Homogeneous', and 'User defined' are distributed with the DEPONS model.
#' @slot crs Object of class "CRS", i.e. the coordinate reference system. This
#' value is called 'proj4string' in spatial objects (from "RasterLayer").
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
#' @seealso \code{\link{read.DeponsRaster}}
#' @note Use data(bathymetry) to load example data.
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


setMethod("show", "DeponsRaster",
          function(object) {
            cat("class:\t", "DeponsRaster \n")
            cat("type:\t", object@type, "\n")
            cat("landsc:\t", object@landscape, "\n")
            cat("crs:\t", object@crs, "\n")
            cat("extent:\t ", object@ext$xleft, ", ", object@ext$ybottom, ", ",
                object@ext$xright, ", ", object@ext$ytop,
                " (xleft, ybottom, xright, ytop) \n", sep="")
            cat("dim: \t", as.character(nrow(object@data), ""))
            cat(" x", as.character(ncol(object@data)))
            cat(" (nrow, ncol) \n")
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
#' 11(5), e12563. \url{https://doi.org/10.1111/conl.12563}
#' @export read.DeponsRaster
read.DeponsRaster <- function(fname, type="NA", landscape="NA",
                              crs="NA") {
  good.tps <- c("food", "patches", "bathymetry", "dtc", "salinity", "blocks", "NA")
  if (!type %in% good.tps) stop(paste("type =", type, "not allowed"))
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
#' \code{add=TRUE} to the call.
#' @aliases plot.DeponsRaster
#' @aliases plot,DeponsRaster,ANY-method
#' @aliases plot,DeponsRaster,DeponsTrack-method
#' @param x \code{DeponsRaster} object
#' @param y A \code{DeponsTrack} object or missing
#' @param trackToPlot Integer indicating which track to plot if the DeponsTrack
#' object contains more than one track. Ignored if \code{y} is missing
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < ncell(x), sampleRegular is used before plotting.
#' @param col A color palette, i.e. a vector of n contiguous colors. Reasonable
#' defaults are provided.
#' @param alpha Number between 0 and 1 to set transparency. 0 is entirely
#' transparent, 1 is not transparent (NULL is equivalent to 1)
#' @param colNA The color to use for the background (default is transparent)
#' @param add Logical. Whether to add map to the current plot
#' @param ext An extent object allowing to plot only part of the map
#' @param useRaster If TRUE, the rasterImage function is used for plotting.
#' Otherwise the image function is used
#' @param interpolate Logical. Should the image be interpolated (smoothed)?
#' Only used when useRaster = TRUE
#' @param addfun Function to add additional items such as points or polygons
#' to the plot
#' @param nc Not used for plotting DeponsRaster objects
#' @param nr Not used for plotting DeponsRaster objects
#' @param maxnl Not used for plotting DeponsRaster objects
#' @param main Character. Plot title
#' @param npretty Integer. Number of decimals for pretty lables on the axes
#' @param axes Whether to plot tick marks
#' @param legend Whether to plot the colour legend
#' @param ... Other optional plotting parameters
#' @examples
#' data("bathymetry")
#' plot(bathymetry)
#' data("coastline")
#' plot(coastline, add=TRUE, col="lightyellow2")
#'
#' plot(bathymetry, axes=FALSE, legend=FALSE, main="Simulated porpoise track")
#' data("porpoisetrack")
#' plot(porpoisetrack, add=TRUE)
#' @seealso See method for \code{\link[raster]{plot}} in the
#' \code{raster} package for details.
#' @exportMethod plot
setMethod("plot", signature("DeponsRaster", "ANY"),
          function(x, y, maxpixels=500000, col, alpha=NULL, colNA=NA, add=FALSE,
                   ext=NULL, useRaster=TRUE, interpolate=FALSE, addfun=NULL,
                   nc, nr, maxnl=16, main, npretty=0, axes=TRUE,
                   legend=TRUE, trackToPlot=1, ...)  {

            if (missing(main)) {
              main <- paste(x@landscape, x@type, sep=" - ")
            }
            # Define colours specific or 'type'
            if(missing(col) && x@type=="bathymetry") {
              tmp.col <- grDevices::rainbow(1000)[501:800]
              col <- c(tmp.col[1:100], rep(tmp.col[101:250], each=5))
            }
            # Use {raster}-package for plotting
            if(x@crs=="NA") {
              crs2 <- sp::CRS(as.character(NA))
            } else {
              crs2 <- sp::CRS(x@crs)
            }
            rdata <- raster::raster(x=x@data, xmn=x@ext$xleft, xmx=x@ext$xright,
                                    ymn=x@ext$ybottom, ymx=x@ext$ytop,
                                    crs=crs2)
            raster::plot(rdata, col=col, main=main,
                         alpha=alpha, add=add, ext=ext, axes=axes, legend=legend)
          }
)



as.DeponsRaster <- function(obj, type="NA", landscape="NA", nodata_value=-9999) {
  the.res <- res(obj)
  if(!identical(the.res[1], the.res[2])) stop("x-resolution must be
                                                        equal to y-resolution")
  if(the.res[1]!=400) warning("Resolution not equal to 400 m")
  new.dr <- new("DeponsRaster", type=type, landscape=landscape,
                crs="NA", ncols=dim(obj)[2], nrows=dim(obj)[1],
                xllcorner=extent(obj)@xmin, yllcorner=extent(obj)@ymin,
                cellsize=the.res[1], nodata_value=nodata_value
  )
  new.dr@data <- as.matrix(obj)
  return(new.dr)
}


#' 'as.DeponsRaster'
#' @title Convert RasterLayer objects to DeponsRaster objects
#' @param obj RasterLayer object (see \code{\link[raster]{raster}})
#' @param type Landscape type (see \code{\link{DeponsRaster-class}})
#' @param landscape The spatial region of the landscape
#' @param nodata_value Value used for missing data in the output object
#' @exportMethod as.DeponsRaster
setMethod("as.DeponsRaster", signature("RasterLayer"), as.DeponsRaster)


setGeneric("crs")

#' Get map projection
#' @name crs
#' @description Get the map projection (also known as coordinate reference
#' system, crs) of DeponsRaster and DeponsTrack objects. For {sp} objects the
#' text string defining the crs is called the \code{\link[sp]{proj4string}}.
#' @aliases crs,DeponsRaster-method
#' @aliases crs,DeponsTrack-method
#' @param x Object of class {code{DeponsRaster}} or \code{DeponsTrack}.
#' @exportMethod crs
setMethod("crs", signature("DeponsRaster"),
          function(x) {
            return(sp::CRS(x@crs))
          }
)



make.br <- function(template, blocks=NA, blockvals=NULL) {
  # the template is another DeponsRaster file with same size and resolution
  if(class(blocks)!="list") stop("Please a list of 'blocks' to update")
  good.class <- function(elem) {
    any(class(elem)=="matrix") || any(class(elem)=="SpatialPoints")
  }
  if(!all(sapply(blocks, FUN=good.class))) {
    stop("All elements in 'blocks' must be of class 'matrix' or
                   'SpatialPoints'")
  }
  if(missing(blockvals)) blockvals <- 1:(length(blocks)+1)
  if(class(blockvals)!="integer") stop("'blockvals' should be an vector of integers'")
  if(length(blockvals) != length(blocks)+1) stop("Please provide a value for the
      background plus a value for each element in 'blocks'")
  template[] <- blockvals[1]  # Set background value
  # Convert each element in the blocks list to a SpatialPoints element outlining
  # the new block. If it was a matrix of coordinates, use the bounding box to
  # define the new block. The numbers assigned to these blocks are taken from
  # 'blockvals'.
  new.blocks <- list()
  templ.ext <- raster::extent(template)
  for(i in 1:length(blocks)) {
    if(any(class(blocks[[i]])=="SpatialPoints")) {
      if(as.character(new.blocks[[i]]@proj4string)!=as.character(crs(template))) {
        warning(paste("CRS of block", i,"and template do not match"))
      }
      new.blocks[[i]] <- blocks[[i]]
    }
    if(any(class(blocks[[i]])=="matrix")) {
      if(ncol(blocks[[i]])!=2) stop("Matrices defining blocks must have 2 columns")
      bb <- sp::SpatialPoints(blocks[[i]])
      bb <- sp::bbox(bb)
      bb.matr.x <- c(bb[1,1], bb[1,2], bb[1,2], bb[1,1])
      if(min(bb.matr.x)<xmin(templ.ext)) warning(paste("xmin < raster extent in block", i))
      if(max(bb.matr.x)>xmax(templ.ext)) warning(paste("xmax > raster extent in block", i))
      bb.matr.y <- c(bb[2,1], bb[2,1], bb[2,2], bb[2,2])
      if(min(bb.matr.y)<ymin(templ.ext)) warning(paste("ymin < raster extent in block", i))
      if(max(bb.matr.y)>ymax(templ.ext)) warning(paste("ymax > raster extent in block", i))
      bb.coords <- cbind("x"=bb.matr.x, "y"=bb.matr.y)
      srl <- list(sp::Polygon(bb.coords))
      Srl <- list(sp::Polygons(srl, ID=as.vector(as.character(i))))
      new.blocks[[i]] <- SpatialPolygons(Srl, proj4string=template@crs)  # ASSUMES correct proj
    }
    out <- raster::rasterize(new.blocks[[i]], template)
    # Can't get 'update=TRUE' to work. Instead update template based on 'out'
    template[!is.na(out)] <- blockvals[i+1]
  }
  return(template)
}


setGeneric("make.blocksraster", make.br)


#' 'make.blocksraster'
#' @title Create a new blocks file
#' @param template DeponsRaster object used as template for new blocks file
#' @param blocks list Areas to be used for new blocks. Each item in 'blocks' should
#' be a matrix (with two columns, corresponding to x- and y-coordinates) or of
#' class SpatialPoints.
#' @param blockvals Vector of integer values defining the labels of the new blocks.
#' The first value defines the background value, so the length of 'blockvals'
#' should equal the number of blocks plus 1.
#' @param crs Value used for missing data in the output object
#' @exportMethod make.blocksraster
setMethod("make.blocksraster", signature("DeponsRaster"), make.br)


# MAKE BLOCKS FILE

# bb.file <- "/Applications/DEPONS 2.1/DEPONS/data/Kattegat/blocks.asc"
# the.crs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
# x <- read.DeponsRaster(bb.file, crs=the.crs)
# template <- raster::raster(x=x@data, xmn=x@ext$xleft, xmx=x@ext$xright,
#                            ymn=x@ext$ybottom, ymx=x@ext$ytop, crs=the.crs)
#
# x <- c(700000, 700000, 760000, 760000)
# y <- c(6200000, 6300000, 6300000, 6200000)
# xy.1 <- cbind(x,y)
# class(xy.1)
# x <- c(600000, 600000, 670000, 670000)
# y <- c(6100000, 6200000, 6200000, 6100000)
# xy.2 <- cbind(x,y)
# blocks <- list(xy.1, xy.2)
# ##make another blocks list
#
# library(sp)
# library(raster)
#
# ttt <- make.blocksraster(template, blocks, crs=the.crs, blockvals=c(4:6))
# dim(ttt)
# summary(ttt)
# plot(ttt)
# class(ttt)
# uuu <- as.matrix(ttt)
# dim(uuu)
# summary(uuu)
#
