
# Author: Jacob Nabe-Nielsen
# Date: 1 August 2020
# Version 0.9
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
#'is provided as a \code{\link[sp]{proj4string}} text string.
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
#' @exportMethod summary
setMethod("summary", "DeponsRaster",
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
  if (class(crs)=="CRS") crs <- as.character(crs)
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
#' \donttest{
#' data("bathymetry")
#' plot(bathymetry)
#' data("coastline")
#' library(rgdal)
#' # Change projection of coastline to match that of bathymetry data
#' coastline2 <- spTransform(coastline, crs(bathymetry))
#' plot(coastline2, add=TRUE, col="lightyellow2")
#' }
#' text(512000, 6240000, 'Denmark')
#' text(800000, 6300000, 'Sweden')
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
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
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


setGeneric("crs")

#' Get or set map projection
#' @name crs
#' @description Get or set the map projection (also known as coordinate reference
#' system, crs) of DeponsRaster and DeponsTrack objects. For {sp} objects the
#' text string defining the crs is called the \code{\link[sp]{proj4string}}.
#' @aliases crs,DeponsRaster-method
#' @aliases crs,DeponsTrack-method
#' @param x Object of class {code{DeponsRaster}}.
#' @exportMethod crs
setMethod("crs", signature("DeponsRaster"),
          function(x) {
            return(sp::CRS(x@crs))
          }
)


make.br <- function(template, blocks=NA, blockvals=NULL, NAvalue=-9999,
                    plot=FALSE, fname=NULL, overwrite=FALSE) {
  # the template is another DeponsRaster file with same size and resolution
  if(class(blocks)!="list") stop("Please a list of 'blocks' to update")
  good.class <- function(elem) {
    any(class(elem)=="matrix") || any(class(elem)=="SpatialPolygons")
  }
  if(!all(sapply(blocks, FUN=good.class))) {
    stop("All elements in 'blocks' must be of class 'matrix' or
                   'SpatialPolygons'")
  }
  if(missing(blockvals)) blockvals <- 1:(length(blocks)+1)
  if(class(blockvals)!="integer") stop("'blockvals' should be an vector of integers'")
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
      if(any(class(blocks[[i]])=="SpatialPolygons")) {
        crs.b <- as.character(blocks[[i]]@proj4string)
        crs.t <- as.character(crs(template))
        if(!(is.na(crs.b)==is.na(crs.t))) {
          warning(paste("CRS of block", i,"and template do not match"))
        } else if(crs.b != crs.t) {
          warning(paste("CRS of block", i,"and template do not match"))
        }
        new.blocks[[i]] <- blocks[[i]]
      }
      if(any(class(blocks[[i]])=="matrix")) {
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




