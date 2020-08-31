# DEPONS2R -- Accessor functions


setGeneric("crs", function(x) { return(sp::CRS(x@crs)) })

#' @name crs
#' @aliases crs,DeponsTrack-method
#' @aliases crs<-,DeponsTrack-method
#' @title Get or set map projection
#' @param x Object of class {code{DeponsRaster}} or \code{DeponsTrack}.
#' @exportMethod crs
setMethod("crs", signature("DeponsTrack"),
          function(x) {
            return(sp::CRS(x@crs))
          }
)


#' @name crs<-
#' @rdname crs
#' @param x Object of class class {code{DeponsRaster}} or \code{DeponsTrack}
#' @param value \code{\link{proj4string}} identifying the map projection
#' @exportMethod crs<-
setMethod("crs<-", "DeponsTrack", function(x, value) {
  x@crs <- value
  validObject(x)
  x
})


setGeneric("title<-", function(x, value) {
  x@title <- value
  validObject(x)
  x
})


#' @name title<-
#' @rdname title
#' @param x Object of class \code{DeponsTrack}
#' @param value Title of the object (text string)
#' @exportMethod title<-
setMethod("title<-", "DeponsTrack", function(x, value) {
  x@title <- value
  validObject(x)
  x
})


setGeneric("title", function(x, value) { return(x@title) })

#' @name title
#' @title Get or set the title of DeponsTrack objects
#' @aliases title,DeponsTrack-method
#' @aliases title<-,DeponsTrack-method
#' @rdname title
#' @param value Character string
#' @param x Object of class \code{DeponsTrack}.
#' @exportMethod title
setMethod("title", signature=("DeponsTrack"), function(x, value) { return(x@title) })


#' @name title<-
#' @rdname title
#' @param x Object of class \code{DeponsDyn}
#' @param value Title of the object (text string)
#' @exportMethod title<-
setMethod("title<-", "DeponsDyn", function(x, value) {
  x@title <- value
  validObject(x)
  x
})


#' @name title
#' @title Get or set the title of DeponsDyn objects
#' @aliases title,DeponsDyn-method
#' @aliases title<-,DeponsDyn-method
#' @rdname title
#' @param value Character string
#' @param x Object of class \code{DeponsDyn}.
#' @exportMethod title
setMethod("title", signature=("DeponsDyn"), function(x, value) { return(x@title) })



setGeneric("landscape<-", function(x, value) {
  x@landscape <- value
  validObject(x)
  x
})


#' @name landscape<-
#' @rdname landscape
#' @param x Object of class \code{DeponsTrack}
#' @param value landscape of the object (text string)
#' @exportMethod landscape<-
setMethod("landscape<-", "DeponsTrack", function(x, value) {
  x@landscape <- value
  validObject(x)
  x
})


setGeneric("landscape", function(x, value) { return(x@landscape) })

#' @name landscape
#' @title Get or set the landscape name of DeponsTrack objects
#' @aliases landscape,DeponsTrack-method
#' @aliases landscape<-,DeponsTrack-method
#' @rdname landscape
#' @param value Character string
#' @param x Object of class \code{DeponsTrack}.
#' @exportMethod landscape
setMethod("landscape", signature=("DeponsTrack"), function(x, value) { return(x@landscape) })


#' @name landscape<-
#' @rdname landscape
#' @param x Object of class \code{DeponsDyn}
#' @param value landscape of the object (text string)
#' @exportMethod landscape<-
setMethod("landscape<-", "DeponsDyn", function(x, value) {
  x@landscape <- value
  validObject(x)
  x
})


#' @name landscape
#' @title Get or set the landscape of DeponsDyn objects
#' @aliases landscape,DeponsDyn-method
#' @aliases landscape<-,DeponsDyn-method
#' @rdname landscape
#' @param value Character string
#' @param x Object of class \code{DeponsDyn}.
#' @exportMethod landscape
setMethod("landscape", signature=("DeponsDyn"), function(x, value) { return(x@landscape) })
