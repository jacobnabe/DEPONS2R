\docType{data}
\name{porpoisetrack}
\alias{porpoisetrack}
\title{Simulated porpoise track}
\format{
DeponsTrack
}
\description{
An object with five elements: \code{title}, \code{landscape},
\code{simtime}, \code{crs}, and \code{tracks}. The \code{crs} stores information
about the map projection used ("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs").
The \code{tracks} element is a list of objects of class
\code{\link[sp]{SpatialPointsDataFrame}}, each ofwhich corresponds to one
simulated animal. \code{simtime} is the simulation date.
}
\seealso{
\code{\link{DeponsTrack-class}}
}
\keyword{datasets}
