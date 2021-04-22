
\docType{data}
\name{shipdata}
\alias{shipdata}
\title{Ships on  routes through Kattegat}
\format{
DeponsShips
}
\description{
The standard ship routes data included with DEPONS, including
the ship routes and ships used in the study by Nabe-Nielsen et al. (2014).
The coordinates defining the routes use 'grid units' (default for ship routes
in DEPONS 2.1), and must be converted before plotting on a map.
The coordinates defining the routes use the UTM zone 32 projection, (EPSG:32632).
The corresponding proj4string is "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
(see \url{https://epsg.io/32632}).
}
\references{
Nabe-Nielsen, J., Sibly, R. M., Tougaard, J., Teilmann, J., &
Sveegaard, S. (2014). Effects of noise and by-catch on a Danish harbour
porpoise population. Ecological Modelling, 272, 242–251.
\doi{10.1016/j.ecolmodel.2013.09.025}
}
\seealso{
\code{\link{DeponsShips-class}}
}
\keyword{datasets}
