\docType{data}
\name{bathymetry}
\alias{bathymetry}
\title{Bathymetry of the Kattegat area}
\format{
DeponsRaster
}
\description{
The standard bathymetry file for Kattegat which is used in DEPONS
simulations. It is based on a raster file with 1000 rows and 600 columns
where each grid cell corresponds to 400 m x 400 m. Cells on land are
assigned a missing data value of -9999.

The Kattegat landscapes use the UTM zone 32 projection, (EPSG:32632) as in
the study by Nabe-Nielsen et al (2014). The corresponding proj4string is
"+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
(see \url{https://epsg.io/32632}).
}
\references{
Nabe-Nielsen, J., Sibly, R. M., Tougaard, J., Teilmann, J., &
Sveegaard, S. (2014). Effects of noise and by-catch on a Danish harbour
porpoise population. Ecological Modelling, 272, 242–251.
\url{https://doi.org/10.1016/j.ecolmodel.2013.09.025}
}
\seealso{
\code{\link{DeponsRaster-class}}
}
\keyword{datasets}
