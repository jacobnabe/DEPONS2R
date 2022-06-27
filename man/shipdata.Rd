\docType{data}
\name{shipdata}
\alias{shipdata}
\title{Hypothetical ships on  routes through Kattegat}
\format{
DeponsShips

data.frame
}
\description{
The ship routes and ships used in the study by Nabe-Nielsen et
al. (2014). The fix points that define the routes use the UTM zone 32
projection, (EPSG:32632; see \url{https://epsg.io/32632}).

The definitions of the ships has been modified since earlier versions of
DEPONS (i.e. 2.1 and erlier) in that it now includes ship length, type, and
speed (in knots). These are used for calculating the sound source level
(following McGilliwray)

Automatic identification system (AIS) data for three ships
in Kattegat and the Western Baltic from 20 Dec 2015. The data set includes
the variables id (the Maritime Mobile Service Identity number), time,
speed (in knots), type, length (in meters), x and y (which provide the
coordinates of the ship at a given time. The coordinates use the UTM zone 32
projection (CRS = "+proj=utm +zone=32 +units=m +no_defs +datum=WGS84"). Data
were downloaded from the Danish Maritime Authority web page
(\url{https://dma.dk}).
}
\references{
MacGillivray A & de Jong C (2021). A Reference Spectrum Model for Estimating
Source Levels of Marine Shipping Based on Automated Identification System
Data. J Mar Sci Eng 9:369 . \doi{10.3390/jmse9040369}

Nabe-Nielsen, J., Sibly, R. M., Tougaard, J., Teilmann, J., &
Sveegaard, S. (2014). Effects of noise and by-catch on a Danish harbour
porpoise population. Ecological Modelling, 272, 242–251.
\doi{10.1016/j.ecolmodel.2013.09.025}
}
\seealso{
\code{\link{DeponsShips-class}}
}
\keyword{datasets}
