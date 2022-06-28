# Testing functionality of functions for handling ships and routes

# library(DEPONS2R)
data(aisdata)
depons.ais <- ais.to.DeponsShips(data=aisdata, bathymetry,
                                 startday="2015-12-20", endday="2015-12-20")
for (i in 1:length(routes(depons.ais))) {
	ttt <- routes(depons.ais)[[i]]
	ttt.pause <- sum(ttt$pause)
	if(nrow(ttt)+ttt.pause != 49) stop("ais.to.DeponsShips w startday=endday: rows+pauses should be 49")
}


aisdata2 <- aisdata
aisdata2$time <- as.character(as.POSIXct(aisdata$time)+300)
depons.ais2 <- ais.to.DeponsShips(data=aisdata2, bathymetry,
                                  startday="2015-12-20", endday="2015-12-21")
for (i in 1:length(routes(depons.ais2))) {
	ttt <- routes(depons.ais2)[[i]]
	ttt.pause <- sum(ttt$pause)
	if(nrow(ttt)+ttt.pause != 97) stop("ais.to.DeponsShips w startday=endday: rows+pauses should be 49+49=97")
}

