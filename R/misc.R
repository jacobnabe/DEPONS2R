# Notes regarding development of the DEPONS2R package

# Enable plotting of landscapes, including clipped border
# Enable manipulation and plotting of ship tracks
# Make plots of sound transmission loss
# Plot movement tracks
# Reproduce population effects plots from paper



# Notes about development of packages
# • Never use depends() or library()

# old <- options(stringsAsFactors = FALSE)
# on.exit(options(old), add = TRUE)

# Collate controls the order in which R files are sourced. This only matters if
# your code has side-effects; most commonly because you’re using S4.


# devtools::use_package(raster)
# devtools::use_package(methods)
# devtools::use_package(sp)
# roxygen2::update_collate('.')

# rm(list = c("DeponsRaster"))
# rm(list = c(".__T__print:base", ".__T__summary:base"))
# roxygen2::roxygenize()  # for creating namespace
# devtools::document()  # Make rd files based on roxygen comments.



# code for reading and writing example data
#
# proj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
# porpoisetrack <- read.DeponsTrack(fname=fn, title="Track simulated using DEPONS 2.0", crs=CRS(proj))
# class(porpoisetrack)
  # save(porpoisetrack, file="porpoisetrack.RData", compress=TRUE)
#

# class(porpoisetrack)
# plot(porpoisetrack)



#####

# MAKE track FILE
# fname <- "/Applications/DEPONS 2.1/DEPONS/RandomPorpoise.2020.Aug.19.11_28_36.csv"
# porpoisetrack <- read.DeponsTrack(fname, title="Porpoise track simulated with DEPONS 2.1",
#                                 crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
# )
# save(porpoisetrack, file="porpoisetrack.RData", compress="xz")

#####

# MAKE bathymetry FILE
# fname <- "/Applications/DEPONS 2.1/DEPONS/data/Kattegat/bathy.asc"
# bathymetry <- read.DeponsRaster(fname,
#                                 crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs",
#                                 type="bathymetry", landscape="Kattegat"
# )
# save(bathymetry, file="bathymetry.RData", compress="xz")


# MAKE bathymetry FILE
# fname <- "/Applications/DEPONS 2.1/DEPONS/data/NorthSea/bathy.asc"
# bathymetry <- read.DeponsRaster(fname,
#                                 crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs",
#                                 type="bathymetry", landscape="North Sea"
# )
# save(bathymetry, file="bathymetry.RData", compress="xz")


