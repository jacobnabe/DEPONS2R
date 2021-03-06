% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ships.methods.R
\name{write,DeponsShips-method}
\alias{write,DeponsShips-method}
\alias{write.DeponsShips}
\title{Writing DEPONS ship files}
\usage{
\S4method{write}{DeponsShips}(x, file)
}
\arguments{
\item{x}{Name of the DeponsShips object to be exported}

\item{file}{Name of the file (character) that defines the ship routes}
}
\value{
No return value, called for side effects
}
\description{
Function  for writing a json-file for controlling
how ship agents behave in DEPONS. Ships move along pre-defined routes in 30-min
time steps. The routes are defined by the fix-points provided in the
json file, and the geographic projection is assumed to match that of the
landscape. The projection is not stored as part of the json file.
}
