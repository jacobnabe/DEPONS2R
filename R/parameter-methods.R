# Methods for reading and summarizing parameter lists from "parameters.xml" files


#' @name read.DeponsParam
#' @title Read simulation parameters
#' @description Read the parameters that were used for running a specific DEPONS
#' simulation
#' @param fname Name of the XML file (character) that contains the parameter
#' list used for running a DEPONS simulation. The name includes the path to the
#' directory if this is not the current working directory.
#' @details The parameter file can be generated from within DEPONS by pressing the
#' 'Save' icon after modifying the user settings on the 'Parameters' tab within
#' the main DEPONS model window. It is strongly recommended that the parameter
#' list is stored with the simulation output.
#' @examples \dontrun{
#' the.file <- "/Applications/DEPONS 2.1/DEPONS/DEPONS.rs/parameters.xml"
#' pfile <- read.DeponsParam
#' pfile
#'
#' # Store with simulation output
#' all.sim.out <- list(porpoisebdyn, pfile)
#' save(all.sim.out, file="AllSimOut_yyyy-mm-dd.RData")
#' }
#' @export read.DeponsParam
read.DeponsParam <- function(fname) {
  tmp <- xml2::read_xml("parameters.xml")
  recs2 <- xml2::xml_find_all(tmp, "//parameter")
  # the xml_text function doesn't work as it shoul, using grep instead
  all.param <- data.frame()
  for(i in 1:length(recs2)) {
    uuu <- as.character(recs2[[i]])
    pnm.start <- as.integer(regexpr("parameter name=", uuu)) + 16
    pnm.end <- as.integer(regexpr("displayName", uuu)) - 3
    pnm <- substr(uuu, pnm.start, pnm.end)
    pval.start <- as.integer(regexpr("defaultValue=", uuu)) + 14
    pval.end <- as.integer(regexpr("isReadOnly", uuu)) - 3
    pval <- substr(uuu, pval.start, pval.end)
    one.line <- data.frame("parameter"=pnm, "value"=pval)
    all.param <- rbind(all.param, one.line)
  }
}
