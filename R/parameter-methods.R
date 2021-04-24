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
#' the main DEPONS model window. See TRACE document for details regarding the
#' parameters in the model: \url{https://github.com/jacobnabe/DEPONS}. It is strongly recommended that the parameter
#' list is stored with the simulation output.
#' @return Data frame containing all parameters used in a specific
#' simulation
#' @examples \dontrun{
#' # Parameters read from file created by DEPONS run in interactive mode
#' the.file <- "/Applications/DEPONS 2.1/DEPONS/DEPONS.rs/parameters.xml"
#' pfile <- read.DeponsParam(the.file)
#' }
#' @export read.DeponsParam
read.DeponsParam <- function(fname) {
  if(!(file.exists(fname))) stop(paste0("The file ", fname, " does not exist"))
  tmp <- xml2::read_xml(fname)
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
  default.param <- c("Euse", "tdisp", "b0", "b1", "b2", "b3", "simYears",
                     "Elact", "tmating", "bycatchProb", "alpha_hat", "tmature",
                     "dmax_mov", "R1", "rR", "tgest", "R2", "rS", "wdisp", "rU",
                     "trackedPorpoiseCount", "Einit", "tnurs", "tdeter",
                     "randomSeed", "ships", "beta_hat", "PSM_angle", "ddisp",
                     "PSM_tol", "RT", "dispersal", "PSM_dist", "Ewarm", "wmin",
                     "PSM_log", "beta", "dmax_deter", "q1", "porpoiseCount",
                     "Psi_deter", "debug", "c", "turbines", "h", "k", "tmaxage",
                     "a0", "a1", "a2", "Umin", "wrapBorderHomo", "landscape")
  param.nms <- all.param$param
  if(!all(param.nms %in% default.param)) {
    nm.not.default <- param.nms[which(!(param.nms %in% default.param))]
    nm2 <- paste(nm.not.default, collapse=", ")
    warning(paste("Following parameter(s) are not derault:", nm2))
  }
  if(!all(default.param %in% param.nms)) {
    defaults.missing <- default.param[which(!(default.param %in% param.nms))]
    def2 <- paste(defaults.missing, collapse=", ")
    warning(paste("Following default parameter(s) are missing:", def2))
  }
  return(all.param)
}
