# Methods for reading and summarizing parameter lists from "parameters.xml" files


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

