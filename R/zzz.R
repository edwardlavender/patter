.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("This is {patter} v.", utils::packageVersion("patter"), ". For an overview, see ?patter For support, contact edward.lavender@eawag.ch."))
}
