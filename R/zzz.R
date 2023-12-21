.onLoad <- function(libname, pkgname) {
  # Collect options
  op <- options()
  # Define patter options
  op_patter <- list(patter.verbose = TRUE)
  # Set necessary patter options
  bool <- !(names(op_patter) %in% names(op))
  if (any(bool)) {
    options(op_patter[bool])
  }
  invisible(NULL)
}

.onUnload <- function(libpath){
  options("patter.verbose" = NULL)
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("This is {patter} v.", utils::packageVersion("patter"), ". For an overview, see `?patter`. For support, contact edward.lavender@eawag.ch."))
}
