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
  msg_startup <- paste0("This is {patter} v.", utils::packageVersion("patter"), ". For an overview, see `?patter`. For support, contact edward.lavender@eawag.ch.")
  msg_linux   <- ""
  if (grepl("linux", tolower(Sys.info()["sysname"]))) {
    msg_linux   <- "WARNING: On Linux, {patter} can be used but you cannot simultaneously use geospatial routines in R and Julia. Thus, you can only call `library(terra)` or `terra::foo()` and use {patter} routines that exploit {terra} and other geospatial packages in R sessions that are not connected to a Julia session (via `julia_connect()`). Set the map in Julia using a file path (via `set_map()`). Check the {patter} function documentation for supported options."
  }
  packageStartupMessage(paste(msg_startup, msg_linux))
}
