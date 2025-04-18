.onLoad <- function(libname, pkgname) {
  # Collect options
  op <- options()
  # Define patter options
  patter.verbose <- TRUE
  patter.progress <- ifelse(requireNamespace("pbapply", quietly = TRUE) &&
                              pbapply::pboptions()$type == "none", FALSE, TRUE)
  op_patter <- list(patter.verbose = patter.verbose,
                    patter.progress = patter.progress)
  # Set necessary patter options
  bool <- !(names(op_patter) %in% names(op))
  if (any(bool)) {
    options(op_patter[bool])
  }
  invisible(NULL)
}

.onUnload <- function(libpath){
  options("patter.verbose" = NULL,
          "patter.progress" = NULL)
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  msg_startup <- paste0("This is {patter} v.", utils::packageVersion("patter"), ". For an overview, see `?patter`. For support, raise an issue at https://github.com/edwardlavender/patter/issues.")
  msg_linux   <- ""
  if (os_linux()) {
    msg_linux   <- "WARNING: On Linux, {patter} can be used but you cannot simultaneously use geospatial routines in `R` and `Julia`. Thus, you can only call `library(terra)` or `terra::foo()` and use {patter} routines that exploit {terra} and other geospatial packages in `R` sessions that are not connected to a `Julia` session (via `julia_connect()`). Set the map in `Julia` using a file path (via `set_map()`). Check the {patter} function documentation for supported options."
  }
  packageStartupMessage(paste(msg_startup, msg_linux))
}
