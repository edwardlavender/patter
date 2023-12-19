#' @title Utilities: temporary directories
#' @description This function wipes and (re)-creates a temporary directory.
#' @param ... Arguments passed to [`file.path`].
#' @author Edward Lavender
#' @export

mktempdir <- function(...) {
  folder <- file.path(tempdir(), ...)
  if (dir.exists(folder)) {
    unlink(folder, recursive = TRUE)
  }
  dir.create(folder, recursive = TRUE)
  folder
}
