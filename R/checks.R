#' @title Check inherited class(es)
#' @description This function checks inherited class(es).
#' @param x An object.
#' @param class The required class(es).
#' @return The function checks whether `x` inherits `class` and, if not, returns an error.
#' @keywords internal

check_inherits <- function(x, class) {
  if (!inherits(x, class)) {
    abort("`{deparse(substitute(x))}` must be a {substitute(class)}.",
          .envir = environment())
  }
}


#' @title Check input datasets
#' @description These functions check input datasets.
#' @param acoustics A data.table.
#' @param archival A data.table or `NULL`.
#' @name check_data
NULL

#' @rdname check_data
#' @keywords internal

check_acoustics <- function(acoustics) {
  # Check class
  check_inherits(acoustics, "data.table")
  # Check required columns
  # TO DO
  # Check sorted
  # TO DO
}

#' @rdname check_data
#' @keywords internal

check_archival <- function(archival) {
  if (!is.null(archival)) {
    # Check class
    check_inherits(archival, "data.table")
    # Check required columns
    # TO DO
    # Check sorted
    # TO DO
  }
}
