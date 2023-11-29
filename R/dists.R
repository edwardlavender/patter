#' @title Distances: along a path
#' @description This function calculates distances between sequential coordinates in a matrix.
#' @param .xy A two-column matrix of coordinates.
#' @param .lonlat A `logical` variable that defines whether or not the coordinates are in longitude/latitude format.
#' @details This function is a simple wrapper for [`terra::distance()`].
#' @seealso
#' * `dist_*` functions in [`flapper`](https://github.com/edwardlavender/flapper);
#' @export

dist_along_path <- function(.xy, .lonlat = FALSE) {
  terra::distance(as.matrix(.xy, ncol = 2),
                  lonlat = .lonlat,
                  sequential = TRUE)
}
