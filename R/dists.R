#' @title Distances: along a path
#' @description This function calculates distances between sequential coordinates in a matrix.
#' @param .xy A two-column matrix of coordinates.
#' @param .lonlat A `logical` variable that defines whether or not the coordinates are in longitude/latitude format.
#' @details This function is a simple wrapper for [`terra::distance()`].
#' @seealso
#' * `dist_*` functions in [`flapper`](https://github.com/edwardlavender/flapper);
#' * [`dist_along_path()`] calculates distances along a path;
#' * [`dist_btw_cells()`] calculates distances between [`SpatRaster`] cells;
#' @export

dist_along_path <- function(.xy, .lonlat = FALSE) {
  terra::distance(as.matrix(.xy, ncol = 2),
                  lonlat = .lonlat,
                  sequential = TRUE)
}

#' @title Distances: between cells on a [`SpatRaster`]
#' @description This function calculates distances between cells on a [`SpatRaster`]
#' @param .bathy A [`SpatRaster`].
#' @param .from,.to Integer vectors that define cell IDs.
#' @param .lonlat A `logical` variable that defines whether or not the `.bathy` is in longitude/latitude format.
#' @param ... Arguments passed to  [`terra::distance()`].
#' @seealso
#' * `dist_*` functions in [`flapper`](https://github.com/edwardlavender/flapper);
#' * [`dist_along_path()`] calculates distances along a path;
#' * [`dist_btw_cells()`] calculates distances between [`SpatRaster`] cells;
#' @export

dist_btw_cells <- function(.bathy,
                           .from, .to,
                           .lonlat = FALSE, ...) {
  from <- terra::xyFromCell(.bathy, .from)
  to   <- terra::xyFromCell(.bathy, .to)
  terra::distance(from, to, .lonlat, ...)
}
