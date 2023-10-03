#' @title Spatial helper: create a template [`SpatRaster`]
#' @description This function creates a template [`SpatRaster`].
#' @param .xmin,.xmax,.ymin,.ymax Numbers that define the [`SpatRaster`] limits.
#' @param .res A `numeric` vector that defines [`SpatRaster`] resolution.
#' @param .crs A `character` that defines the Coordinate Reference System.
#' @param .value The value of the [`SpatRaster`] cells.
#' @param .units A `character` that defines the units.
#' @param ... Additional arguments passed to [`SpatRaster`].
#' @return The function returns a [`SpatRaster`].
#' @examples
#' r <- rast_template()
#' terra::plot(r)
#' r <- rast_template(.value = 1)
#' terra::plot(r)
#' @author Edward Lavender
#' @export

rast_template <- function(.xmin = 0, .xmax = 1000,
                          .ymin = 0, .ymax = 1000,
                          .res = c(10, 10),
                          .crs = "+proj=utm +zone=1 +datum=WGS84",
                          .value = 0,
                          .units = "m", ...) {
  r <- terra::rast(xmin = .xmin, xmax = .xmax,
                   ymin = .ymin, ymax = .ymax,
                   res = .res,
                   crs = .crs, ...)
  r <- terra::setValues(r, .value)
  terra::units(r) <- .units
  r
}

#' @title Calculate the centre of mass of weighted coordinates
#' @description This is a wrapper for [`geosphere::geomean()`] that handles one-row matrices.
#' @param xy,w Arguments passed to [`geosphere::geomean()`].
#' @author Edward Lavender
#' @keywords internal

geomean <- function(xy, w = NULL) {
  if (nrow(xy) == 1L) {
    xy
  } else {
    geosphere::geomean(xy, w)
  }
}
