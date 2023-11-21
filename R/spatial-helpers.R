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
#' r <- spatTemplate()
#' terra::plot(r)
#' r <- spatTemplate(.value = 1)
#' terra::plot(r)
#' @author Edward Lavender
#' @export

spatTemplate <- function(.xmin = 0, .xmax = 1000,
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

#' @title Spatial helper: normalise a [`SpatRaster`]
#' @description This function normalises a [`SpatRaster`].
#' @param x A [`SpatRaster`].
#' @details
#' # Warning
#' * For speed in iterative applications (e.g., [`acs()`]), this function does not implement any internal checks.
#' * `NA`s are ignored.
#' @author Edward Lavender
#' @export

spatNormalise <- function(x) {
  x / as.numeric(terra::global(x, "sum", na.rm = TRUE))
}

#' @title Spatial helper: create circular angle in degrees
#' @description This function is a wrapper for `circular::circular(x, units = "degrees")`.
#' @param .x A numeric vector of angles (in degrees).
#' @return The function returns a [`circular::circular`] object.
#' @examples
#' degrees(10)
#' degrees(c(10, 20))
#' @author Edward Lavender
#' @export

degrees <- function(.x) {
  circular::circular(x = .x, units = "degrees")
}
