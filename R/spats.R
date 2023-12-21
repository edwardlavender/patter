#' @title Spatial helper: template [`SpatRaster`]s
#' @description This function creates a template [`SpatRaster`].
#' @param .xmin,.xmax,.ymin,.ymax Numbers that define the [`SpatRaster`]'s extent.
#' @param .res A `numeric` vector that defines the [`SpatRaster`]'s resolution.
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
  rlang::check_dots_used()
  r <- terra::rast(xmin = .xmin, xmax = .xmax,
                   ymin = .ymin, ymax = .ymax,
                   res = .res,
                   crs = .crs, ...)
  r <- terra::setValues(r, .value)
  terra::units(r) <- .units
  r
}
