#' @title Get animal 'home ranges'
#' @description These functions extract 'home range' estimates from a [`SpatRaster`] that describes the intensity of movements within an area.
#'
#' @param .x A [`SpatRaster`] (utilisation distribution).
#' @param .prop For [`get_hr_prop()`], `.prop` is a number that defines the range proportion.
#' @param .add A logical variable that defines whether or not to add a polygon of the range to an existing map.
#' @param ... If `.add = TRUE`, `...` is a place holder for additional arguments passed to [`terra::plot()`].
#'
#' @details These functions are modelled on the [`get_hr_*()`](https://edwardlavender.github.io/flapper/reference/get_hr.html) functions in the [`flapper`](https://github.com/edwardlavender/flapper) package, where full details are provided. The [`spatialEco`] package is required.
#'
#' @return The functions return a [`SpatRaster`]. Cells with a value of one are inside the specified range boundaries; cells with a value of zero are beyond range boundaries. If `.add` is `TRUE`, the boundaries are added to an existing plot.
#'
#' @examples
#'
#' @author Edward Lavender
#' @name get_hr
NULL

#' @name get_hr
#' @export

get_hr_prop <- function(.x, .prop = 0.5, .add = FALSE, ...) {
  rlang::check_installed("spatialEco")
  if (length(.prop) != 1L) {
    abort("`.prop` should be a single number (proportion).")
  }
  .x <- terra::classify(.x, cbind(0, NA))
  map <- spatialEco::raster.vol(.x, p = .prop, sample = FALSE)
  if (.add) {
    poly <- terra::as.polygons(map == 1)
    if (any(poly$layer == 1)) {
      poly <- poly[poly$layer == 1]
      terra::plot(poly, add = TRUE, ...)
    }
  }
  invisible(map)
}

#' @name get_hr
#' @export

get_hr_core <- function(.x, .add = FALSE, ...) {
  get_hr_prop(.x = .x, .prop = 0.5, .add = .add, ...)
}

#' @name get_hr
#' @export

get_hr_home <- function(.x, .add = FALSE, ...) {
  get_hr_prop(.x = .x, .prop = 0.95, .add = .add, ...)
}

#' @name get_hr
#' @export

get_hr_full <- function(.x, .add = FALSE, ...) {
  get_hr_prop(.x = .x, .prop = 1, .add = .add, ...)
}
