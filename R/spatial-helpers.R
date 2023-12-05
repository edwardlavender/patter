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

#' @title Invert a spatial polygon
#' @keywords internal

.st_invert <- function(.x, .bbox = sf::st_bbox(.x)) {
  rlang::check_installed("sf")
  .x <- sf::st_union(.x)
  .bbox |>
    sf::st_as_sfc() |>
    sf::st_difference(.x)
}
