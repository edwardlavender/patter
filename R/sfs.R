#' @title Spatial helper: `st_*` functions
#' @description Internal [`sf::sf-package`] helper functions.
#' @author Edward Lavender
#' @name sf-helper

#' @rdname sf-helper
#' @keywords internal

# Invert a spatial polygon
st_invert <- function(.x, .bbox = sf::st_bbox(.x)) {
  rlang::check_installed("sf")
  .x <- sf::st_union(.x)
  .bbox |>
    sf::st_as_sfc() |>
    sf::st_difference(.x)
}
