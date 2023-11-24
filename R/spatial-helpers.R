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
