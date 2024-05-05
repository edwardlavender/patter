#' @title Spatial helper: `spat*` functions
#' @description Internal helpers for `terra` [`SpatRaster`] and [`SpatVector`] objects.
#' @author Edward Lavender
#' @name spat

#' @rdname spat
#' @keywords internal

# Intersect SpatVectors in a `list`
spatIntersect <- function(.x) {
  check_inherits(.x, "list")
  if (length(.x) == 1L) {
    return(.x[[1]])
  }
  int <- .x[[1]]
  for (i in 2:length(.x)) {
    int <- terra::intersect(int, .x[[i]])
  }
  int
}
