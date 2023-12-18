#' @title Map: internal functions
#'
#' A [`matrix`], [`data.frame`] or [`data.table`] with x and y coordinates, in columns named `x` and `y` or `cell_x` and `cell_y`. `x` and `y` columns are used preferentially. Coordinates must be planar.  A `timestep` column can also be included if there are multiple possible locations at each time step. A `mark` column can be included with coordinate weights; otherwise, equal weights are assumed (see Details). Other columns are ignored.
#'
#'
#'
#' @keywords internal

.map_marks <- function(.xpf, .coord) {
  if (is.null(.coord)) {
    xym <- spatMarks(.x = .xpf)
  } else {
    xym <- spatMarksFromCoord(.x = .xpf, .coord = .coord)
  }
  xym
}
