#' @title Distances: along a path
#' @description This function calculates distances between sequential coordinates in a matrix.
#' @param .xy A two-column [`matrix`], [`data.frame`] or [`data.table`] of x, y coordinates. The input is coerced to a [`matrix`].
#' @param .lonlat A `logical` variable that defines whether or not the coordinates are in longitude/latitude format (`TRUE`) or planar (`FALSE`).
#' @details This function is a simple wrapper for [`terra::distance()`].
#'
#' @return The function returns a `numeric` vector. Each element (`i`) is the distance from the corresponding coordinates (`.xy[, i]`) to the next set of coordinates (`.xy[, i + 1]`). The final value is `NA_real_`.
#'
#' @examples
#' require(data.table)
#' require(dplyr)
#'
#' #### Example (1): Illustration with synthetic data
#' p <- matrix(c(1, 2,
#'               3, 4,
#'               5, 6), ncol = 2, byrow = TRUE)
#'
#' dist_along_path(p)
#'
#' #### Example (2): illustration with a `patter` movement-path data.table
#' dat_pfp() |>
#'   group_by(path_id) |>
#'   mutate(dist = dist_along_path(cbind(cell_x, cell_y))) |>
#'   as.data.table()
#'
#' @seealso
#' * See [`flapper::dist_*()`](https://edwardlavender.github.io/flapper/reference/) functions for other distance routines;
#' * See [`flapper::lcp_*()`](https://edwardlavender.github.io/flapper/reference/) functions for least-cost distance routines;
#' @export

dist_along_path <- function(.xy, .lonlat = FALSE) {
  if (!inherits(.xy, "matrix")) {
    if (ncol(.xy) != 2L) {
      abort("`.xy` should be a two-column matrix of coordinates.")
    }
    .xy <- as.matrix(.xy, ncol = 2)
  }
  dist <- terra::distance(.xy,
                          lonlat = .lonlat,
                          sequential = TRUE)
  c(dist[2:length(dist)], NA_real_)
}
