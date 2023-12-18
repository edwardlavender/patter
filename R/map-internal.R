#' @title Map: location coordinates (cell_id, x, y, mark)
#'
#' A [`matrix`], [`data.frame`] or [`data.table`] with x and y coordinates, in columns named `x` and `y` or `cell_x` and `cell_y`. `x` and `y` columns are used preferentially. Coordinates must be planar.  A `timestep` column can also be included if there are multiple possible locations at each time step. A `mark` column can be included with coordinate weights; otherwise, equal weights are assumed (see Details). Other columns are ignored.
#'
#'
#'
#' @keywords internal

.map_coord <- function(.map, .coord) {
  if (is.null(.coord)) {
    xym <- spatMarks(.x = .map)
  } else {
    xym <- spatMarksFromCoord(.x = .map, .coord = .coord)
  }
  xym
}


#' @title Map: location marks (weights)
#' @description This function calculates location weights in [`map_pou()`] and [`map_dens()`].
#' @param .pxy A [`data.table`] that contains locations. This should include a `cell_id` column and may include a `timestep` column.
#' @details This function can be used to calculate weights for any set of coordinates (grid cells), including but not exclusively from particle filtering.
#' @return The function returns a [`data.table`].
#' @author Edward Lavender
#' @keywords internal

.map_mark <- function(.coord) {

  # Check user inputs
  check_inherits(.coord, "data.table")
  check_names(.coord, "cell_id")

  # Define required columns
  if (is.null(.coord$timestep)) {
    timestep <- NULL
    .coord[, timestep := 1L]
  }

  # Define position weights
  # * If un-supplied, equal weights are assumed, summing to 1 at each time step
  # * Otherwise, existing weights are used, and forced to sum to 1 at each time step
  if (is.null(.coord$mark)) {
    .coord <-
      .coord |>
      group_by(.data$timestep) |>
      mutate(mark = rep(1/n(), n())) |>
      ungroup() |>
      as.data.table()
  } else {
    .coord <-
      .coord |>
      group_by(.data$timestep)  |>
      mutate(mark = .data$mark / sum(.data$mark)) |>
      ungroup() |>
      as.data.table()
  }

  # Calculate the total weight of each location within time steps
  .coord <-
    .coord |>
    # Drop NA or zero weights (required for `map_dens()`)
    filter(!is.na(.data$mark)) |>
    filter(.data$mark != 0) |>
    # Implement aggregation
    group_by(.data$timestep, .data$cell_id) |>
    mutate(mark = sum(.data$mark)) |>
    slice(1L) |>
    ungroup() |>
    as.data.table()

  # Calculate the total weight of each location across the whole time series
  .coord |>
    group_by(.data$cell_id) |>
    mutate(mark = sum(.data$mark)) |>
    slice(1L) |>
    ungroup() |>
    # Divide by number of time steps
    mutate(mark = .data$mark / sum(.data$mark)) |>
    select(!"timestep") |>
    as.data.table()
}
