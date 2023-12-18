#' @title Map: location coordinates (cell_id, x, y, mark)
#' @description This function defines coordinates and weights (marks) for utilisation distribution (UD) estimation.
#'
#' @param .map The [`SpatRaster`] used to represent the utilisation distribution.
#' @param .coord A [`matrix`], [`data.frame`] or [`data.table`] with x and y coordinates, in columns named `x` and `y` or `cell_x` and `cell_y`. `x` and `y` columns are used preferentially. **Coordinates must be planar**.  A `timestep` column can also be included if there are multiple possible locations at each time step. A `mark` column can be included with coordinate weights; otherwise, equal weights are assumed (see Details). Other columns are ignored.
#'
#' @details
#' This function defines a [`data.table`] that includes cell IDs (`cell_id`), coordinates (`x` and `y`) and associated weights (`mark`) for UD estimation.
#'
#' * If `.coord` is `NULL`, a [`data.table`] of coordinates is extracted from `.map` (in non `NA` regions) via [`terra::as.data.frame()`]. The values on `.map` are taken as weights and must sum to one.
#'
#' * If `.coord` is supplied, equal weights are assumed unless specified in a `mark` column. Default or supplied weights are normalised to sum to one at each time step. The total weight of each grid cell within time steps is calculated and then these weights are aggregated by location across the whole time series and renomalised. See the internal [`.map_mark()`] function for full details.
#'
#' @return The function returns a [`data.table`] with four columns:
#' * `cell_id`
#' * `x`,`y`---on the grid???
#' * `mark`
#'
#' @author Edward Lavender
#' @keywords internal

.map_coord <- function(.map, .coord = NULL, .simplify) {
  if (is.null(.coord)) {
    xym <- spatMarks(.x = .map)
  } else {
    xym <- spatMarksFromCoord(.x = .map, .coord = .coord, .simplify = .simplify)
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
  check_names(.coord, "id")

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
    group_by(.data$timestep, .data$id) |>
    mutate(mark = sum(.data$mark)) |>
    slice(1L) |>
    ungroup() |>
    as.data.table()

  # Calculate the total weight of each location across the whole time series
  .coord |>
    group_by(.data$id) |>
    mutate(mark = sum(.data$mark)) |>
    slice(1L) |>
    ungroup() |>
    # Divide by number of time steps
    mutate(mark = .data$mark / sum(.data$mark)) |>
    select(!"timestep") |>
    as.data.table()
}
