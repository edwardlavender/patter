#' @title PF: Calculate location weights
#' @description This function calculates location weights in [`pf_map_pou()`] and [`pf_map_dens()`].
#' @param .pxy A [`data.table`] that contains locations. This should include a `cell_id` column and may include a `timestep` column.
#' @details This function can be used to calculate weights for any set of coordinates (grid cells), including but not exclusively from particle filtering.
#' @return The function returns a [`data.table`].
#' @author Edward Lavender
#' @keywords internal

.pf_map_weights <- function(.pxy) {

  # Check user inputs
  check_inherits(.pxy, "data.table")
  check_names(.pxy, "cell_id")

  # Define required columns
  if (is.null(.pxy$timestep)) {
    timestep <- NULL
    .pxy[, timestep := 1L]
  }

  # Define position weights
  # * If un-supplied, equal weights are assumed, summing to 1 at each time step
  # * Otherwise, existing weights are used, and forced to sum to 1 at each time step
  if (is.null(.pxy$mark)) {
    .pxy <-
      .pxy |>
      group_by(.data$timestep) |>
      mutate(mark = rep(1/n(), n())) |>
      ungroup() |>
      as.data.table()
  } else {
    .pxy <-
      .pxy |>
      group_by(.data$timestep)  |>
      mutate(mark = .data$mark / sum(.data$mark)) |>
      ungroup() |>
      as.data.table()
  }

  # Calculate the total weight of each location within time steps
  .pxy <-
    .pxy |>
    # Drop NA or zero weights (required for `pf_map_dens()`)
    filter(!is.na(.data$mark)) |>
    filter(.data$mark != 0) |>
    # Implement aggregation
    group_by(.data$timestep, .data$cell_id) |>
    mutate(mark = sum(.data$mark)) |>
    slice(1L) |>
    ungroup() |>
    as.data.table()

  # Calculate the total weight of each location across the whole time series
  .pxy |>
    group_by(.data$cell_id) |>
    mutate(mark = sum(.data$mark)) |>
    slice(1L) |>
    ungroup() |>
    # Divide by number of time steps
    mutate(mark = .data$mark / sum(.data$mark)) |>
    select(!"timestep") |>
    as.data.table()
}
