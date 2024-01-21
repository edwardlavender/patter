#' @title PF: particle coordinates
#' @description This function collects particle samples and extracts coordinates.
#'
#' @param .history Particle samples, provided in any format accepted by [`.pf_history_dt()`].
#' @param .bathy The bathymetry [`SpatRaster`].
#' @param .obs,.cols (optional) A [`data.table`] and a character vector of column names in `.obs` to match onto the output. `.obs` must contain a `timestep` column for matching.
#'
#' @details
#' The `timestep` and `cell_now` columns in [`pf_forward()`] outputs are required to extract coordinates. `cell_now` is used to extract coordinates on `.bathy`.
#'
#' This function is not memory safe, since the entire time series of coordinates is collected in memory.
#'
#' @return The function returns a [`data.table`] that defines time steps, sampled locations and, optionally, information from `.obs`, with the following columns:
#' * `timestep`---an `integer` vector that defines the time step;
#' * `cell_id`---an `integer` vector that defines the cell ID on `.bathy`;
#' * `cell_x`, `cell_y`, `cell_z`---`double`s that define cell coordinates;
#'
#' If `.obs` is supplied, the output also contains any columns specified in `.cols`.
#'
#' @example man/examples/pf_coord-examples.R
#'
#' @inherit pf_forward seealso
#'
#' @author Edward Lavender
#' @export

pf_coord <- function(.history, .bathy, .obs = NULL, .cols = NULL) {

  # Check user inputs
  # * `.history` is checked via .pf_history_dt()
  # * Check remaining inputs
  if (missing(.bathy)) {
    abort("`.bathy` is required for `pf_coord()`.")
  }
  .pf_path_pivot_checks(.obs, .cols)

  # Define particle coordinates
  sch <- schema(timestep = int32(),
                cell_now = int32(),
                x_now = double(),
                y_now = double())
  p <-
    .history |>
    .pf_history_dt(schema = sch, .collect = TRUE) |>
    rename(cell_id = "cell_now") |>
    as.data.table()

  # Add grid cell coordinates
  p <-
    p |>
    mutate(cell_xy = terra::xyFromCell(.bathy, .data$cell_id),
           cell_x = as.numeric(.data$cell_xy[, 1]),
           cell_y = as.numeric(.data$cell_xy[, 2]),
           cell_z = terra::extract(.bathy, .data$cell_id)) |>
    select(any_of("timestep"), "cell_id", "cell_x", "cell_y", "cell_z", any_of("mark")) |>
    as.data.table()

  # Add columns from `.obs` by matching by timestep (from `pf_path_pivot()`)
  if (!is.null(.obs)) {
    for (col in .cols) {
      p[, (col) := .obs[[col]][match(p$timestep, .obs$timestep)]]
      if (any(is.na(p[[col]]))) {
        warn("There are NAs in the {col} column in the output.",
             .envir = environment())
      }
    }
  }

  # Return data.table
  p

}
