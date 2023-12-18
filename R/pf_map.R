#' @title PF: particle coordinates
#' @description This function collects particle samples and extracts coordinates.
#'
#' @param .history Particle samples, provided in any format accepted by [`.pf_history_dt()`].
#' @param .bathy The bathymetry [`SpatRaster`].
#' @param .wtd A `logical` variable that defines whether or not to summarise coordinates (see [`.pf_map_weights()`]).
#' @param .obs,.cols (optional) A [`data.table`] and a character vector of column names in `.obs` to match onto the output. `.obs` must contain a `timestep` column for matching.
#'
#' @details
#' This function is not memory safe.
#'
#' @return The function returns a [`data.table`] that defines timesteps, sampled locations and, optionally, information from `.obs`, with the following columns:
#' * `timestep`---an `integer` vector that defines the time step;
#' * `cell_id`---an `integer` vector that defines the cell ID on `.bathy`;
#' * `cell_x`, `cell_y`, `cell_z`---`double`s that define cell coordinates;
#'
#' If `.obs` is supplied, the output also contains any columns specified in `.cols`.
#'
#' @examples
#' p <- dat_pfbk()
#' pf_coords(p$history, dat_gebco())
#'
#' @seealso
#' TO DO
#'
#' @author Edward Lavender
#' @export

pf_coords <- function(.history, .bathy, .wtd = FALSE, .obs = NULL, .cols = NULL) {

  # Check user inputs
  # * `.history` is checked via .pf_history_dt()
  # * Check remaining inputs
  if (missing(.bathy)) {
    abort("`.bathy` is required for `pf_coords()`.")
  }
  .pf_path_pivot_checks(.obs, .cols)

  # Define particle coordinates
  sch <- schema(timestep = int32(),
                cell_now = int32(),
                x_now = double(),
                y_now = double())
  p <-
    .history |>
    .pf_history_dt(schema = sch, .collect = FALSE) |>
    rename(cell_id = .data$cell_now) |>
    collect() |>
    as.data.table()

  # (Optional) summarise coordinates
  if (.wtd) {
    p <- .pf_map_weights(p)
  }

  # Add grid cell coordinates
  p <-
    p |>
    mutate(cell_xy = terra::xyFromCell(.bathy, .data$cell_id),
           cell_x = .data$cell_xy[, 1],
           cell_y = .data$cell_xy[, 2],
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
