# TO DO: DEPRECATE

#' @title PF: template movement models
#' @description These functions are example movement models, of the kind required by `.kick` in [`pf_forward_*()`].
#' @param .particles A [`data.table`], from [`pf_forward_*()`], that defines the current particle samples:
#' * `cell_now` is an integer vector of cell IDs;
#' * `x_now` is a numerical vector of `x` coordinates;
#' * `y_now` is a numerical vector of `y` coordinates;
#' @param .obs,.t,.bathy (optional) The `.obs` [`data.table`], an integer that indexes `.obs` and the `.bathy` [`SpatRaster`] (see [`pf_forward_1()`]). These inputs are unused in this template movement model but supported within [`pf_forward_*()`].
#' @param .sim_length,.sim_angle,... Functions and additional arguments that simulate step lengths and turning angles. The first argument of each function should be the number of step lengths/turning angles to simulate (defined internally).
#' @param .lonlat A logical variable that defines whether or not coordinates are longitudes/latitudes.
#'
#' @details This template movement model is a biased random walk. Step lengths are simulated from a truncated Gamma distribution via [`rlen()`]. Turning angles are simulated from a wrapped normal distribution via [`rangrw()`].
#'
#' # Warning
#'
#' * These function are used to streamline examples and do not represent a generically suitable model.
#' * The functions do not check user inputs.
#'
#' @examples
#' # Load packages
#' require(graphics)
#' require(data.table)
#' require(dtplyr)
#' require(dplyr, warn.conflicts = FALSE)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Define hypothetical `.particles` data.table
#' p <-
#'   dat_gebco() |>
#'   terra::spatSample(size = 10L, cells = TRUE, xy = TRUE, as.df = FALSE,
#'                     na.rm = TRUE) |>
#'   as.data.table() |>
#'   select(cell_now = cell, x_now = x, y_now = y) |>
#'   as.data.table()
#'
#' # Kick particles into new locations
#' p <- pf_kick(p)
#'
#' # Visualise current & next locations
#' terra::plot(dat_gebco())
#' arrows(x0 = p$x_now, x1 = p$x_next,
#'        y0 = p$y_now, y1 = p$y_next,
#'        length = 0.02)
#'
#' @return The function returns a [`data.table`], as supplied, with two additional columns:
#' * `x_next`---the `x` coordinate of the next proposal location(s);
#' * `y_next`---the `y` coordinates of the next proposal location(s);
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward_*()`]:
#'     * [`pf_forward_1()`] refines AC-branch algorithm ([`acs()`] and [`dc()`]) outputs using PF;
#'     * [`pf_forward_2()`] is an integrated implementation that couples AC- and PF-branch algorithms internally;
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_setup_files()`];
#'     * Template movement models, namely [`pf_kick()`];
#'
#' * The backward pass is implemented by [`pf_backward()`];
#'
#' * Movement paths are built from PF outputs via `pf_path()` functions:
#'     * [`pf_path()`] reconstructs paths;
#'     * [`pf_path_pivot()`] supports path reconstruction;
#'
#' * To reconstruct maps of space use, see:
#'     * [`pf_coords()`] to extract particle coordinates;
#'     * [`pf_map_pou()`] for probability-of-use maps;
#'     * [`pf_map_dens()`] for smooth utilisation distributions;
#'     * [`get_hr()`] for home range estimates;
#'
#' @author Edward Lavender
#' @name pf_kick

#' @rdname pf_kick
#' @export

pf_kick <- function(.particles,
                    .obs = NULL,
                    .t = NULL,
                    .bathy = NULL,
                    .sim_length = rlen,
                    .sim_angle = rangrw,
                    .lonlat = FALSE, ...) {
  # Simulate step length & turning angle
  n    <- nrow(.particles)
  rlen <- .sim_length(n, ...)
  rang <- .sim_angle(n, ...)
  # Kick each particle into new proposal locations
  x_now <- y_now <- NULL
  xy_next <- cstep(.xy_now = as.matrix(.particles[, list(x_now, y_now)]),
                   .lonlat = .lonlat,
                   .length = rlen,
                   .angle = rang)
  # Update data.table
  x_next <- y_next <- NULL
  .particles[, x_next := xy_next[, 1]]
  .particles[, y_next := xy_next[, 2]]
  .particles
}
