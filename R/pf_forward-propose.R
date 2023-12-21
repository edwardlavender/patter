#' @title PF: proposal functions
#' @description These are proposal functions for [`pf_forward()`].
#' @param .particles,.obs,.t,.dlist Required arguments for all `propose` functions.
#' * `.particles`---a [`data.table`] of particle samples from the previous time step;
#' * `.obs`---the `.obs` [`data.table`] from [`pf_forward()`];
#' * `.t`---an `integer` that defines the time step;
#' * `.dlist`---the `.dlist` `list` from [`pf_forward()`];
#' @param .sim_length,.sim_angle Additional arguments for [`pf_rpropose_kick()`]. These are functions that simulate step lengths and turning angles respectively. Each function must accept a `.n` argument that defines the number of simulated values.
#' @param ... Additional arguments.
#' * For [`pf_rpropose_kick()`], `...` is passed to `.sim_length` and `.sim_angle`.
#' * For [`pf_dpropose()`], `...` is passed to [`dstep()`].
#'
#' @details
#' In [`pf_forward()`], proposal functions are used to generate (propose) new, candidate locations for the individual's position, contingent upon previous positions (particle samples). Proposal locations are generated from previous locations via stochastic kicks and directed sampling.
#'
#' The `.rpropose` argument in [`pf_forward()`] expects a stochastic-kick routine and [`pf_rpropose_kick()`] is the default. This is used to simulate proposal locations by 'kicking' particles into new locations as specified by a movement model. Custom functions that accept the `.particles`, `.obs`, `.t` and `.dlist` arguments can be provided.
#'
#' If stochastic kicks fail to produce a sufficient number of valid particle samples, [`pf_rpropose_reachable()`] may be called under-the-hood for directed sampling. For selected particles, this function identifies the set of reachable locations, for which likelihood calculations and sampling are then implemented (see [`pf_forward()`]).
#'
#' [`pf_dpropose()`] calculates the probability density of movements into proposal locations and is required for directed sampling.
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @name pf_propose

#' @rdname pf_propose
#' @export

pf_rpropose_kick <- function(.particles, .obs, .t, .dlist,
                             .sim_length = rlen,
                             .sim_angle = rangrw, ...
                             ) {
  # Simulate step length & turning angle
  n    <- nrow(.particles)
  rlen <- .sim_length(n, ...)
  rang <- .sim_angle(n, ...)
  # Kick each particle from previous location into new proposal locations
  x_past <- y_past <- NULL
  xy_next <- cstep(.xy_now = as.matrix(.particles[, list(x_past, y_past)]),
                   .lonlat = .dlist$pars$lonlat,
                   .length = rlen,
                   .angle = rang)
  # Update data.table
  cell_now <- NULL
  .particles[, cell_now := as.integer(terra::cellFromXY(.dlist$spatial$bathy, xy_next))]
  xy_next <- terra::xyFromCell(.dlist$spatial$bathy, .particles$cell_now)
  x_now <- y_now <- NULL
  .particles[, x_now := as.numeric(xy_next[, 1])]
  .particles[, y_now := as.numeric(xy_next[, 2])]
  .particles
}

#' @rdname pf_propose
#' @export

pf_rpropose_reachable <- function(.particles, .obs, .t, .dlist) {

  if (.t == 1L) {
    rlang::check_installed("sf")
    rlang::check_installed("exactextractr")
  }

  # Isolate unique particles
  .particles <-
    .particles |>
    # select("cell_past", "x_past", "y_past") |>
    distinct(.data$cell_past, .keep_all = TRUE)

  # Define reachable zone(s) (given .mobility)
  zone <-
    .particles |>
    select("x_past", "y_past") |>
    as.matrix() |>
    terra::vect(type = "point") |>
    terra::buffer(width = .obs$mobility[.t], quadsegs = 1e3)
  terra::crs(zone) <- terra::crs(.dlist$spatial$bathy)

  # Identify permitted location choices given .mobility
  # * Use exact_extract() for much faster speeds
  choices <-
    exactextractr::exact_extract(.dlist$spatial$bathy,
                                 sf::st_as_sf(zone),
                                 include_cell = TRUE,
                                 include_xy = TRUE,
                                 progress = FALSE)
  for (i in seq_len(length(choices))) {
    choices[[i]]$cell_past <- .particles$cell_past[i]
  }
  choices <-
    choices |>
    rbindlist() |>
    filter(!is.na(.data$value)) |>
    mutate(cell = as.integer(.data$cell),
           x = as.numeric(.data$x),
           y = as.numeric(.data$y)) |>
    select("cell_past", cell_now = "cell",
           x_now = "x", y_now = "y",
           bathy = "value") |>
    as.data.table()

  # Return 'proposal' (possible) cells (given .mobility only)
  # * In downstream functions, we filter & weight these
  merge(.particles, choices, by = "cell_past")
}

#' @rdname pf_propose
#' @export

pf_dpropose <- function(.particles, .obs, .t, .dlist, ...) {
  # Handle empty data.tables
  # * These result when all proposals have zero likelihood
  if (fnrow(.particles) == 0L) {
    dens <- NULL
    return(.particles[, dens := numeric()])
  } else {
    # Calculate densities
    x_past <- y_past <- x_now <- y_now <- NULL
    .particles[, dens := dstep(.data_now = .particles[, list(x_now = x_past, y_now = y_past)],
                               .data_past = .particles[, list(x_now, y_now)],
                               pairwise = TRUE,
                               lonlat = .dlist$pars$lonlat, ...)]
    # Isolate particles with positive densities
    .particles |>
      filter(dens > 0) |>
      as.data.table()
  }
}
