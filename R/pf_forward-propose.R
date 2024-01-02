#' @title PF: proposal functions
#' @description These are proposal functions and supporting routines for [`pf_forward()`].
#' @param .obs,.t,.dlist Required arguments for all functions.
#' * `.obs`---the `.obs` [`data.table`] from [`pf_forward()`];
#' * `.t`---an `integer` that defines the time step;
#' * `.dlist`---the `.dlist` `list` from [`pf_forward()`];
#'      * For [`rkick()`] and [`dkick()`], `.dlist` must contain `.dlist$spatial$bathy`;
#'      * For [`pf_rpropose_kick()`], `.dlist` must contain `.dlist$spatial$bathy`;
#'      * For [`pf_rpropose_reachable()`], `.dlist` must contain `.dlist$spatial$bathy`;
#'
#' @param .particles Additional required arguments for all `.rpropose` and `.dpropose` functions:
#' * `.particles`---a [`data.table`] of particle samples from the previous time step;
#'
#' @param .rkick,.dkick,... Additional arguments for [`pf_rpropose_kick()`] and [`pf_dpropose()`] respectively.
#' * `.rkick` is a `function`, like [`rkick()`], that simulates new locations;
#' * `.dkick` is a `function`, like [`dkick()`], that calculates the probability density of movements between locations;
#'
#' At the time of writing, these functions must accept the `.xy0`, `.xy1`, `.rstep`/`.dstep` and `...` arguments.
#'
#' @param .xy1,.xy1,.rstep,.dstep Arguments for `.rkick` (and [`rkick()`]) and `.dkick` (and [`dkick()`]).
#' * `.xy0`---a two-column object ([`matrix`], [`data.frame`], [`data.table`]) of accepted (x, y) coordinates from the previous time step. In `.rkick`, movement is simulated from `.xy0` into new locations;
#' * `.xy1`---For `.dkick` and [`dkick()`], `.xy1` is a two-column matrix of accepted coordinates for the current time step;
#' * `.rstep` or `.dstep` and `...`---`function`s that simulate new locations or calculate the probability density of movements between locations (such as [`rstep()`] and [`dstep()`]) and additional arguments passed to those functions. Both functions must accept:
#'    * `.xy0` and `.xy1`---as above;
#'    * `...`---arguments passed from above;
#'    * `.lonlat`---a `logical` variable that defines whether or not coordinates are in longitude/latitude format or planar (extracted from `.dlist$pars$lonlat` by default);
#'
#' @details
#' In [`pf_forward()`], proposal functions are used to generate (propose) new, candidate locations for the individual's position, contingent upon previous positions (particle samples). Proposal locations are generated from previous locations via stochastic kicks and directed sampling.
#'
#' The `.rpropose` argument in [`pf_forward()`] expects a stochastic-kick routine and [`pf_rpropose_kick()`] is the default. This is used to simulate proposal locations by 'kicking' particles into new locations as specified by a movement model (`.rkick`).
#'
#' [`pf_rpropose_kick()`] is a simple wrapper for [`rkick()`] that passes the `.particles` [`data.table`] to the function as required and then updates `.particles` with simulated locations. Simulated coordinates are redefined on the grid (`.dlist$spatial$bathy`) and grid cell IDs are included in the output. [`rkick()`] itself is a wrapper for `.rstep` = [`rstep`] that accepts (but silently ignores) the `.obs`, `.t` and `.dlist` objects (except `.dlist$pars$lonlat`, which is passed to the `.lonlat` argument of `.rstep`). In [`pf_rpropose_kick`], arguments passed via `...` are passed to `.rkick`, which under default settings means `.rstep = ` = [`rstep`] (i.e., `.rlen` and `.rang` or additional arguments passed to those arguments). This facilitates simulation of a wide variety of random walks. At the time of writing, correlated random walks are not easy for users to implement, but this should improve in future.
#'
#' In [`pf_forward()`], if stochastic kicks fail to produce a sufficient number of valid particle samples, [`pf_rpropose_reachable()`] may be called under-the-hood for directed sampling (see [`pf_opt_trial()`]). For selected particles, this function identifies the set of reachable locations. We evaluate the likelihood of reachable locations and the probability density of moving into each location and then sample locations according to the (normalised) product of these two variables. The `.dpropose` argument in [`pf_forward()`] is required to calculate the probability density of moving between locations. `.dpropose` is a function that must accept the usual `.particles`, `.obs`, `.t`, `.dlist` and `...` arguments and return a [`data.table`], for the subset of valid locations, with a `dens` column that defines probability densities. The function must be able to handle empty [`data.table`]s, which are passed down the call stack if all proposal (reachable) locations have zero likelihood.
#'
#' [`pf_dpropose()`] is the default `.dpropose` routine. Under default settings, this is a simple wrapper for [`dkick()`] that handles empty [data.table]s or passes the relevant coordinate columns, for the accepted locations from the previous time step ((`x_past`, `y_past`) in `.particles`) and the proposal locations for the current time step ((`x_now`, `y_now`) in `.particles`) to `.dkick`. Under default settings, `.dkick =` [`dkick`]. [`dkick()`] wraps itself wraps a `.dstep` function such as [`dstep()`], accepting (but silently ignoring) the `.obs`, `.t` and `.dlist` arguments (except `.dlist$pars$lonlat`). In this situation, `...` arguments are passed to `.dstep`.
#'
#' In [`pf_forward()`], use `.rpropose` and `.dpropose` to write fully custom routines, if required. Use `.rargs` and `.dargs` to customise the default routines. For movement models that require `.obs`, `.t` and `.dlist`, use custom `.rkick` and `.dkick` functions. Otherwise, you can simply customise `.rstep` and `.dstep` (for instance, by revising the models used to simulate step lengths and turning angles, or the parameters passed to those models).
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @name pf_propose

#' @rdname pf_propose
#' @export

rkick <- function(.xy0,
                  .rstep = rstep, ...,
                  .obs, .t, .dlist){
  # One-off check (placeholder)
  if (.t == 2L) {
    check_dlist(.dlist = .dlist,
                .par = "lonlat")
  }
  # Call `.rstep`, silently ignoring `.obs`, `.t`, `.dlist` as required
  .rstep(.xy0 = .xy0, ...,
         .lonlat = .dlist$pars$lonlat)
}

#' @rdname pf_propose
#' @export

dkick <- function(.xy0, .xy1,
                  .dstep = dstep, ...,
                  .obs, .t, .dlist) {
  # One-off check (placeholder)
  if (.t == 2L) {
    check_dlist(.dlist = .dlist, .par = "lonlat")
  }
  # Call `.dstep`, silently ignoring `.obs`, `.t`, `.dlist` as required
  .dstep(.xy0 = .xy0,
         .xy1 = .xy1, ...,
         .lonlat = .dlist$pars$lonlat)
}

#' @rdname pf_propose
#' @export

pf_rpropose_kick <- function(.particles, .obs, .t, .dlist, .rkick = rkick, ...) {
  # Check inputs once
  # * The loop in pf_forward() starts on .t = 2L
  # * We assume pf_rpropose_kick() is activated on this step
  if (.t == 2L) {
    rlang::check_dots_used(error = function(cnd) rlang::inform(paste(cnd)))
    check_dlist(.dlist = .dlist,
                .par = "lonlat",
                .spatial = "bathy")
  }
  # Kick each particle from previous location into new proposal locations
  x_past <- y_past <- NULL
  xy_now <- .rkick(.xy0 = .particles[, list(x_past, y_past)],
                   .obs = .obs, .t = .t, .dlist = .dlist, ...)
  # Update data.table with cell IDs
  cell_now <- NULL
  .particles[, cell_now := as.integer(terra::cellFromXY(.dlist$spatial$bathy, xy_now))]
  # Update data.table with coordinates on grid
  xy_now <- terra::xyFromCell(.dlist$spatial$bathy, .particles$cell_now)
  x_now <- y_now <- NULL
  .particles[, x_now := as.numeric(xy_now[, 1])]
  .particles[, y_now := as.numeric(xy_now[, 2])]
  .particles
}

#' @rdname pf_propose
#' @export

pf_rpropose_reachable <- function(.particles, .obs, .t, .dlist) {

  # Check inputs placeholder
  # * TO DO
  # * The loop in pf_forward() starts on .t = 2L
  # * We check inputs assuming pf_rpropose_kick() is activated on this step
  # * (But this is unlikely)
  if (.t == 2L) {
    rlang::check_installed("sf")
    rlang::check_installed("exactextractr")
    check_dlist(.dlist = .dlist, .spatial = "bathy")
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

pf_dpropose <- function(.particles, .obs, .t, .dlist, .dkick = dkick, ...) {

  # Check inputs placeholder
  # * TO DO
  # * The loop in pf_forward() starts on .t = 2L
  # * We check inputs assuming pf_dpropose() is activated on this step
  # * (But this is unlikely)
  if (.t == 2L) {
    rlang::check_dots_used(error = function(cnd) rlang::inform(paste(cnd)))
    check_dlist(.dlist = .dlist, .par = "lonlat")
  }

  # Handle empty data.tables
  # * These result when all proposals have zero likelihood
  if (fnrow(.particles) == 0L) {
    dens <- NULL
    return(.particles[, dens := numeric()])
  } else {
    # Calculate densities
    x_past <- y_past <- x_now <- y_now <- NULL
    .particles[, dens := .dkick(.xy0 = .particles[, list(x_past, y_past)],
                                .xy1 = .particles[, list(x_now, y_now)],
                                ...,
                                .obs = .obs, .t = .t, .dlist = .dlist)]
    # Isolate particles with positive densities
    .particles |>
      filter(dens > 0) |>
      as.data.table()
  }
}
