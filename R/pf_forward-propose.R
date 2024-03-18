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
#' @param .particles,.drop Additional required arguments for all `.rpropose` and/or `.dpropose` functions:
#' * `.particles`---a [`data.table`] of particle samples from the previous time step;
#' #' * `.drop`---for `.dpropose`, `.drop` is a `logical` variable that defines whether or not to drop particles with zero density;
#'
#' @param .rkick,.dkick,... Additional arguments for [`pf_rpropose_kick()`] and [`pf_dpropose()`] respectively.
#' * `.rkick` is a `function`, like [`rkick()`], that simulates new locations;
#' * `.dkick` is a `function`, like [`dkick()`], that calculates the probability density of movements between locations;
#'
#' At the time of writing, these functions must accept the `.xy0`, `.xy1`, `.rstep`/`.dstep` and `...` arguments.
#'
#' @param .xy0,.xy1,.rstep,.dstep Arguments for `.rkick` (and [`rkick()`]) and `.dkick` (and [`dkick()`]).
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
#' [`pf_rpropose_kick()`] is a simple wrapper for [`rkick()`] that passes the `.particles` [`data.table`] to the function as required and then updates `.particles` with simulated locations (`x_now` and `y_now` columns). Coordinates are defined on a continuous domain, as required to ensure consistency among routines, but grid cells IDs (on `.dlist$spatial$bathy`) are included in the output (in a `cell_now` column). (Proposals beyond the grid are silently dropped.) This means that the simulation of stochastic kicks remains accurate irrespective of the spatial resolution of likelihood evaluations (i.e., whether or not likelihoods are evaluated at particle locations or on a grid). [`rkick()`] itself is a wrapper for `.rstep` = [`rstep`] that accepts (but silently ignores) the `.obs`, `.t` and `.dlist` objects (except `.dlist$pars$lonlat`, which is passed to the `.lonlat` argument of `.rstep`). In [`pf_rpropose_kick`], arguments passed via `...` are passed to `.rkick`, which under default settings means `.rstep = ` = [`rstep`] (i.e., `.rlen` and `.rang` or additional arguments passed to those arguments). At the time of writing, correlated random walks are not easy for users to implement, but this should improve in future.
#'
#' In [`pf_forward()`], if stochastic kicks fail to produce a sufficient number of valid particle samples, [`pf_rpropose_reachable()`] may be called under-the-hood for directed sampling (see [`pf_opt_trial()`]). For selected particles, this function identifies the set of reachable locations, by drawing a circle of radius `.obs$mobility[.t]` around each particle at time `.t`. The coordinates of reachable locations (within these circles) are defined at the centroids of each grid cell. We evaluate the likelihood of reachable locations and the probability density of moving into each location and then sample locations according to the (normalised) product of these two variables. The `.dpropose` argument in [`pf_forward()`] is required to calculate the probability density of moving between locations. `.dpropose` is a function that must accept the usual `.particles`, `.obs`, `.t`, `.dlist` and `...` arguments and return a [`data.table`], for the subset of valid locations, with a `dens` column that defines probability densities. The function must be able to handle empty [`data.table`]s, which are passed down the call stack if all proposal (reachable) locations have zero likelihood. Note that since in [`pf_rpropose_reachable()`] coordinates are necessarily defined on a grid, a discretisation error is introduced that can prevent movement into valid cells, even if the edges of those cells are reachable, since coordinates are defined at cell centres. In general, this error should be negligible, but it may be important with low-resolution grids and/or in situations where there are very few valid locations. A possible solution in this instance is to increase `.obs$mobility` and the `.mobility` (maximum moveable distance) parameter for `.dpropose` (but not `.rpropose`) by half a grid cell. This is not currently implemented automatically.
#'
#' [`pf_dpropose()`] is the default `.dpropose` routine. Under default settings, this is a simple wrapper for [`dkick()`] that handles empty [data.table]s or passes the relevant coordinate columns, for the accepted locations from the previous time step ((`x_past`, `y_past`) in `.particles`) and the proposal locations for the current time step ((`x_now`, `y_now`) in `.particles`) to `.dkick`. Under default settings, `.dkick =` [`dkick`]. [`dkick()`] wraps itself wraps a `.dstep` function such as [`dstep()`], accepting (but silently ignoring) the `.obs`, `.t` and `.dlist` arguments (except `.dlist$pars$lonlat`). In this situation, `...` arguments are passed to `.dstep`.
#'
#' In [`pf_forward()`], use `.rpropose` and `.dpropose` to write fully custom routines, if required. Use `.rargs` and `.dargs` to customise the default routines. For movement models that require `.obs`, `.t` and `.dlist`, use custom `.rkick` and `.dkick` functions. Otherwise, you can simply customise `.rstep` and `.dstep` (for instance, by revising the models used to simulate step lengths and turning angles, or the parameters passed to those models).
#'
#' For consistency, all proposal functions should account for the maximum moveable distance in a given time step. In the default routines:
#' * [`pf_rpropose_kick()`] uses a `.mobility` parameter that is passed down to [`rtruncgamma()`] that prevents kicks exceeding the moveable distance;
#' * [`pf_rpropose_reachable()`] uses `.obs$mobility` to define reachable locations;
#' * [`pf_dpropose()`] uses a `.mobility` parameter that is passed down to [`dtruncgamma()`] that eliminates kicks exceeding the moveable distance;
#'
#' @return In [`pf_forward()`], `.rpropose` and `.dpropose` must return a [`data.table`], as inputted, but including the following additional columns:
#' * `.rpropose`:
#'    * `cell_now`---an `integer` vector of cell IDs;
#'    * `x_now` and `y_now`---`numeric` vectors of coordinates;
#' * `.dpropose`:
#'    * `dens`---a `numeric` vector that defines the probability density of movements from `(x_past, y_past)` to `(x_now, y_now)`;
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
  x_now <- y_now <- x_past <- y_past <- NULL
  xy_now <- .rkick(.xy0 = .particles[, list(x_past, y_past)],
                   .obs = .obs, .t = .t, .dlist = .dlist, ...)
  # Update data.table with coordinates
  .particles[, x_now := as.numeric(xy_now[, 1])]
  .particles[, y_now := as.numeric(xy_now[, 2])]
  cell_now <- NULL
  .particles[, cell_now := as.integer(terra::cellFromXY(.dlist$spatial$bathy, xy_now))]
  # Drop kicks beyond the study area
  # * Use if (any(bool)) here to avoid copying .particles unless essential
  # * .particles[!is.na(cell_now), ] copies .particles even if there are no NAs
  bool <- is.na(.particles$cell_now)
  if (any(bool)) {
    .particles <- .particles[!is.na(cell_now), ]
  }
  # TO DO
  # * Check behaviour if all proposals are beyond the study area
  # Update data.table with coordinates on grid
  # * This is no longer implemented
  # * It can create movement distances > mobility
  # * This causes issues with pf_rpropose_reachable() and pf_dpropose()
  # * Now, we evaluate movement in continuous space
  # * Likelihoods are evaluated on the grid
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
    check_names(input = .obs, req = "mobility")
  }

  # Isolate unique particles
  .particles <-
    .particles |>
    lazy_dt() |>
    distinct(.data$cell_past, .keep_all = TRUE) |>
    select("timestep", "cell_past", "x_past", "y_past", "logwt") |>
    as.data.table()

  # Define reachable zone(s) (given .mobility)
  zone <-
    .particles |>
    select("x_past", "y_past") |>
    as.matrix() |>
    terra::vect(type = "point") |>
    terra::buffer(width = .obs$mobility[.t], quadsegs = 1e3L) |>
    sf::st_as_sf() |>
    sf::st_set_crs(terra::crs(.dlist$spatial$bathy)) |>
    mutate(cell_past = .particles$cell_past)

  # Identify permitted location choices given .mobility
  # * Use exact_extract() for much faster speeds
  choices <-
    exactextractr::exact_extract(.dlist$spatial$bathy,
                                 zone,
                                 include_cols = "cell_past",
                                 include_cell = TRUE,
                                 include_xy = TRUE,
                                 progress = FALSE) |>
    rbindlist() |>
    lazy_dt() |>
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

pf_dpropose <- function(.particles, .obs, .t, .dlist, .drop, .dkick = dkick, ...) {

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
    logdens <- NULL
    return(.particles[, logdens := numeric()])
  } else {
    # Calculate densities
    x_past <- y_past <- x_now <- y_now <- NULL
    .particles[, logdens := log(.dkick(.xy0 = .particles[, list(x_past, y_past)],
                                       .xy1 = .particles[, list(x_now, y_now)],
                                       ...,
                                       .obs = .obs, .t = .t, .dlist = .dlist))]
    # Isolate particles with positive densities
    if (.drop) {
      .particles <-
        .particles |>
        lazy_dt() |>
        filter(logdens > -Inf) |>
        as.data.table()
    }
    .particles
  }
}
