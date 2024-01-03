#' @title Simulation: distribution functions
#' @description These convenience functions support the generation of animal movement paths and observations in _de novo_ simulations (`sim_*()` functions) and simulation-based reconstructions of movement paths ([`pf_forward()`] and [`pf_backward_sampler()`]).
#'
#' * `ss*()` functions set seeds;
#' * `r*()` functions simulate random variates;
#' * `c*()` functions calculate outcomes from random-variate inputs;
#' * `d*()` functions return densities;
#'
#' `r*()` and `c*()` functions are used in _de novo_ simulations (via `sim_*()` functions such as [`sim_path_walk()`] and [`sim_detections()`]) and the forward simulation-based reconstruction of movement paths (in [`pf_forward()`]).
#'
#' `d*()` functions are primarily used in the simulation-based reconstruction of movement paths as part of the backward sampler via [`pf_backward_sampler()`].
#'
#' @param .n,.x Arguments for distribution functions ([`rbern()`], [`dbern()`], [`rtruncgamma()`] and [`dtruncgamma()`]):
#' * `.n` is an `integer` that defines the number of simulated outcome(s);
#' * `.x` is a `numeric` vector that simulated outcome(s);
#'
#' @param .prob Additional argument(s) for [`rbern()`] and [`dbern()`]:
#' * `.prob` is `numeric` vector of probabilities.
#'
#' @param .alpha,.beta,.gamma Additional arguments for [`ddetlogistic()`] (see Details).
#' * `.alpha` is a `numeric` value for the intercept, on the scale of the logistic function.
#' * `.beta` is a `numeric` value for the gradient, on the scale of the logistic function.
#' * `.gamma` is a `numeric` value for the receiver detection range.
#'
#' @param .data,.ddet Arguments for [`rdet()`].
#' * `.data` is a [`data.table`].
#' * `.ddet` is a `function` evaluates the probability of a detection;
#'
#' For [`rdet()`], `.ddet` must accept the `.data` [`data.table`] as its first argument and evaluate the probability of a detection accordingly. [`rbern()`] then simulates detection(s). For `.ddet =` [`ddet`], `.data` must include a `dist` column that defines the distances between transmission locations and receivers. In [`ddet()`], `.data$dist` is passed to [`ddetlogistic()`] to calculate detection probabilities (see Details).
#'
#' @param .ddetx Additional arguments for [`ddet()`].
#' * `.ddetx` is a function that evaluates the probability density of detections from a `numeric` vector (e.g,. of distances).
#'
#' @param .shape,.scale,.mobility Additional arguments for [`rtruncgamma()`] and [`dtruncgamma()`]:
#' * `.shape` is a `numeric` value that defines the shape parameter of a Gamma distribution (see [`stats::rgamma()`]).
#' * `.scale` is a `numeric` value that defines the scale parameter of a Gamma distribution (see [`stats::rgamma()`]).
#' * `.mobility` is a `numeric` value that defines the maximum possible value (step length) (see `truncdist::rtrunc()`).
#'
#' @param .mu,.rho,.sd Arguments for [`rwn()`] for the simulation of turning angles, passed to the `mu`, `rho` and `sd` arguments of [`circular::rwrappednormal()`].
#'
#' @param .prior,.t Additional arguments for [`rlen()`], [`rangrw()`] and [`rangcrw()`], as used in top-level functions (i.e., [`sim_path_walk()`]):
#' * `.n`---an `integer` that defines the number of simulated outcome(s);
#' * `.prior`---a `numeric` vector that defines the simulated value(s) from the previous time step;
#' * `.t`---an `integer` that defines the time step;
#' * `...`---additional arguments, if needed;
#'
#' @param .xy0,.xy1,.lonlat Shared arguments for `*step()` functions and `c*()` functions.
#' * `.xy0`---a two-column object ([`matrix`], [`data.frame`], [`data.table`]) of (x, y) coordinates;
#' * `.xy1`---a two-column object of (x, y) coordinates;
#' * `.lonlat`---a `logical` variable that defines whether or not coordinates are in longitude/latitude or planar coordinates;
#'
#' @param .rlen,.rang Additional arguments for [`rstep()`].
#' * `.rlen`---A `function` that simulates step lengths (metres);
#' * `.rang`---A `function` that simulates turning angles (degrees);
#'
#' The first argument to both functions must be the number of values to simulate. Other arguments are passed via `...`.
#'
#' @param .len,.ang Additional arguments for [`cstep()`].
#' * `.len`---A `numeric` vector of lengths (e.g., from [`rlen()`]);
#' * `.ang`---A `numeric` vector of turning angles (e.g., from [`rangrw()`]);
#'
#' @param .clen,.cang,.dlen,.dang Additional arguments for [`dstep()`].
#' * `.clen`---A `function` that calculates step lengths between coordinate pairs, such as [`clen()`];
#' * `.cang`---(ignored);
#' * `.dlen`---A `function` that calculates the probability density of step lengths, such as [`dtruncgamma()`];
#' * `.dang`---(ignored);
#'
#' `.cang` and `.dang` arguments are not yet implemented and must be [`missing`].
#'
#' @param ... Arguments passed to/from functions.
#' * [`ssf()`], [`ssv()`]: not used;
#' * [`rbern()`], [`ddet()`]: not used;
#' * [`rdet()`], [`ddet()`]: passed to `.ddet` ( = [`ddet()`], wrapping [`ddetlogistic()`]). Dots must be used;
#' * [`ddetlogistic()`]: not used;
#' * [`rtruncgamma()`], [`dtruncgamma()`]: silently ignored;
#' * [`rwn()`]: silently ignored;
#' * [`rlen()`]: passed to [`rtruncgamma()`];
#' * [`rangrw()`]: passed to [`rwn()`];
#' * [`rangcrw()`]: passed to [`rwn()`], excluding `.mu`;
#' * [`rstep()`]: passed to `.rlen` and `.rang`;
#' * [`dstep()`]: not used;
#' * [`dstep()`]: passed to `.dlen` and `.cang`;
#' * [`clen()`]: not used;
#' * [`cang()`]: not used;
#'
#' @details
#'
#' # Reproducibility
#'
#' * [`ssf()`] sets a fixed seed (wrapping `set.seed(123L)`);
#' * [`ssv()`] sets a time-varying seed (wrapping `set.seed(as.integer(as.Date(Sys.time())))`) and is primarily intended for package testing;
#'
#' # Detections
#'
#' ## Distribution functions
#'
#' * `*bern()` functions are distribution functions for the Bernoulli distribution:
#'    * [`rbern()`] wraps [`stats::rbinom()`] and simulates values from a Bernoulli distribution;
#'    * [`dbern()`] wraps [`stats::dbinom()`] and evaluates the probability density of simulated values;
#'
#' ## Wrappers
#'
#' * `*det()` functions wrap `*bern()` functions for the simulation of detections at receivers:
#'    * [`rdet()`] wraps [`ddet()`] and [`rbern()`] to simulate detections;
#'    * [`ddet()`] wraps [`ddetlogistic()`] to calculate the probability density of detections;
#'    * [`ddetlogistic()`] effectively wraps [`dbern()`] and evaluates the probability density of a detection event (at receiver \eqn{k} at time \eqn{t}), given a transmission from location \eqn{\bold{s} = (x, y)} (i.e., \eqn{p(\text{ev}[k, t] | \bold{s})}), as a truncated \eqn{\text{logistic}} function of the distance (`.x`) between the transmission (\bold{s}) and receiver (\bold{r}) locations, i.e.,
#' \deqn{
#' p(\text{ev}[k, t] | \bold{s}) =  \text{Bernoulli}(\text{.prob}) \\
#' \text{.prob} =
#'  \begin{cases}
#'    \text{logistic}(\text{.alpha} + \text{.beta} \times .x) & \text{if } .x < \text{.gamma} \\
#'    0 & \text{otherwise}
#'  \end{cases}
#' }
#' where \eqn{\text{logistic}(x) = 1 + e^{-x}}; `.alpha`, and `.beta` are parameters; and `.gamma` is the receiver's detection range.
#'
#' # Movement
#'
#' ## Distribution functions
#'
#' * `*truncgamma()` functions are distribution functions for the truncated Gamma distribution:
#'    * [`rtruncgamma()`] simulates values from a truncated Gamma distribution with a `.mobility` parameter that truncates the right-hand side of the distribution;
#'    * [`dtruncgamma()`] returns the densit(ies) of specified values(s);
#
#' * `*wn()` functions are distribution functions for the wrapped normal distribution:
#'    * [`rwn()`] wraps [`circular::rwrappednormal()`] and simulates turning angle(s).
#'    * `dwn()` is not currently implemented.
#'
#' ## Step length and turning angle wrappers
#'
#' * `*len()` functions wrap `*truncgamma()` for the simulation of movement step lengths:
#'    * [`rlen()`] wraps [`rtruncgamma()`];
#'    * `dlen()` is not currently implemented;
#'    * [`clen()`] calculates the distance (step length) between coordinate pair(s) for [`dstep()`];
#'
#' * `*ang*()` functions wrap `*rw()` for turning angles:
#'    * [`rangrw()`] wraps [`rwn()`] and is used to simulate random-walk turning angles;
#'    * [`rangcrw()`] wraps [`rwn()`] with `.mu = .prior` (if specified) and is used to simulate correlated random walks;
#'    * `dangrw` and `dangcrw()` are not currently implemented;
#'    * [`cang()`] calculates turning angles between coordinate pairs;
#'
#' ## Step wrappers
#'
#' * [`rstep()`] wraps `.rlen =` [`rlen`] and `.rang =` [`rangrw`] to simulate coordinates (via [`cstep()`]);
#' * [`cstep()`] used (simulated) step lengths and turning angles to calculate coordinates;
#' * [`dstep()`] evaluates the probability density of movements between locations. At the time of writing, this only evaluates the density of step lengths and is only suitable for random walks;
#'
#' # Properties
#'
#' * All angles are in degrees (-180, 180):
#'    * North = 0 degrees
#'    * East = 90 degrees
#'    * South = 180 degrees
#'    * West = -90 degrees
#'
#' # Warnings
#'
#' * **Dots.** Most of the above low-level functions silently accept unused dots (`...`). In higher level functions (such as [`sim_path_walk()`], [`pf_rpropose_kick()`] and [`pf_dpropose()`]), unused dots produce an error or a warning.
#'
#' * **Correlated random walks.** It is possible to simulate correlated random walks in [`sim_path_walk()`] but at the time of writing this is not supported [`pf_forward()`]. Probability density functions for correlated random walks, as required for [`pf_forward()`] and [`pf_backward_sampler()`] (e.g., `dangrw()`, `dangcrw()`) are not implemented.
#'
#' @example man/examples/sim_helpers-examples.R
#' @seealso
#' * `sim_*` functions implement _de novo_ simulation of movements and observations:
#'    * [`sim_helpers`] are convenience functions for simulations;
#'    * [`sim_array()`] simulates acoustic array(s);
#'    * [`sim_path_walk()`] simulates movement path(s) via a walk model;
#'    * [`sim_detections()`] simulates detection(s) at receivers;
#'
#' * [`pf_forward()`] implements forward simulation-based reconstruction of movement paths;
#' * [`pf_backward_sampler()`] implements backward simulation-based reconstruction of movement paths;
#' * [`skill`] functions compared simulated and reconstructed patterns to evaluate model skill;
#'
#' @author Edward Lavender
#' @name sim_helpers

#' @rdname sim_helpers
#' @export

ssf <- function() {
  set.seed(123L)
}

#' @rdname sim_helpers
#' @export

ssv <- function() {
  set.seed(as.integer(as.Date(Sys.time())))
}

#' @rdname sim_helpers
#' @export

rbern <- function(.n, .prob) {
  stats::rbinom(n = .n, size = 1L, prob = .prob)
}

#' @rdname sim_helpers
#' @export

dbern <- function(.x, .prob) {
  stats::dbinom(x = .x, size = 1L, prob = .prob)
}

#' @rdname sim_helpers
#' @export

rdet <- function(.data, .ddet = ddet, ...) {
  pr <- detection <- NULL
  .data[, pr := .ddet(.data, ...)]
  .data[, detection := rbern(.N, .prob = pr)]
  .data
}

#' @rdname sim_helpers
#' @export

ddetlogistic <- function(.x,
                         .alpha = 4, .beta = -0.01,
                         .gamma = 750) {
  pr <- stats::plogis(.alpha + .beta * .x)
  pr[.x > .gamma] <- 0
  # pr <- dbern(.x = 1L, size = 1L, prob = pr)
  pr
}

#' @rdname sim_helpers
#' @export

ddet <- function(.data, .ddetx = ddetlogistic, ...) {
  check_names(.data, "dist")
  .ddetx(.data$dist, ...)
}

#' @rdname sim_helpers
#' @export

rtruncgamma <- function(.n = 1L, .shape = 15, .scale = 15, .mobility = 500, ...) {
  u <- stats::runif(.n, min = 0, max = 1)
  pmin(
    stats::qgamma(u * stats::pgamma(.mobility, shape = .shape, scale = .scale),
                  shape = .shape,
                  scale = .scale),
    .mobility)
}

#' @rdname sim_helpers
#' @export

dtruncgamma <- function(.x,
                        .shape = 15,
                        .scale = 15,
                        .mobility = 500, ...) {
  tt <- rep(0, length(.x))
  tt[.x <= .mobility] <-
    stats::dgamma(x = .x[.x <= .mobility], shape = .shape, scale = .scale) /
    stats::pgamma(q = .mobility, shape = .shape, scale = .scale)
  tt
}

#' @rdname sim_helpers
#' @export

rwn <- function(.n = 1L, .mu = 0, .rho = 0, .sd = 1, ...) {
  as.numeric(
    circular::rwrappednormal(
      n = .n,
      mu = degrees(.mu),
      rho = .rho,
      sd = .sd,
      control.circular = list(units = "degrees")
    )
  )
}

#' @rdname sim_helpers
#' @export

rlen <- function(.n = 1L,
                 .prior = NULL, .t = NULL, ...) {
  rtruncgamma(.n = .n, ...)
}

#' @rdname sim_helpers
#' @export

rangrw <- function(.n = 1L,
                   .prior = NULL, .t = NULL, ...) {
  rwn(.n = .n, ...)
}

#' @rdname sim_helpers
#' @export

rangcrw <- function(.n = 1L,
                    .prior = NULL, .t = NULL, ...) {
  if (is.null(.prior)) {
    .mu <- 0
  } else {
    .mu <- .prior
  }
  rwn(.n = .n, .mu = .mu, ...)
}

#' @rdname sim_helpers
#' @export

rstep <- function(.xy0,
                  .xy1 = matrix(NA, nrow = fnrow(.xy0), ncol = 2L),
                  .rlen = rlen, .rang = rangrw, ..., .lonlat) {
  # Simulate step lengths and turning angles
  n    <- fnrow(.xy0)
  rlen <- .rlen(n, ...)
  rang <- .rang(n, ...)
  # Step into new locations
  cstep(.xy0 = .xy0,
        .xy1 = .xy1,
        .len = rlen,
        .ang = rang,
        .lonlat = .lonlat)
}

#' @rdname sim_helpers
#' @export

cstep <- function(.xy0,
                  .xy1 = matrix(NA, nrow = fnrow(.xy0), ncol = 2L),
                  .len,
                  .ang,
                  .lonlat) {
  .xy0 <- as.matrix(.xy0)
  .xy1 <- as.matrix(.xy1)
  if (.lonlat) {
    .xy1 <- geosphere::destPoint(p = .xy0, b = .ang, d = .len)
  } else {
    .ang <- geoangle(.ang)
    .xy1[, 1] <- .xy0[, 1] + .len * cos(.ang)
    .xy1[, 2] <- .xy0[, 2] + .len * sin(.ang)
  }
  unname(.xy1)
}

#' @rdname sim_helpers
#' @export

dstep <- function(.xy0, .xy1,
                  .clen = clen, .cang,
                  .dlen = dtruncgamma, .dang, ...,
                  .lonlat) {
  stopifnot(missing(.cang))
  stopifnot(missing(.dang))
  .xy0 <- as.matrix(.xy0)
  .xy1 <- as.matrix(.xy1)
  # Calculate step length between selected location and all previous locations
  rlen <- .clen(.xy0 = .xy0, .xy1 = .xy1, .lonlat = .lonlat)
  # Calculate turning angle
  # rang <- .cang(.data_past = .data_past, .data_now = .data_now, .lonlat = .lonlat)
  # Translate step lengths and turning angles into probability densities
  .dlen(rlen, ...) # * .rang(.x = rang, ...)
}

#' @rdname sim_helpers
#' @export

clen <- function(.xy0, .xy1, .lonlat) {
  .xy0 <- as.matrix(.xy0)
  .xy1 <- as.matrix(.xy1)
  if (.lonlat) {
    terra::distance(.xy0,
                    .xy1,
                    lonlat = .lonlat,
                    pairwise = TRUE)
  } else {
    dist_2d(.x0 = .xy0[, 1], .y0 = .xy0[, 2],
            .x1 = .xy1[, 1], .y1 = .xy1[, 2])
  }
}

#' @rdname sim_helpers
#' @export

cang <- function(.xy0, .xy1, .lonlat) {
  .xy0 <- as.matrix(.xy0)
  .xy1 <- as.matrix(.xy1)
  if (.lonlat) {
    geosphere::bearing(p1 = .xy0, p2 = .xy1)
  } else {
    cang_planar(.xy0 = .xy0, .xy1 = .xy1, .convention = "180")
  }
}

#' @title Simulation: acoustic arrays
#' @description This function simulates acoustic arrays (i.e., networks of acoustic receiver(s)) on a grid.
#' @param .bathy A [`SpatRaster`] that defines the region of interest. Receivers are not simulated in `NA` regions.
#' @param .lonlat A `logical` variable that defines whether or not coordinates on `.bathy` are in longitudes/latitude or planar format. This input controls the output name of the coordinate columns (see Value).
#' @param .arrangement,.n_receiver,... Arguments passed to [`terra::spatSample()`].
#' * `.arrangement` is a `character` that defines the receiver arrangement (passed to the `method` argument).
#' * `.n_receiver` is an `integer` that defines the number of receivers to simulate (passed to the `size` argument).
#' * `...` ... Additional arguments.
#' @param .receiver_start,.receiver_end,.receiver_range (optional) Additional columns to include in the output:
#'  * `.receiver_start` and `.receiver_end` are `Date` or `POSIXct` inputs that specify the deployment time period;
#'  * `.receiver_range` is a `numeric` input that defines the detection range;
#'
#'  Single inputs are expected to these arguments, which are constant across all receivers.
#'
#' @param .n_array An `integer` that defines the number of array designs to simulate with the aforementioned parameters.
#' @param .plot A `logical` variable that defines whether or not to plot simulated arrays.
#' @param .one_page If `.plot = TRUE`, `.one_page` is a `logical` variable that defines whether or not to produce plots on a single page.
#'
#' @details
#' Receiver locations are simulated using [`terra::spatSample()`].
#'
#' This function replaces [`flapper::sim_array()`](https://edwardlavender.github.io/flapper/reference/sim_array.html).
#'
#' @return The function returns a `data.table` with the following columns:
#' * `array_id`---an `integer` vector of array IDs,
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `receiver_easting` (if `.lonlat = FALSE`) or `receiver_lon` (if `.lonlat = TRUE`)---a `numeric` vector that defines receiver x coordinates;
#' * `receiver_northing` (if `.lonlat = FALSE`) or `receiver_lat` (if `.lonlat = TRUE`)---a `numeric` vector that defines receiver y coordinates;
#' * `receiver_start`---receiver start dates (if defined);
#' * `receiver_end`---receiver end dates (if defined);
#' * `receiver_range`---receiver detection ranges (if defined);
#'
#' @examples
#' #### Example (1): The default implementation
#' # The function returns a data.table
#' a <- sim_array()
#' a
#'
#' #### Example (2): Customise receiver placement/number
#' a <- sim_array(.arrangement = "regular", .n_receiver = 100)
#'
#' #### Example (3): Add additional columns for downstream functions
#' a <- sim_array(.receiver_start = as.Date("2013-01-01"),
#'                .receiver_end = as.Date("2014-01-01"),
#'                .receiver_range = 750)
#'
#' #### Example (4): Control the plot(s)
#' sim_array(.plot = FALSE)
#' sim_array(.n_array = 5L, .plot = TRUE, .one_page = TRUE)
#' sim_array(.n_array = 5L, .plot = TRUE, .one_page = FALSE)
#'
#' @inherit sim_helpers seealso
#' @author Edward Lavender
#' @export

sim_array <- function(.bathy = spatTemplate(), .lonlat = FALSE,
                      .arrangement = "random", .n_receiver = 10L, ...,
                      .receiver_start = NULL, .receiver_end = NULL, .receiver_range = NULL,
                      .n_array = 1L,
                      .plot = TRUE, .one_page = FALSE) {
  #### Check user inputs
  # Check dots
  check_dots_for_missing_period(formals(), list(...))
  check_dots_allowed(c("x", "size", "method", "replace", "na.rm", "xy",
                       "cells", "values"))
  # * Check receiver_end is after receiver_start (if supplied)
  lapply(list(.receiver_start, .receiver_end, .receiver_range), \(x) {
    if (!is.null(x) && length(x) != 1L) {
      abort("Single inputs are expected for `.receiver_start`, `.receiver_end` and `.receiver_range`.")
    }
  })
  if (!is.null(.receiver_start) && !is.null(.receiver_end)) {
    if (.receiver_end <= .receiver_start) {
      warn("`.receiver_end` should be after `.receiver_start`.")
    }
  }

  #### Simulate arrays
  arrays <-
    lapply(seq_len(.n_array), function(i) {
      # Sample receiver locations
      array <-
        .bathy |>
        terra::spatSample(size = .n_receiver,
                          method = .arrangement, replace = FALSE,
                          na.rm = TRUE, xy = TRUE, cells = TRUE, values = FALSE, ...) |>
        as.data.table() |>
        mutate(array_id = i,
               receiver_id = as.integer(row_number())) |>
        select("array_id", "receiver_id", "x", "y") |>
        as.data.table()
      # Add optional columns
      if (!is.null(.receiver_start)) {
        receiver_start <- NULL
        array[, receiver_start := .receiver_start]
      }
      if (!is.null(.receiver_end)) {
        receiver_end <- NULL
        array[, receiver_end := .receiver_end]
      }
      if (!is.null(.receiver_range)) {
        receiver_range <- NULL
        array[, receiver_range := .receiver_range]
      }
      # Return simulated array (moorings data.table)
      array
    })

  #### Plot arrays
  if (.plot) {
    pp <- one_page(.one_page, length(arrays))
    on.exit(par(pp), add = TRUE)
    lapply(seq_len(length(arrays)), function(i) {
      terra::plot(.bathy, main = paste("Array", i))
      points(arrays[[i]]$x, arrays[[i]]$y)
    }) |> invisible()
  }

  #### Return outputs
  arrays <-
    arrays |>
    rbindlist()
  if (.lonlat) {
    arrays <-
      arrays |>
      rename(receiver_lon = "x", receiver_lat = "y") |>
      as.data.table()
  } else {
    arrays <-
      arrays |>
      rename(receiver_easting = "x", receiver_northing = "y") |>
      as.data.table()
  }
  arrays
}

#' @title Simulation: movement walks
#' @description [`sim_path_walk()`] facilitates the simulation of discrete-time animal movement paths from walk models (e.g., random walks, biased random walks, correlated random walks).
#'
#' @param .bathy A [`SpatRaster`] that defines the region within which movements are simulated. Movements are simulated in continuous space but restricted within the boundaries defined by `.bathy` and non-`NA` regions.
#' @param .lonlat A `logical` variable that defines whether or not `.bathy` uses longitude/latitude or planar coordinates.
#' @param .origin (optional) A one-row, two-column matrix that defines the origin. If unsupplied, `.origin` is sampled at random from `.bathy`. One origin is used for all simulated paths (see `.n_path`).
#' @param .n_step An `integer` that defines the number of time steps.
#' @param .timestamp (optional) A vector of time stamps, one for each time step, for inclusion in the output [`data.table`] as a `timestamp` column.
#' @param .rlen,.rang,... Functions and accompanying arguments that simulate step lengths and turning angles. Simulated step lengths should be in map units (e.g., metres) if `.lonlat = FALSE` or metres if `.lonlat = TRUE`. Turning angles should be in degrees. The functions must accept four named arguments, even if unused:
#' * `.n`---an `integer` that defines the number of simulated outcome(s);
#' * `.prior`---a `numeric` vector that defines the simulated value(s) from the previous time step;
#' * `.t`---an `integer` that defines the time step;
#' * `...`---additional arguments, if needed;
#'
#' If `.prior` is used, the function should be able to handle the first time step (when `.prior` is set to `NULL`). See [`rangcrw()`] (below) for an example.
#'
#' Note that `...` is passed down from [`sim_path_walk()`] to both `.rlen` and `.rang` so care is required to ensure that `...` parameters are handled correctly.
#'
#' The following template functions are provided:
#' * [`rlen()`] is an example `.rlen` function that simulates step lengths from a truncated Gamma distribution (via [`rtruncgamma()`]);
#' * [`rangrw()`] is an example `.rang` function that simulates uncorrelated turning angles from a wrapped normal distribution (via [`rwn()`]);
#' * [`rangcrw()`] is an example `.rang` function that can simulate correlated turning angles (via [`rwn()`]);
#'
#' @param .n_path An `integer` that defines the number of paths to simulate.
#' @param .plot,.one_page Plot options.
#' * `.plot` is a `logical` variable that defined whether or not to plot `.bathy` and simulated path(s). Each path is plotted on a separate plot.
#' * `.one_page` is a logical variable that defines whether or not to produce all plots on a single page.
#'
#' @details
#'
#' The following convenience functions are provided:
#' * [`rtruncgamma()`] and [`rwn()`] simulate step lengths (from a truncated Gamma distribution) and turning angles (from a wrapped normal distribution);
#' * [`rlen()`], [`rangrw()`] and [`rangcrw()`] are wrappers in the form required by [`sim_path_walk()`];
#' * [`sim_path_walk()`] simulates the movement path(s);
#'
#' Within [`sim_path_walk()`], at each time step, if `.lonlat = FALSE`, current locations (x, y) are updated via `x + length * cos(angle)` and `y + length * sin(angle)`.
#'
#' If `.lonlat = TRUE`, current locations are updated via [`geosphere::destPoint()`].
#'
#' `.lonlat` support is experimental. Be especially careful with correlated random walks if `lonlat = TRUE`. On an ellipsoid, the initial (simulated) bearing is not the same as the final bearing, but is not currently updated.
#'
#' [`sim_path_walk()`] replaces [`flapper::sim_path_sa()`](https://edwardlavender.github.io/flapper/reference/sim_path_sa.html). Other [`flapper::sim_path_*()`](https://edwardlavender.github.io/flapper/reference/sim_path_-times.html) functions are not currently implemented in [`patter`].
#'
#' @return [`sim_path_walk()`] returns a [`data.table`] with 10 columns:
#' * `path_id`--- an `integer` vector that identifies each path;
#' * `timestep`---an `integer` vector that defines the time step;
#' * `cell_id`, `cell_x`, `cell_y`, `cell_z`---`integer`/`numeric` vectors that define the locations of the simulated positions on `.bathy`;
#' * `x`,`y`---`numeric` vectors that define simulated x and y coordinates;
#' * `length`,`angle`---`numeric` vectors that define simulated step lengths and angles (for the movement from timestep `t` to time step `t + 1`);
#'
#' @example man/examples/sim_path_walk-examples.R
#'
#' @inherit sim_helpers seealso
#' @author Edward Lavender
#' @name sim_path_walk
NULL

#' @rdname sim_path_walk
#' @export

sim_path_walk <- function(.bathy = spatTemplate(), .lonlat = FALSE,
                          .origin = NULL,
                          .n_step = 10L, .timestamp = NULL,
                          .rlen = rlen, .rang = rangrw, ...,
                          .n_path = 1L,
                          .plot = TRUE, .one_page = FALSE) {
  # Check user inputs
  rlang::check_dots_used()
  if (.lonlat) {
    rlang::check_installed("geosphere")
  }

  # Implement simulation
  out <- .sim_path_flux(.bathy = .bathy,
                        .lonlat = .lonlat,
                        .origin = .origin,
                        .n_step = .n_step,
                        .move = .cstep_using_flux,
                        .flux_vals = .flux_template(.n_step, .n_path),
                        .rlen = .rlen, .rang = .rang, ...,
                        .n_path = .n_path,
                        .plot = .plot, .one_page = .one_page)
  # Collect flux parameters (step lengths & turning angles)
  params <- attr(out, "flux")
  length <- .flux_pivot(params$length, .n_step = .n_step, .n_path = .n_path)
  angle  <- .flux_pivot(params$angle, .n_step = .n_step, .n_path = .n_path)
  # Update data.table with flux parameters & tidy
  out <-
    out |>
    merge(length, by = c("path_id", "timestep")) |>
    rename(length = "value") |>
    merge(angle, by = c("path_id", "timestep")) |>
    rename(angle = "value") |>
    as.data.table()
  if (!is.null(.timestamp)) {
    timestamp <- NULL
    out[, timestamp := .timestamp[out$timestep]]
  }
  # Select column order
  out |>
    select("path_id", "timestep", any_of("timestamp"),
           "length", "angle",
           "x", "y",
           "cell_x", "cell_y", "cell_z", "cell_id",
           ) |>
    as.data.table()
}

#' @title Simulation: acoustic detections
#' @description These functions facilitate the simulation of detections, arising from animal movement path(s), at passive acoustic telemetry receiver(s).
#'
#' @param .paths A [`data.table`] that defines movement path(s) (e.g., from [`sim_path_walk()`]). This should contain the following columns:
#' * (optional) `path_id`---an `integer` vector that identifies paths (if the number of paths > 1);
#' * `timestep`---an `integer` vector that defines time steps;
#' * `x`,`y`---`numeric` vectors that define path coordinates;
#' @param .arrays A [`data.table`] that defines array(s) in which to simulate detections. This should contain the following columns:
#' * (optional) `array_id`---an `integer` vector that identifies arrays (if the number of arrays > 1);
#' * `receiver_id`---a vector that identifies receivers;
#' * `receiver_easting` and `receiver_northing` or `receiver_lon` and `receiver_lat` (if `.lonlat = TRUE`)---`numeric` vectors that define receiver coordinates;
#' @param .calc_distance,.lonlat Distance arguments.
#' * `.calc_distance` is a function that calculates distances between points along a selected path and receiver locations (for a specific array) (e.g., [`terra::distance()`]). This should accept:
#' * a matrix of path coordinates (for a selected path);
#' * a matrix of receiver coordinates (for a specific array);
#' * `lonlat`, a `logical` variable that defines whether or not path/array coordinates are in longitude/latitude format (defined by `.lonlat`);
#' @param .rdet,... A `function`, and associated arguments, that simulate detections (such as [`rdet()`]. A [`data.table`] is passed to this function (in the form `.rdet(.data, ...)`) that defines, for each path point, the distance to each corresponding receiver (in a column called `dist`). All other variables in `.paths` and `.arrays` are also available in this [`data.table`]. This makes it possible to specify a wide variety of detection probability models (see Examples). The function should return the inputted [`data.table`] with an `integer` `detection` column that defines detections (0, 1).
#' @param .type If `.paths` and `.arrays` contain multiple paths/arrays, `.type` is a `character` that defines whether or not to simulate detections for each path/array pair (`.type = "pairwise"`) or for all combinations of paths/arrays (`type = "combinations"`).
#' @param .return (optional) A `character` vector that defines column names retained in the output. `NULL` retains all columns in `.paths` and `.arrays` plus internally computed columns:
#' * `dist`---the distance between points on the path(s) and the receiver(s) that recorded detections;
#' * Columns computed by `.rdet`, such as `pr` (the probability of detection at receivers that recorded detections), in the case of `.rdet =` [`rdet`].
#'
#' @details
#' [`sim_detections()`] implements the simulation. This requires the movement path(s) and array(s) in which detections are simulated to be provided as [`data.table`]s. If multiple paths and/or arrays are provided, the function simulates detections for each path/array pair (if `.type = "pairwise"`) or for all combinations of arrays and paths (if `.type = "combinations`). Detections are simulated in three steps:
#' * A distance function (`.calc_distance`) is used to calculate distances between points along a selected path and receiver(s):
#' * A random generation function (`.rdet`) is used to simulate detections, given distances and other information in `.paths` and `.arrays`;
#'
#' In the output, only detections are retained (as in 'real-world' datasets).
#'
#' These functions replace [`flapper::sim_detections()`](https://edwardlavender.github.io/flapper/reference/sim_detections.html) and [`flapper::get_detection_pr()`](https://edwardlavender.github.io/flapper/reference/get_detection_pr.html).
#'
#' @return [`sim_detections()`] returns a [`data.table`] with columns specified by `.return`.
#'
#' @example man/examples/sim_detections-examples.R
#'
#' @inherit sim_helpers seealso
#' @author Edward Lavender
#' @name sim_detections
NULL

#' @rdname sim_detections
#' @export

sim_detections <- function(.paths, .arrays,
                           .calc_distance = terra::distance, .lonlat = FALSE,
                           .rdet = rdet, ...,
                           .type = c("pairwise", "combinations"),
                           .return = c("array_id", "path_id",
                                       "timestep", "timestamp", "receiver_id",
                                       "dist", "pr")
) {

  #### Check user inputs
  rlang::check_dots_used()
  check_inherits(.paths, "data.table")
  check_inherits(.arrays, "data.table")
  check_names(.paths, c("timestep", "x", "y"))
  check_names(.arrays, "receiver_id")
  if (!rlang::has_name(.paths, "path_id")) {
    path_id <- NULL
    .paths[, path_id := 1L]
  }
  if (!rlang::has_name(.arrays, "array_id")) {
    array_id <- NULL
    .arrays <- .arrays[, array_id := 1L]
  }
  check_new_colnames(.arrays, c("receiver_x", "receiver_y"))
  receiver_x <- receiver_y <- NULL
  if (!.lonlat) {
    check_names(.arrays, c("receiver_easting", "receiver_northing"))
    receiver_easting <- receiver_northing <- NULL
    .arrays[, receiver_x := receiver_easting]
    .arrays[, receiver_y := receiver_northing]
  } else {
    check_names(.arrays, c("receiver_lon", "receiver_lat"))
    receiver_lon <- receiver_lat <- NULL
    .arrays[, receiver_x := receiver_lon]
    .arrays[, receiver_y := receiver_lat]
  }
  .type <- match.arg(.type)
  if (.type == "pairwise") {
    if (length(unique(.arrays$array_id)) != length(unique(.paths$path_id))) {
      abort("`.type = 'pairwise'` requires the number of arrays and paths to be identical.")
    }
  }

  #### Simulate detections
  # Define arguments for simulation
  args <- list(.path = NULL, .array = NULL,
               .calc_distance = .calc_distance, .lonlat = .lonlat,
               .rdet = .rdet, ...,
               .return = .return)
  # Implement simulation (pairwise or for all combinations)
  if (.type == "pairwise") {

    # Loop over each path and array pair and simulate detections
    out <- pbapply::pbmapply(function(.path, .array) {
      .sim_detections_call(.path = .path, .array = .array, .args = args)
    },
    split(.paths, .paths$path_id),
    split(.arrays, .arrays$array_id),
    SIMPLIFY = FALSE
    )

  } else if (.type == "combinations") {

    # For each array, simulate detections for each path
    out <- pbapply::pblapply(split(.arrays, .arrays$array_id), function(.array) {
      lapply(split(.paths, .paths$path_id), function(.path) {
        .sim_detections_call(.path = .path, .array = .array, .args = args)
      }) |> rbindlist()
    })
  }

  #### Return outputs
  out |>
    rbindlist() |>
    arrange("array_id", "path_id", "timestep") |>
    as.data.table()

}
