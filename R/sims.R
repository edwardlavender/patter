#' @title Simulate an acoustic array
#' @description This function simulates acoustic receivers on a grid.
#' @param .bathy A [`SpatRaster`] that defines the region of interest.
#' @param .arrangement A character that defines the receiver arrangement (passed to the `method` argument of [`terra::spatSample()`]).
#' @param .n_receivers An `integer` that defines the number of receivers (passed to the `size` argument of [`terra::spatSample()`]).
#' @param ... Additional arguments passed to [`terra::spatSample()`].
#' @param .receiver_start,.receiver_end,.receiver_range (optional) Additional columns to include in the output:
#'  * `.receiver_start` and `.receiver_end` specify the deployment time period;
#'  * `.receiver_range` defines the detection range;
#'
#'  Single inputs are expected to these arguments, which are constant across all receivers.
#'
#' @param .n_arrays An `integer` that defines the number of array designs to simulate.
#' @param .plot A `logical` variable that defines whether or not to plot simulated arrays.
#' @param .one_page If `.plot = TRUE`, `.one_page` is a `logical` variable that defines whether or not to produce plots on a single page.
#'
#' @details
#' Receiver locations are simulated using [`terra::spatSample()`].
#'
#' @return The function returns a `list`. Each element of the list is a `data.table` with the following columns:
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `receiver_easting`---a `numeric` vector that defines receiver x coordinates;
#' * `receiver_northing`---a `numeric` vector that defines receiver y coordinates;
#' * `receiver_start`---receiver start dates (if defined);
#' * `receiver_end`---receiver end dates (if defined);
#' * `receiver_range`---receiver detection ranges (if defined);
#'
#' @examples
#' #### Example (1): The default implementation
#' # The function returns a list, with one element for each simulated array
#' a <- sim_array()
#' summary(a)
#'
#' #### Example (2): Customise receiver placement/number
#' a <- sim_array(.arrangement = "regular", .n_receivers = 100)
#'
#' #### Example (3): Add additional columns for downstream functions
#' a <- sim_array(.receiver_start = as.Date("2013-01-01"),
#'                .receiver_end = as.Date("2014-01-01"),
#'                .receiver_range = 500)
#'
#' #### Example (4): Control the plot(s)
#' sim_array(.plot = FALSE)
#' sim_array(.n_arrays = 5L, .plot = TRUE, .one_page = TRUE)
#' sim_array(.n_arrays = 5L, .plot = TRUE, .one_page = FALSE)
#'
#' @author Edward Lavender
#' @export

sim_array <- function(.bathy = rast_template(),
                      .arrangement = "random", .n_receivers = 10L, ...,
                      .receiver_start = NULL, .receiver_end = NULL, .receiver_range = NULL,
                      .n_arrays = 1L,
                      .plot = TRUE, .one_page = FALSE) {
  #### Check user inputs
  # * Check ... are not implement internally
  # * Check receiver_end is after receiver_start (if supplied)

  #### Simulate arrays
  arrays <-
    lapply(seq_len(.n_arrays), function(i) {
      # Sample receiver locations
      array <-
        .bathy |>
        terra::spatSample(size = .n_receivers,
                          method = .arrangement, replace = FALSE,
                          na.rm = TRUE, xy = TRUE, cells = TRUE, values = FALSE, ...) |>
        as.data.table() |>
        mutate(receiver_id = as.integer(dplyr::row_number())) |>
        select("receiver_id", receiver_easting = "x", receiver_northing = "y") |>
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
    on.exit(graphics::par(pp), add = TRUE)
    lapply(seq_len(length(arrays)), function(i) {
      terra::plot(.bathy, main = paste("Array", i))
      graphics::points(arrays[[i]]$receiver_easting, arrays[[i]]$receiver_northing)
    }) |> invisible()
  }

  #### Return outputs
  arrays
}

#' @title Simulate movement paths
#' @description These functions facilitate the simulation of animal movement paths from walk models (e.g., random walks, biased random walks, correlated random walks).
#'
#' @param .bathy A [`SpatRaster`] that defines the region within which movements are simulated. Movements are simulated in continuous space but restricted within the boundaries defined by `.bathy` and non-NA regions.
#' @param .origin (optional) A one-row, two-column matrix that defines the origin. If unsupplied, `.origin` is sampled at random from `.bathy`. One origin is used for all simulated paths (see `.n_path`).
#' @param .n_step An `integer` that defines the number of steps.
#' @param .sim_length,.sim_angle,... Functions and accompanying arguments that simulate step lengths and turning angles. These must accept four named arguments, even if unused:
#' * `.n`---an `integer` that defines the number of simulated outcome(s) (defined internally);
#' * `.prior`---a `numeric` vector that defines the simulated value(s) from the previous time step;
#' * `.t`---an `integer` that defines the time step;
#' * `...`---additional arguments, if needed;
#'
#' If `.prior` is used, the function should be able to handle the first time step (when `.prior` is set to `NULL`). See [`sim_angle_crw()`] (below) for an example.
#'
#' Note that `...` is passed down from [`sim_path_walk()`] to both `.sim_length` and `sim_angle` so care is required to ensure that `...` parameters are used by the correct function(s).
#'
#' The following template functions are provided:
#' * [`sim_length()`] is an example `.sim_length` function that simulated step lengths from a truncated Gamma distribution (via [`rtruncgamma()`]);
#' * [`sim_angle_rw()`] is an example `.sim_angle` function that simulates uncorrelated turning angles from a wrapped normal distribution (via [`rwn()`]);
#' * [`sim_angle_crw()`] is an example `.sim_angle` function that can simulate correlated turning angles (via [`rwn()`]);
#'
#' @param .n_path An `integer` that defines the number of paths to simulated.
#' @param .plot,.one_page Plot options.
#' * `.plot` is a `logical` variable that defined whether or not to plot `.bathy` and simulated path(s). Each plot is created on a separate panel.
#' * `.one_page` is a logical variable that defines whether or not to produce all plots on a single page.
#'
#' @param .n,.prior,.t Arguments required for `.sim_length` and `.sim_step` functions (defined above).
#' @param .shape,.scale,.mobility Arguments for [`rtruncgamma()`] for the simulation of step lengths.
#' * `.shape` is a `numeric` value that defines the shape parameter of a Gamma distribution (see [`stats::rgamma()`]).
#' * `.scale` is a `numeric` value that defines the scale parameter of a Gamma distribution (see [`stats::rgamma()`]).
#' * `.mobility` is a `numeric` value that defines the maximum length (see [`truncdist::rtrunc()`]).
#' @param .mu,.rho,.sd Arguments for [`rwn`] for the simulation of turning angles, passed to the `mu`, `rho` and `sd` arguments of [`circular::rwrappednormal()`].
#'
#' @details
#'
#' * [`rtruncgamma()`] and [`rwn()`] simulate step lengths (from a truncated Gamma distribution) and turning angles (from a wrapped normal distribution);
#' * [`sim_length()`], [`sim_angle_rw()`] and [`sim_angle_crw()`] are wrappers in the form required by [`sim_path_walk()`];
#' * [`sim_path_walk()`] simulates the movement path(s);
#'
#' @return [`sim_path_walk()`] returns a [`data.table`] with 10 columns:
#' * `path_id`--- an `integer` that identifies each path;
#' * `timestep`---an `integer` that defines the time step;
#' * `cell_id`, `cell_x`, `cell_y`, `cell_z`---`integer`/`numeric` vectors that define the locations of the simulated positions on `.bathy`;
#' * `x`,`y`---`numeric` vectors that define simulated x and y coordinates;
#' * `length`,`angle`---`numeric` vectors that define simulated step lengths and angles (for the movement from timestep `t` to time step `t + 1`);
#' @example man/examples/sim_path_walk-examples.R
#'
#' @author Edward Lavender
#' @name sim_path_walk
NULL

#' @rdname sim_path_walk
#' @export

sim_path_walk <- function(.bathy = rast_template(),
                          .origin = NULL,
                          .n_step = 10L,
                          .sim_length = sim_length, .sim_angle = sim_angle_rw, ...,
                          .n_path = 1L,
                          .plot = TRUE, .one_page = FALSE) {
  # Check user inputs
  check_dots_for_missing_period(formals(), list(...))
  # Define flux function
  # * Define this within sim_path_walk() for correct handling of ...

  .flux <- function(.fv, .row, .col) {
    n <- length(.row)
    if (.col == 1L) {
      prior_length <- prior_angle <- NULL
    } else {
      prior_length <- .fv$length[.row, .col - 1, with = FALSE] |> dplyr::pull()
      prior_angle  <- .fv$angle[.row, .col - 1, with = FALSE] |> dplyr::pull()
    }
    .fv$length[.row, (.col) := .sim_length(.n = n, .prior = prior_length, .t = .col, ...)]
    .fv$angle[.row, (.col) := .sim_angle(.n = n, .prior = prior_angle, .t = .col, ...)]
  }
  # Implement simulation
  out <- .sim_path_flux(.bathy = .bathy,
                        .origin = .origin,
                        .n_step = .n_step,
                        .move = .step_using_flux,
                        .flux = .flux,
                        .flux_vals = .flux_template(.n_step, .n_path),
                        .n_path = .n_path,
                        .plot = .plot, .one_page = .one_page)
  # Collect flux parameters (step lengths & turning angles)
  params <- attr(out, "flux")
  length <- .flux_pivot(params$length, .n_step = .n_step, .n_path = .n_path)
  angle  <- .flux_pivot(params$angle, .n_step = .n_step, .n_path = .n_path)
  # Update data.table with flux parameters & tidy
  out |>
    merge(length, by = c("path_id", "timestep")) |>
    dplyr::rename(length = "value") |>
    merge(angle, by = c("path_id", "timestep")) |>
    dplyr::rename(angle = "value") |>
    as.data.table() |>
    # Select column order
    select("path_id", "timestep",
           "length", "angle",
           "x", "y",
           "cell_x", "cell_y", "cell_z", "cell_id",
           ) |>
    as.data.table()
}

#' @rdname sim_path_walk
#' @export

rtruncgamma <- function(.n = 1, .shape = 15, .scale = 15, .mobility = 500, ...) {
  truncdist::rtrunc(.n, "gamma", a = 0, b = .mobility,
                    shape = .shape, scale = .scale)
}

#' @rdname sim_path_walk
#' @export

rwn <- function(.n = 1, .mu = 0, .rho = 0, .sd = 1, ...) {
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

#' @rdname sim_path_walk
#' @export

sim_length <- function(.n = 1,
                       .prior = NULL, .t = NULL, ...) {
  rtruncgamma(.n = .n, ...)
}

#' @rdname sim_path_walk
#' @export

sim_angle_rw <- function(.n = 1,
                         .prior = NULL, .t = NULL, ...) {
  rwn(.n = .n, ...)
}

#' @rdname sim_path_walk
#' @export

sim_angle_crw <- function(.n = 1,
                          .prior = NULL, .t = NULL, ...) {
  if (is.null(.prior)) {
    .mu <- 0
  } else {
    .mu <- .prior
  }
  rwn(.n = .n, .mu = .mu, ...)
}

