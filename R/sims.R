#' @title Simulate an acoustic array
#' @description This function simulates acoustic receivers on a grid.
#' @param .bathy A [`SpatRaster`] that defines the region of interest.
#' @param .lonlat A `logical` variable that defines whether or not coordinates on `.bathy` are longitudes/latitudes. This input controls the output name of the coordinates column (see Value).
#' @param .arrangement A character that defines the receiver arrangement (passed to the `method` argument of [`terra::spatSample()`]).
#' @param .n_receiver An `integer` that defines the number of receivers (passed to the `size` argument of [`terra::spatSample()`]).
#' @param ... Additional arguments passed to [`terra::spatSample()`].
#' @param .receiver_start,.receiver_end,.receiver_range (optional) Additional columns to include in the output:
#'  * `.receiver_start` and `.receiver_end` are `Date` or `POSIXct` inputs that specify the deployment time period;
#'  * `.receiver_range` is a `numeric` input that defines the detection range;
#'
#'  Single inputs are expected to these arguments, which are constant across all receivers.
#'
#' @param .n_array An `integer` that defines the number of array designs to simulate.
#' @param .plot A `logical` variable that defines whether or not to plot simulated arrays.
#' @param .one_page If `.plot = TRUE`, `.one_page` is a `logical` variable that defines whether or not to produce plots on a single page.
#'
#' @details
#' Receiver locations are simulated using [`terra::spatSample()`].
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
#' @author Edward Lavender
#' @export

sim_array <- function(.bathy = rast_template(), .lonlat = FALSE,
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
               receiver_id = as.integer(dplyr::row_number())) |>
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
    on.exit(graphics::par(pp), add = TRUE)
    lapply(seq_len(length(arrays)), function(i) {
      terra::plot(.bathy, main = paste("Array", i))
      graphics::points(arrays[[i]]$x, arrays[[i]]$y)
    }) |> invisible()
  }

  #### Return outputs
  arrays <-
    arrays |>
    rbindlist()
  if (.lonlat) {
    arrays <-
      arrays |>
      dplyr::rename(receiver_lon = "x", receiver_lat = "y") |>
      as.data.table()
  } else {
    arrays <-
      arrays |>
      dplyr::rename(receiver_easting = "x", receiver_northing = "y") |>
      as.data.table()
  }
  arrays
}

#' @title Simulate movement paths
#' @description These functions facilitate the simulation of discrete-time animal movement paths from walk models (e.g., random walks, biased random walks, correlated random walks).
#'
#' @param .bathy A [`SpatRaster`] that defines the region within which movements are simulated. Movements are simulated in continuous space but restricted within the boundaries defined by `.bathy` and non-NA regions.
#' @param .lonlat A `logical` variable that defines whether or not `.bathy` uses longitude/latitude coordinates.
#' @param .origin (optional) A one-row, two-column matrix that defines the origin. If unsupplied, `.origin` is sampled at random from `.bathy`. One origin is used for all simulated paths (see `.n_path`).
#' @param .n_step An `integer` that defines the number of time steps.
#' @param .sim_length,.sim_angle,... Functions and accompanying arguments that simulate step lengths and turning angles. Simulated step lengths should be in map units (e.g., metres) if `.lonlat = FALSE` or metres if `.lonlat = TRUE`. Turning angles should be in degrees. The functions must accept four named arguments, even if unused:
#' * `.n`---an `integer` that defines the number of simulated outcome(s);
#' * `.prior`---a `numeric` vector that defines the simulated value(s) from the previous time step;
#' * `.t`---an `integer` that defines the time step;
#' * `...`---additional arguments, if needed;
#'
#' If `.prior` is used, the function should be able to handle the first time step (when `.prior` is set to `NULL`). See [`sim_angle_crw()`] (below) for an example.
#'
#' Note that `...` is passed down from [`sim_path_walk()`] to both `.sim_length` and `.sim_angle` so care is required to ensure that `...` parameters are handled correctly).
#'
#' The following template functions are provided:
#' * [`sim_length()`] is an example `.sim_length` function that simulates step lengths from a truncated Gamma distribution (via [`rtruncgamma()`]);
#' * [`sim_angle_rw()`] is an example `.sim_angle` function that simulates uncorrelated turning angles from a wrapped normal distribution (via [`rwn()`]);
#' * [`sim_angle_crw()`] is an example `.sim_angle` function that can simulate correlated turning angles (via [`rwn()`]);
#'
#' @param .n_path An `integer` that defines the number of paths to simulate.
#' @param .plot,.one_page Plot options.
#' * `.plot` is a `logical` variable that defined whether or not to plot `.bathy` and simulated path(s). Each path is plotted on a separate plot.
#' * `.one_page` is a logical variable that defines whether or not to produce all plots on a single page.
#'
#' @param .n,.prior,.t Arguments required for `.sim_length` and `.sim_step` functions (defined above).
#' @param .shape,.scale,.mobility Arguments for [`rtruncgamma()`] for the simulation of step lengths:
#' * `.shape` is a `numeric` value that defines the shape parameter of a Gamma distribution (see [`stats::rgamma()`]).
#' * `.scale` is a `numeric` value that defines the scale parameter of a Gamma distribution (see [`stats::rgamma()`]).
#' * `.mobility` is a `numeric` value that defines the maximum length (see [`truncdist::rtrunc()`]).
#' @param .mu,.rho,.sd Arguments for [`rwn`] for the simulation of turning angles, passed to the `mu`, `rho` and `sd` arguments of [`circular::rwrappednormal()`].
#'
#' @details
#'
#' The following convenience functions are provided:
#' * [`rtruncgamma()`] and [`rwn()`] simulate step lengths (from a truncated Gamma distribution) and turning angles (from a wrapped normal distribution);
#' * [`sim_length()`], [`sim_angle_rw()`] and [`sim_angle_crw()`] are wrappers in the form required by [`sim_path_walk()`];
#' * [`sim_path_walk()`] simulates the movement path(s);
#'
#' Within [`sim_path_walk()`], at each time step, if `.lonlat = FALSE`, current locations (x, y) are updated via `x + length * cos(angle)` and `y + length * sin(angle)`.
#'
#' If `.lonlat = TRUE`, current locations are updated via [`geosphere::destPoint()`].
#'
#' `.lonlat` support is experimental. Be especially careful with correlated random walks if `lonlat = TRUE`. On an ellipsoid, the initial (simulated) bearing is not the same as the final bearing, but is not currently updated.
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

sim_path_walk <- function(.bathy = rast_template(), .lonlat = FALSE,
                          .origin = NULL,
                          .n_step = 10L,
                          .sim_length = sim_length, .sim_angle = sim_angle_rw, ...,
                          .n_path = 1L,
                          .plot = TRUE, .one_page = FALSE) {
  # Check user inputs
  check_dots_for_missing_period(formals(), list(...))
  if (.lonlat) {
    rlang::check_installed("geosphere")
  }
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
                        .lonlat = .lonlat,
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


#' @title Simulate detections at receivers
#' @description These functions facilitate the simulation of detections, arising from animal movement path(s), at passive acoustic telemetry receiver(s).
#'
#' @param .paths A [`data.table`] that defines movement path(s) (e.g., from [`sim_path_walk()`]. This should contain the following columns:
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
#' @param .calc_detection_pr,... A function that calculates detection probabilities. A [`data.table`] is passed to this function that defines, for each path point, the distance to each corresponding receiver (in a column called `dist`). All other variables in `.paths` and `.arrays` are also available in this [`data.table`]. This makes it possible to specify a wide variety of detection probability models (see Examples). [`calc_detection_pr()`] function is an example that wraps [`calc_detection_pr_logistic()`], which implements a standard, distance-dependent logistic detection probability model. Other arguments can be passed to `.calc_detection_pr` via `...`.
#' @param .sim_obs A function that simulates detections (0, 1), such as [`stats::rbinom()`]. This must accept three arguments:
#' * `n`---an `integer` that defines the number of outcomes to simulate
#' * `size`---an `integer` that defines the number of trials (`size = 1`);
#' * `prob`---a `numeric` vector that defines detection probabilities;
#' @param .type If `.paths` and `.arrays` contain multiple paths/arrays, `.type` is a `character` that defines whether or not to simulate detections for each path/array pair (`.type = "pairwise"`) or for all combinations of paths/arrays (`type = "combinations"`).
#' @param .return (optional) A `character` vector that defines column names retained in the output. `NULL` retains all columns in `.paths` and `.arrays` plus internally computed columns:
#' * `dist`---the distance between points on the path(s) and the receiver(s) that recorded detections;
#' * `pr`---the probability of detection at receivers that recorded detections;
#'
#' @param .data The input for [`calc_detection_pr()`], an example `.calc_detection_pr` function (see above).
#' @param .distance,.alpha,.beta,.gamma Arguments for [`calc_detection_pr_logistic()`].
#' * `.distance` is a `numeric` vector of distances;
#' * `.alpha` is the intercept;
#' * `.beta` is the coefficient for the effect of distance;
#' * `.gamma` is a `numeric` vector of detection range(s);
#'
#' @details
#' [`sim_detections()`] implements the simulation. This requires the movement path(s) and array(s) in which detections are simulated to be provided as [`data.table`]s. If multiple paths and/or arrays are provided, the function simulates detections for each path/array pair (if `.type = "pairwise"`) or for all combinations of arrays and paths (if `.type = "combinations`). Detections are simulated in three steps:
#' * A distance function (`.calc_distance`) is used to calculate distances between points along a selected path and receiver(s):
#' * A detection probability function (`.calc_detection_pr`) is used to calculate detection probabilities, given distances and other information in `.paths` and `.arrays`;
#' * A random generation function (`.sim_obs`) is used to simulate detections (0, 1) at receivers;
#'
#' In the output, only detections are retained (as in 'real-world' datasets).
#'
#' @return [`sim_detections()`] returns a [`data.table`] with columns specified by `.return`.
#'
#' @example man/examples/sim_detections-examples.R
#'
#' @author Edward Lavender
#' @name sim_detections
NULL

#' @rdname sim_detections
#' @export

sim_detections <- function(.paths, .arrays,
                           .calc_distance = terra::distance, .lonlat = FALSE,
                           .calc_detection_pr = calc_detection_pr, ...,
                           .sim_obs = stats::rbinom,
                           .type = c("pairwise", "combinations"),
                           .return = c("array_id", "path_id",
                                       "timestep", "receiver_id",
                                       "dist", "pr")
) {

  #### Check user inputs
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
               .calc_detection_pr = .calc_detection_pr, ...,
               .sim_obs = .sim_obs, .return = .return)
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


#' @rdname sim_detections
#' @export

calc_detection_pr <- function(.data, ...) {
  calc_detection_pr_logistic(.distance = .data$dist, ...)
}

#' @rdname sim_detections
#' @export

calc_detection_pr_logistic <- function(.distance,
                                       .alpha = 2.5, .beta = -0.02,
                                       .gamma = 500) {
  pr <- stats::plogis(.alpha + .beta * .distance)
  pr[.distance > .gamma] <- 0
  pr
}
