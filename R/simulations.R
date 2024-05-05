#' @title Simulation: acoustic arrays
#' @description Simulate acoustic arrays (i.e., networks of acoustic receiver(s)) on a grid.
#'
#' @param .map A [`SpatRaster`] that defines the region of interest (see [`glossary`]). Here, `map` is used to:
#' * Sample receiver locations in appropriate (non `NA`) regions, via [`terra::spatSample()`];
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation. Here, `.timeline` is used to:
#' * Define receiver deployment periods (that is, `receiver_start` and `receiver_end` columns in the output [`data.table`]). Receiver deployment periods are defined by `min(.timeline)` and `max(.timeline)` and constant for all receivers.
#' @param .arrangement,.n_receiver,... Arguments passed to [`terra::spatSample()`], used to sample receiver locations.
#' * `.arrangement` is a `character` that defines the receiver arrangement (passed to the `method` argument).
#' * `.n_receiver` is an `integer` that defines the number of receivers to simulate (passed to the `size` argument).
#' * `...` Additional arguments, passed to [`terra::spatSample()`], excluding `x`, `size`, `method`, `replace`, `na.rm`, `xy`, `cells` and `values`.
#' @param .receiver_alpha,.receiver_beta,.receiver_gamma (optional) `numeric` constants for the default detection probability parameters for inclusion in the output [`data.table`];
#'
#'  Single inputs are expected to these arguments, which are constant across all receivers.
#'
#' @param .n_array An `integer` that defines the number of array designs to simulate with the aforementioned parameters.
#' @param .plot A `logical` variable that defines whether or not to plot simulated arrays.
#' @param .one_page If `.plot = TRUE`, `.one_page` is a `logical` variable that defines whether or not to produce plots on a single page.
#'
#' @details
#' This function replaces [`flapper::sim_array()`](https://edwardlavender.github.io/flapper/reference/sim_array.html).
#'
#' @return The function returns a `data.table` with the following columns:
#' * `array_id`---an `integer` vector of array IDs,
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `receiver_start`, `receiver_end`---`POSIXct` vectors that defines receiver deployment periods;
#' * `receiver_x` and `receiver_y`---`numeric` vectors that defines receiver coordinates;
#' * `receiver_alpha`, `receiver_beta`, `receiver_gamma`---`numeric` vectors of detection probability parameters, if defined;
#'
#' @example man/example/sim_array.R
#'
#' @seealso TO DO
#' @author Edward Lavender
#' @export

sim_array <- function(.map,
                      .timeline,
                      .arrangement = "random", .n_receiver = 10L, ...,
                      .receiver_alpha = 4, .receiver_beta = -0.01, .receiver_gamma = 750,
                      .n_array = 1L,
                      .plot = TRUE, .one_page = FALSE) {

  #### Check user inputs
  # check_dots_used: terra::spatSample() used
  check_dots_allowed(c("x", "size", "method", "replace", "na.rm", "xy", "cells", "values"), ...)
  check_dots_for_missing_period(formals(), list(...))
  .timeline <- check_timeline(.timeline)

  #### Simulate arrays
  arrays <-
    lapply(seq_len(.n_array), function(i) {
      # Sample receiver locations
      array <-
        .map |>
        terra::spatSample(size = .n_receiver,
                          method = .arrangement, replace = FALSE,
                          na.rm = TRUE, xy = TRUE, cells = TRUE, values = FALSE, ...) |>
        as.data.table() |>
        mutate(array_id = i,
               receiver_id = as.integer(row_number()),
               receiver_start = min(.timeline),
               receiver_end = max(.timeline)) |>
        select("array_id", "receiver_id",
               "receiver_start", "receiver_end",
               "receiver_x" = "x", "receiver_y" = "y") |>
        as.data.table()
      # Add optional columns
      if (!is.null(.receiver_alpha)) {
        receiver_alpha <- NULL
        array[, receiver_alpha := .receiver_alpha]
      }
      if (!is.null(.receiver_beta)) {
        receiver_beta <- NULL
        array[, receiver_beta := .receiver_beta]
      }
      if (!is.null(.receiver_gamma)) {
        receiver_gamma <- NULL
        array[, receiver_gamma := .receiver_gamma]
      }
      # Return simulated array (moorings data.table)
      array
    })

  #### Plot arrays
  if (.plot) {
    pp <- one_page(.one_page, length(arrays))
    on.exit(par(pp), add = TRUE)
    lapply(seq_len(length(arrays)), function(i) {
      terra::plot(.map, main = paste("Array", i))
      points(arrays[[i]]$receiver_x, arrays[[i]]$receiver_y)
    }) |> invisible()
  }

  #### Return outputs
  arrays |>
    rbindlist()

}


#' @title Simulation: movement walks
#' @description Simulate discrete-time animal movement paths from walk models (e.g., random walks, biased random walks, correlated random walks).
#'
#' @param .map A [`SpatRaster`] that defines the study area for the simulation (see [`glossary`]). Here, `.map` is used to:
#' * Simulate initial states if `.xinit = NULL` (via [`sim_states_init()`]);
#' * Extract `.map` coordinates for the simulated path(s);
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation. Here, `.timeline` is used to:
#' * Define the number of time steps for the simulation;
#' * Define the time resolution of the simulation;
#' @param .state A `character` that defines the state type (see [`glossary`]).
#' @param .xinit,.n_path Initial state arguments.
#' * `.xinit` specifies the initial states for the simulation (one for each movement path).
#'    - If `.xinit` is `NULL`, initial states are sampled from `.map` (via [`sim_states_init()`]).
#'    - Otherwise, `.xinit` must be a [`data.table`] with one column for each state dimension.
#' * `.n_path` is an `integer` that defines the number of paths to simulate.
#' @param .move A character string that defines the movement model (see [`move`] and [`glossary`]).
#' @param .plot,.one_page Plot options.
#' * `.plot` is a `logical` variable that defined whether or not to plot `.map` and simulated path(s). Each path is plotted on a separate plot.
#' * `.one_page` is a logical variable that defines whether or not to produce all plots on a single page.
#'
#' @details
#' This function simulates movement paths via `Patter.simulate_path_walk()`:
#' * The internal function [`sim_states_init()`] is used to set the initial state(s) for the simulation; that is, initial coordinates and other variables (one for each `.n_path`). If `.state` is one of the built-in options (see [`glossary`]), initial state(s) can be sampled from `.map`. Otherwise, additional methods or a [`data.table`] of initial states must be provided (see [`sim_states_init()`]). Initial states provided in `.xinit` are re-sampled, with replacement, if required, such that there is one initial state for each simulated path. Initial states are assigned to an `xinit` object in Julia, which is a vector of `State`s.
#' * Using the initial states, the Julia function `Patter.simulate_path_walk()` simulates movement path(s) using the movement model.
#' * Movement paths are passed back to `R` for convenient visualisation and analysis.
#'
#' To use a new `.state` and/or movement model type in [`sim_path_walk()`]:
#' * Define a `State` subtype in `Julia` and provide the name as a `character` string to this function;
#' * To initialise the simulation, write a [`states_init()`] method to enable automated sampling of initial states via [`sim_states_init()`] or provide a [`data.table`] of initial states to `.xinit`;
#' * Define a corresponding `ModelMove` subtype in `Julia`;
#' * Instantiate a `ModelMove` instance (that is, define a specific movement model) in `Julia`;
#' * Write a `Patter.r_get_states()` method to translate the Vector of `State`s from `Patter.simulate_path_walk()` into a `DataFrame` that can be passed to `R`;
#'
#' [`sim_path_walk()`] replaces [`flapper::sim_path_sa()`](https://edwardlavender.github.io/flapper/reference/sim_path_sa.html). Other [`flapper::sim_path_*()`](https://edwardlavender.github.io/flapper/reference/sim_path_-times.html) functions are not currently implemented in [`patter`].
#'
#' @return [`sim_path_walk()`] returns a [`data.table`] with the following columns:
#' * `path_id`---an `integer` vector that identifies each path;
#' * `timestep`---an `integer` vector that defines the time step;
#' * `timestamp`---a `POSIXct` vector of time stamps;
#' * `cell_id`, `cell_x`, `cell_y`, `cell_z`---`integer`/`numeric` vectors that define the locations of the simulated positions on `.map`;
#' * `x`,`y`,`...`---`numeric` vectors that define the components of the state;
#'
#' @seealso TO DO
#' @author Edward Lavender
#' @name sim_path_walk
NULL

#' @rdname sim_path_walk
#' @export

sim_path_walk <- function(.map,
                          .timeline,
                          .state = "StateXY",
                          .xinit = NULL, .n_path = 1L,
                          .move,
                          .plot = TRUE, .one_page = FALSE) {

  #### Check user inputs
  check_inherits(.state, "character")
  check_inherits(.move, "character")

  #### Set initial state
  .xinit <- sim_states_init(.map = .map,
                            .timeline = NULL,
                            .datasets = NULL,
                            .models = NULL,
                            .pars = NULL,
                            .state = NULL,
                            .xinit = .xinit,
                            .n = .n_path)
  set_states_init(.xinit = .xinit)

  #### Set movement model
  set_timeline(.timeline)
  set_move(.move)

  #### Simulate random walk
  set_path()

  #### Get paths in R
  paths       <- julia_eval('Patter.r_get_states(paths)')
  paths       <- as.data.table(paths)
  state_dims  <- colnames(paths)[(!colnames(paths) %in% c("path_id", "timestep"))]

  #### Tidy data.table
  paths <-
    paths |>
    mutate(
      # Define path_id, time step and time stamp
      path_id = as.integer(.data$path_id),
      timestep = as.integer(.data$timestep),
      timestamp = .timeline[.data$timestep],
      # Add map coordinates
      cell_id = terra::cellFromXY(.map, cbind(.data$x, .data$y)),
      cell_x = as.numeric(terra::xFromCell(.map, .data$cell_id)),
      cell_y = as.numeric(terra::yFromCell(.map, .data$cell_id)),
      cell_z = terra::extract(.map, .data$cell_id)[, 1]) |>
    # Tidy columns
    select("path_id", "timestep", "timestamp",
           "cell_x", "cell_y", "cell_z", "cell_id",
           state_dims
    ) |>
    as.data.table()

  #### Validate simulation (check for NAs)
  # This is no longer required as Patter.simulate_move() is implemented with n_trial = Inf
  # This ensures consistency between Patter.jl & patter

  #### Visualise paths & return
  if (.plot) {
    pp <- one_page(.one_page, fndistinct(paths$path_id))
    on.exit(par(pp), add = TRUE)
    lapply(split(paths, paths$path_id), function(d) {
      terra::plot(.map, main = d$path_id[1])
      add_sp_path(d$x, d$y, length = 0)
    }) |> invisible()
  }
  paths

}


#' @title Simulation: observations
#' @description Simulate a time series of observations, such as acoustic detections and depths, arising from simulated animal movement path(s).
#'
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation. This should match the `.timeline` used to simulate movement paths (see [`sim_path_walk()`]).
#' @param .models A `character` vector of `ModelObs` subtype(s) defined in `Julia` (see [`glossary`]).
#' @param .parameters A `list` of [`data.table`]s, one for each model in `.models`, that define, for each sensor of that type, the model parameters.
#'
#' @details
#' This function wraps the `Patter.simulate_obs()` Julia function. The function iterates over simulated paths defined in the Julia workspace by [`sim_path_walk()`]. For each path and time step, the function simulates observation(s). Collectively, `.models` and `.parameters` define the observation models used for the simulation (that is, a `Vector` of `ModelObs` instances). In `Julia`, simulated observations are stored in a hash table (`Dict`) called `yobs`, which is translated into a named `list` that is returned by `R`.
#'
#' @returns The function returns a named `list`, with one element for each sensor type, that is `.models` element. Each element is a `list` of `data.table`s, one for each simulated path. Each row is a time step. The columns depend on the model type.
#'
#' @author Edward Lavender
#' @export

# Simulate observations:
sim_observations <- function(.timeline, .models, .parameters) {
  set_timeline(.timeline)
  set_parameters(.parameters)
  set_models(.models)
  set_yobs_via_sim()
  out <- lapply(.models, function(model) {
    julia_eval(glue("Patter.r_get_dataset(yobs, {model})"))
  })
  names(out) <- .models
  out
}
