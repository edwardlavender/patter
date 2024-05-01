#' @title Simulation: acoustic arrays
#' @description This function simulates acoustic arrays (i.e., networks of acoustic receiver(s)) on a grid.
#' @param .map A [`SpatRaster`] that defines the region of interest (see [`glossary`]). Receivers are not simulated in `NA` regions.
#' @param .arrangement,.n_receiver,... Arguments passed to [`terra::spatSample()`].
#' * `.arrangement` is a `character` that defines the receiver arrangement (passed to the `method` argument).
#' * `.n_receiver` is an `integer` that defines the number of receivers to simulate (passed to the `size` argument).
#' * `...` ... Additional arguments, passed to [`terra::spatSample()`], excluding: `x`, `size`, `method`, `replace`, `na.rm`, `xy`, `cells` and `values`.
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
#' * `receiver_x` and `receiver_y`---`numeric` vectors that defines receiver coordinates;
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
#' @seealso TO DO
#' @author Edward Lavender
#' @export

sim_array <- function(.map = spatTemplate(),
                      .arrangement = "random", .n_receiver = 10L, ...,
                      .receiver_start = NULL, .receiver_end = NULL, .receiver_range = NULL,
                      .n_array = 1L,
                      .plot = TRUE, .one_page = FALSE) {

  #### Check user inputs
  # check_dots_used: terra::spatSample() used
  check_dots_allowed(c("x", "size", "method", "replace", "na.rm", "xy", "cells", "values"), ...)
  check_dots_for_missing_period(formals(), list(...))
  # * Check `.receiver_end` is after `.receiver_start` (if supplied)
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
        .map |>
        terra::spatSample(size = .n_receiver,
                          method = .arrangement, replace = FALSE,
                          na.rm = TRUE, xy = TRUE, cells = TRUE, values = FALSE, ...) |>
        as.data.table() |>
        mutate(array_id = i,
               receiver_id = as.integer(row_number())) |>
        select("array_id", "receiver_id", "receiver_x" = "x", "receiver_y" = "y") |>
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
      terra::plot(.map, main = paste("Array", i))
      points(arrays[[i]]$receiver_x, arrays[[i]]$receiver_y)
    }) |> invisible()
  }

  #### Return outputs
  arrays |>
    rbindlist()

}


#' @title Simulation: movement walks
#' @description [`sim_path_walk()`] simulates discrete-time animal movement paths from walk models (e.g., random walks, biased random walks, correlated random walks).
#'
#' @param .state A `character` that defines the state (see [`glossary`]).
#' @param .xinit,.map,.n_path Initial state arguments.
#' * `.xinit` specifies the initial states for the simulation (one for each movement path).
#'    - If `.xinit` is `NULL`, initial states are sampled from `.map`.
#'    - If `.xinit` is a [`data.frame`] with one column for each state dimension.
#' * `.map` is a [`SpatRaster`] that defines the area within which movements are simulated that is required if `.xinit = NULL` (see [`glossary`].
#' * `.n_path` is an `integer` that defines the number of paths to simulate.
#' @param .move A character string that defines the movement model (see [`move`] and [`glossary`]).
#' @param .n_step An `integer` that defines the number of time steps.
#' @param .timestamp (optional) A vector of time stamps, one for each time step, for inclusion in the output [`data.table`] as a `timestamp` column.
#' @param .plot,.one_page Plot options.
#' * `.plot` is a `logical` variable that defined whether or not to plot `.map` and simulated path(s). Each path is plotted on a separate plot.
#' * `.one_page` is a logical variable that defines whether or not to produce all plots on a single page.
#'
#' @details
#' This function simulates movement paths via `Patter.sim_walk()`:
#' * The internal function [`set_initial_states()`] is used to set the initial state(s) for the simulation; that is, initial coordinates and other variables (one for each `.n_path`). If `.state` is one of the built-in options (see [`glossary`]), initial state(s) can be sampled from `.map`. Otherwise, a [`data.frame`] of initial states must be provided. Initial states provided in the [`data.frame`] are  re-sampled, with replacement, if required, such that there is one initial state for each simulated path. Initial states are assigned to an `xinit` object in Julia, which is a vector of `State` structures.
#' * Using the initial states, the Julia function `Patter.sim_walk()` simulates movements using the movement model.
#' * The resultant movement paths are brought back into `R` for convenient visualisation and analysis.
#'
#' To use a new `.state`, you need to:
#' * Define a `State` subtype in `Julia` and provide the name as a character string to this function;
#' * Define a [`data.frame`] of initial states;
#' * Define a corresponding `ModelMove` subtype in `Julia`;
#' * Specify the movement model components to this function;
#'
#' To use a new type of movement model, follow the last two steps above.
#'
#' [`sim_path_walk()`] replaces [`flapper::sim_path_sa()`](https://edwardlavender.github.io/flapper/reference/sim_path_sa.html). Other [`flapper::sim_path_*()`](https://edwardlavender.github.io/flapper/reference/sim_path_-times.html) functions are not currently implemented in [`patter`].
#'
#' @return [`sim_path_walk()`] returns a [`data.table`] with the following columns:
#' * `path_id`---an `integer` vector that identifies each path;
#' * `timestep`---an `integer` vector that defines the time step;
#' * `timestamp`---(optional) a `POSIXct` vector of time stamps;
#' * `cell_id`, `cell_x`, `cell_y`, `cell_z`---`integer`/`numeric` vectors that define the locations of the simulated positions on `.map`;
#' * `x`,`y`,`...`---`numeric` vectors that define the components of the state;
#'
#' @seealso TO DO
#' @author Edward Lavender
#' @name sim_path_walk
NULL

#' @rdname sim_path_walk
#' @export

sim_path_walk <- function(.state = "StateXY",
                          .xinit = NULL, .map, .n_path = 1L,
                          .move,
                          .n_step = 10L, .timestamp = NULL,
                          .plot = TRUE, .one_page = FALSE) {

  #### Check user inputs
  check_inherits(.state, "character")
  check_inherits(.move, "character")
  if (!is.null(.timestamp) && .n_step != length(.timestamp)) {
    abort("`.n_step` must match `length(.timestamp)`.")
  }

  #### Set initial state
  # * `.xinit` = NULL: we create and export .xinit
  # * `.xinit` = data.frame: we resample (if required) & export
  set_initial_states(.state = .state, .xinit = .xinit, .map = .map, .n = .n_path)

  #### Set movement model
  set_move(.move)

  #### Simulate random walk
  set_path(as.integer(.n_step))
  paths       <- julia_eval('Patter.rget_state_df(path)')
  paths       <- as.data.table(paths)
  state_dims  <- colnames(paths)[(!colnames(paths) %in% c("path_id", "timestep"))]

  #### Tidy data.table
  # Add time stamps
  if (!is.null(.timestamp)) {
    timestamp <- timestep <- NULL
    paths[, timestamp := timestamp[match(timestep, seq_len(.n_step))]]
  }
  # Add bathymetry
  paths <-
    paths |>
    mutate(
      path_id = as.integer(.data$path_id),
      timestep = as.integer(.data$timestep),
      cell_id = terra::cellFromXY(.map, cbind(.data$x, .data$y)),
      cell_x = as.numeric(terra::xFromCell(.map, .data$cell_id)),
      cell_y = as.numeric(terra::yFromCell(.map, .data$cell_id)),
      cell_z = terra::extract(.map, .data$cell_id)[, 1]) |>
    # Tidy columns
    select("path_id", "timestep", any_of("timestamp"),
           "cell_x", "cell_y", "cell_z", "cell_id",
           state_dims
    ) |>
    as.data.table()
  # Validate simulation (check for NAs)
  is_na <- is.na(paths$cell_z)
  if (any(is_na)) {
    # Identify paths with NAs (i.e., paths on land)
    path_with_na <- unique(paths$path_id[is_na])
    # Drop invalid paths
    warn("{length(path_with_na)}/{.n_path} path(s) in inhospitable locations.",
         .envir = environment())
    paths <-
      paths |>
      filter(!(.data$path_id %in% path_with_na)) |>
      as.data.table()
    # Return empty data.table if required
    if (nrow(paths) == 0L) {
      warn("Output is an empty data.table.")
      return(paths)
    }
    # Update path IDs
    path_id <- NULL
    paths[path_id := rleid(path_id)]
  }

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

