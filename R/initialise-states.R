#' @title Simulation: initial states
#' @description Simulate initial states for animal movement walks.
#' @param .map A [`SpatRaster`] that defines the study area for the simulation (see [`glossary`]). Here, `.map` is used to:
#' * Sample initial coordinates, via [`coords_init()`], if `.xinit = NULL`;
#'
#' @param .timeline,.direction,.datasets,.models,.pars (optional) Additional arguments used to restrict `.map`, via [`map_init()`], before sampling initial states;
#' * `.timeline`---A sorted, `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation;
#' * `.direction`---A `character` string that defines the direction of the simulation (`"forward"` or `"backward"`);
#' * `.datasets`---A `list` of observation datasets;
#' * `.models`---A `character` vector of `ModelObs` subtypes;
#' * `.pars`---A named `list` of additional arguments, passed to [`map_init()`];
#'
#' @param .state A `character` that defines the `State` subtype. Here, `.state` is used to:
#' * Convert sampled coordinates to initial states, via [`states_init()`], if `.xinit = NULL`;

#' @param .xinit (optional) A [`data.table`] of initial states, with one column for each state dimension.
#' @param .n An `integer` that defines the number of simulated states:
#' * If `.xinit = NULL`, `.n` specifies the number of simulated states via [`coords_init()`];
#' * If `.xinit` is supplied but there are not `.n` initial states, `.n` initial states are re-sampled from `.xinit` with replacement;
#'
#' @details
#' These internal functions support the simulation of initial states for the simulation of animal movement walks in [`sim_path_walk()`] and [`pf_filter()`].
#'
#' If `.xinit = NULL`, initial coordinates are sampled from `.map`.
#'
#' The region(s) within `.map` from which initial coordinates are sampled can be optionally restricted by the provision of the observation datasets and the associated model subtypes (via [`map_init_iter()`]). This option does not apply to [`sim_path_walk()`] but is used in [`pf_filter()`] where `.models` is supplied. In this instance, [`map_init_iter()`] iterates over each model and uses the [`map_init()`] generic to update `.map`. The following methods are implemented:
#' * [`map_init.default()`]. The default methods returns `.map` unchanged.
#' * [`map_init.ModelObsAcousticLogisTrunc()`]. This method uses acoustic observations to restrict `.map` via Lavender et al.'s (2023) acoustic--container algorithm. The function identifies the receiver(s) that recorded detection(s) immediately before, at and following the first time step (`.timeline[start]`, where `start` is `1` if `.direction = "forward` and `length(.timeline)` otherwise). The 'container' within which the individual must be located from the perspective of each receiver is defined by the time difference and the individual's mobility (that is, the maximum moveable distance the individual could move between two time steps), which must be specified in `pars$mobility`. The intersection between all containers defines the possible locations of the individual at the first time step.
#' * [`map_init.ModelObsDepthUniform()`]. This method uses the depth observations to restrict `.map` (which should represent the bathymetry in a region). The individual must be within a region in which the observed depth at `.timeline[start]` is within a depth envelope around the bathymetric depth defined by the parameters `depth_shallow_eps` and `depth_deep_eps`. (If there is no observation at `.timeline[start]`, `.map` is returned unchanged.)
#' * [`map_init.ModelObsDepthNormalTrunc()`]. This method also uses depth observations to restrict `.map`. The individual must be in a location where the bathymetric depth plus the `depth_deep_eps` parameter at `.timeline[start]` is greater than or equal to the observed depth at `.timeline[start]`. (If there is no observation at `.timeline[start]`, `.map` is returned unchanged.)
#'
#' To handle custom `ModelObs` subtypes, process `.map` beforehand or write an appropriate [`map_init()`] method.
#'
#' Using `.map`, a [`data.table`] of `.n` initial coordinates (`map_value`, `x`, `y`) is sampled using [`coords_init()`]. Additional state dimensions are added, as required depending on the `.state`, via the S3 generic [`states_init()`]. For custom `State` subtypes, a corresponding [`states_init()`] method is required (or supply `.xinit` yourself).
#'
#' If `.xinit()` is provided and `.n` initial states are provided, `.xinit` is returned unchanged. Otherwise, `.n` initial states are resampled from `.xinit`, with replacement, and returned.
#'
#' @author Edward Lavender
#' @name sim_states_init

#' @rdname sim_states_init
#' @keywords internal

# Simulate initial states (wrapper function)
sim_states_init <- function(.map,
                            .timeline,
                            .direction,
                            .datasets,
                            .models,
                            .pars,
                            .state,
                            .xinit,
                            .n) {

  #### If un-provided, sample `.xinit`
  if (is.null(.xinit)) {

    #### Define an initial map from which to sample
    # We use the observation datasets to restrict (if possible) the input `.map` for sampling
    # TO DO: confirm method validity (AS)
    if (!is.null(.models)) {
      .map <- map_init_iter(.map = .map,
                            .timeline = .timeline,
                            .direction = .direction,
                            .datasets = .datasets,
                            .models = .models,
                            .pars = .pars)
    }

    #### Simulate initial states
    # Define initial coordinates (map_value, x, y)
    coord <- coords_init(.map = .map, .n = .n)
    # Add additional state dimensions
    # * User defined State structures require:
    # - states_init method or `.xinit`
    # - A `Patter.julia_get_xinit()` method
    .xinit <- states_init(.state = char_to_class(.state), .coords = coord)

  } else {
    check_inherits(.xinit, "data.table")
  }

  #### Re-sample `.xinit` .`n` times (if required)
  # This is required if:
  # * spatSample does not return `.n` samples
  # * The user supplies `.xinit` with fewer than specified rows
  if (nrow(.xinit) != .n) {
    .xinit <- .xinit[sample.int(.N, size = .n, replace = TRUE), ]
  }
  .xinit

}

#' @rdname sim_states_init
#' @keywords internal

# Define an initial map from which to sample datasets
map_init <- function(.map, .timeline, .direction, .dataset, .model, .pars) {
  UseMethod("map_init", .model)
}

#' @rdname sim_states_init
#' @keywords internal

# The default method (for unknown models) returns `.map`
map_init.default <- function(.map, .timeline, .direction, .dataset, .model, .pars) {
  .map
}

#' @rdname sim_states_init
#' @keywords internal

# For ModelObsAcousticLogisTrunc, we use the AC algorithm
map_init.ModelObsAcousticLogisTrunc <- function(.map,
                                                .timeline,
                                                .direction,
                                                .dataset,
                                                .model,
                                                .pars) {

  #### Check user inputs
  check_names(.dataset, "receiver_gamma")
  check_named_list(.pars)
  if (!("mobility" %in% names(.pars))) {
    abort("`.pars = list(mobility = ...)` is required.")
  }

  #### Define global variables
  receiver_gamma <- gap <- NULL

  #### Define a list of container info
  # Define a timeline of detections
  t1 <- ifelse(.direction == "forward", .timeline[1], .timeline[length(.timeline)])
  step_units <- diffunit(.timeline)
  .dataset <-
    .dataset |>
    lazy_dt(immutable = TRUE) |>
    filter(.data$obs == 1L) |>
    arrange(.data$timestamp) |>
    mutate(gap = as.numeric(difftime(.data$timestamp, t1, units = step_units))) |>
    as.data.table()
  # Define the first detection(s) before t1
  before <- .dataset[gap < 0, ]
  if (fnrow(before) > 0L) {
    before <- before[gap == max(gap), ]
  }
  # Define detection(s) at t1
  at <- .dataset[gap == 0, ]
  # Define the first detection(s) after t1
  after <- .dataset[gap > 0, ]
  if (fnrow(after) > 0L) {
    after <- after[gap == min(gap), ]
  }
  # Define container info
  cinfo <- rbindlist(list(before, at, after))
  # Define distance from receiver:
  buffer <- NULL
  cinfo[, buffer := receiver_gamma + abs(gap) * .pars$mobility]

  #### Define acoustic containers as SpatVectors
  # terra::plot(.map)
  vcontainers <-
    split(cinfo, seq_row(cinfo)) |>
    lapply(\(d) {
      terra::vect(x = cbind(d$receiver_x, d$receiver_y),
                  crs = terra::crs(.map)) |>
        terra::buffer(width = d$buffer, quadsegs = 1000L)
    })

  #### Define initial map
  # Define container as SpatVector
  vcontainer <- spatIntersect(vcontainers)
  # Define container as SpatRaster
  rcontainer <- terra::crop(.map, vcontainer,
                            mask = TRUE, touches = TRUE,
                            extend = FALSE, snap = "out")

  #### Plot
  if (isTRUE(.pars$plot)) {
    # Set window
    pp <- par(mfrow = c(1, 2))
    on.exit(par(pp, no.readonly = TRUE), add = TRUE)
    # Define local helper function that adds receivers & containers
    add_layers <- function() {
      # Add receivers
      points(cinfo$receiver_x, cinfo$receiver_y, pch = 4)
      # Add all containers
      sapply(vcontainers, \(x) terra::lines(x, lwd = 2))
      # Add the intersection
      terra::lines(vcontainer, col = "red", lwd = 1.5, lty = 3)
    }
    # Plot full map
    terra::plot(.map, main = "`.map`"); add_layers()
    # Plot container map
    terra::plot(rcontainer, main = "Container"); add_layers()
  }

  rcontainer

}

#' @rdname sim_states_init
#' @keywords internal

# For ModelObsDepthUniform, we restrict .map use the depth observation
map_init.ModelObsDepthUniform <- function(.map,
                                          .timeline,
                                          .direction,
                                          .dataset,
                                          .model,
                                          .pars) {
  # Identify the first depth observation
  t1    <- ifelse(.direction == "forward", .timeline[1], .timeline[length(.timeline)])
  pos   <- which(.dataset$timestamp == t1)
  if (length(pos) == 0L) {
    return(.map)
  }
  depth <- .dataset$obs[pos]
  # Define the corresponding structure parameters
  depth_shallow_eps <- .dataset$depth_shallow_eps[pos]
  depth_deep_eps    <- .dataset$depth_deep_eps[pos]
  # Mask map between limits
  terra::mask(.map,
              .map >= depth - depth_shallow_eps & .map <= depth + depth_deep_eps,
              maskvalues = 0)
}

#' @rdname sim_states_init
#' @keywords internal

# For ModelObsDepthNormalTrunc, we restrict .map use the depth observation
map_init.ModelObsDepthNormalTrunc <- function(.map,
                                              .timeline,
                                              .direction,
                                              .dataset,
                                              .model,
                                              .pars) {
  # Identify the first depth observation
  t1    <- ifelse(.direction == "forward", .timeline[1], .timeline[length(.timeline)])
  pos   <- which(.dataset$timestamp == t1)
  if (length(pos) == 0L) {
    return(.map)
  }
  depth <- .dataset$obs[pos]
  # Define the corresponding structure parameters
  depth_deep_eps <- .dataset$depth_deep_eps[pos]
  # Mask map between limits
  # * .map + depth_deep_eps must be >= depth
  terra::mask(.map,
              .map >= depth + depth_deep_eps,
              maskvalues = 0)
}


#' @rdname sim_states_init
#' @keywords internal

# Iteratively update .map (according for each input dataset) using map_init methods
map_init_iter <- function(.map,
                          .timeline,
                          .direction,
                          .datasets,
                          .models,
                          .pars) {
  # Define model classes for method dispatch
  .model_classes <- chars_to_classes(.models)
  # Iteratively update map using the input datasets
  for (i in seq_len(length(.models))) {
    .map <- map_init(.map = .map,
                     .timeline = .timeline,
                     .direction = .direction,
                     .dataset = .datasets[[i]],
                     .model = .model_classes[[i]],
                     .pars = .pars)
  }
  .map
}

#' @rdname sim_states_init
#' @keywords internal

# Sample .n initial coordinates (map_value, x, y) from a SpatRaster
coords_init <- function(.map, .n) {
  map_value <- x <- y <- NULL
  .xinit <-
    .map |>
    terra::spatSample(size = .n,
                      method = "random",
                      na.rm = TRUE,
                      xy = TRUE,
                      values = TRUE,
                      warn = FALSE) |>
    as.data.table()
  colnames(.xinit) <- c("x", "y", "map_value")
  .xinit[, list(map_value, x, y)]
}

#' @rdname sim_states_init
#' @keywords internal

# Convert a `data.table` of coordinates (map_value, x, y) to a data.table with all state dimensions
states_init <- function(.state, .coords) {
  UseMethod("states_init", .state)
}

#' @rdname sim_states_init
#' @keywords internal

states_init.default <- function(.state, .coords) {
  abort("For custom states, you need to define a `states_init()` S3 method or provide `.xinit`.")
}

#' @rdname sim_states_init
#' @keywords internal

states_init.StateXY <- function(.state, .coords) {
  .coords
}

#' @rdname sim_states_init
#' @keywords internal

states_init.StateXYZD <- function(.state, .coords) {
  z <- map_value <- angle <- NULL
  # Add z coordinate
  .coords[, z := map_value * runif(.N)]
  # Add angle
  .coords[, angle := runif(.N) * 2 * pi]
  .coords
}
