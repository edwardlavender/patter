#' @title States
#' @description [`State`] is an Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups state sub-types.
#'
#' @details
#' [`State`] sub-types are `Julia` structures that hold the parameters that describe the state of an individual at a given time step. 'State' typically means 'location' (in two or three dimensions), but individual states may include additional fields for those state dimensions that depend on the state time step. (For example, in correlated random walks, the [`State`] should contain the heading, given that the heading at one time step depends on the previous heading and a turning angle.) From an `R`-user perspective, you can think of a [`State`] sub-type as an `S4`-[`class`]-like object, with slots for the state dimensions.
#'
#' In [`patter`] functions, the `.state` argument is a `character` string that defines the animal's state sub-type. This must match a [`State`] sub-type defined in `Julia`. The built-in options are:
#' * `"StateXY"`, which maps to `StateXY`;
#' * `"StateXYZ"`, which maps to `StateXYZ`;
#' * `"StateCXY"`, which maps to `StateCXY` ;
#' * `"StateCXYZ"`, which maps to `StateCXYZ`;
#'
#' See [`State`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("State")` for the fields of the built-in sub-types.
#'
#' `.state` is used by [`sim_path_walk()`] and [`pf_filter()`], both of which effectively simulate time series of states. `.state` controls the simulation of initial locations and subsequent method dispatch in [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). [`Patter.sim_states_init()`](https://github.com/edwardlavender/Patter.jl) handles the simulation of initial states in these routines. When `.state` is
#' * `"StateXY"`, the initial state comprises `x` and `y` coordinates;
#' * `"StateXYZ"`, the initial state comprises `x`, `y` and `z` coordinates;
#' * `"StateCXY"`, the initial state comprises `x` and `y` coordinates and an initial heading;
#' * `"StateCXYZ"`, the initial states comprises `x`, `y` and `z` coordinates and an initial heading;
#'
#' All states additionally include a `map_value` field that defines the value on the movement map at (`x`, `y`).
#'
#' The outcome of [`Patter.sim_states_init()`](https://github.com/edwardlavender/Patter.jl) is a `DataFrame` of initial state(s), which is coerced to a `Vector` of `State`s in `Julia` for the simulation. In [`Patter.jl`](https://github.com/edwardlavender/Patter.jl), the simulation of subsequent states depends on the input state and the movement model.
#'
#' The state must match the movement model (see [`ModelMove`]):
#' * For `"StateXY"`, a movement model that simulates step lengths and headings, and updates state components (that is, `x` and `y` coordinates), is required;
#' * For `"StateXYZ"`, a movement model that simulates step lengths, headings and depths, and updates state components (that is, `x`, `y` and `z` coordinates), is required;
#' * For `"StateCXY"`, a movement model that simulates step lengths and turning angles, and updates `x`, `y`, `heading` state components, is required.
#' * For `"StateCXYZ"`, a movement model that simulates step lengths, turning angles and changes in depth, and updates `x`, `y`, `z` and `heading` state components, is required.
#'
#' To use custom [`ModelObs`] sub-types, see Examples.
#'
#' @example man/examples/example-State.R
#' @seealso
#' The routines in [`patter`] for the simulation of individual movements, observations and statistical modelling are built upon three Abstract Types defined in `Julia`:
#' * See [`State`] for individual-state (location) structures;
#' * See [`ModelMove`] for movement model structures;
#' * See [`ModelObs`] for observation model structures;
#' @author Edward Lavender
#' @name State
#' @aliases .state state StateXY StateXYZ StateCXY StateCXYZ
NULL


#' @title Observation models
#' @description [`ModelObs`] is Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups observation model sub-types. `model_obs_*()` `R` functions create [`data.table`]s of observation model parameters which can be visualised in `R` (via `plot.ModelObs*()` methods) and instantiated as [`ModelObs`] instances in `Julia`.
#'
#' @param .data A [`data.table`] that contains observation model parameters.
#'
#' * For [`ModelObsAcousticLogisTrunc`], required columns are:
#'      * `sensor_id`
#'      * `receiver_x` and `receiver_y`
#'      * `receiver_alpha`, `receiver_beta` and `receiver_gamma`
#'
#' * For [`ModelObsAcousticContainer`], required columns are:
#'      * `sensor_id`
#'      * `receiver_x` and `receiver_y`
#'      * `radius`
#'
#' * For [`ModelObsDepthUniformSeabed`], required columns are:
#'      * `sensor_id`
#'      * `depth_shallow_eps`
#'      * `depth_deep_eps`
#'
#' * For [`ModelObsDepthNormalTruncSeabed`], required columns are:
#'      * `sensor_id`
#'      * `depth_sigma`
#'      * `depth_deep_eps`
#'
#' See [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("ModelObs")` for details.
#'
#' @param .strict A `logical` variable that defines whether or not to only retain columns in `.data` defined in the corresponding `ModelObs` structure.
#' * Set `.strict = TRUE` (default) in [`sim_observations()`];
#' * Set `.strict = FALSE` in [`pf_filter()`] to include time stamps and observations in the [`data.table`];
#'
#' @details
#' Observation model sub-types are `Julia` structures that hold the parameters of observation models. From an `R`-user perspective, you can think of a [`ModelObs`] sub-type as an `S4`-[`class`]-like object, with slots for the parameters of an observation model. With an observation model structure, we can simulate new observations and evaluate the log-probability of existing observations.
#'
#' The following observation models are built in to [`Patter.jl`](https://edwardlavender.github.io/Patter.jl):
#' * [`ModelObsAcousticLogisTrunc`]
#' * [`ModelObsAcousticContainer`]
#' * [`ModelObsDepthUniformSeabed`]
#' * [`ModelObsDepthNormalTruncSeabed`]
#'
#' In [`patter`], observation models are required:
#' * To simulate new observational datasets, via [`sim_observations()`];
#' * To run the particle filter, via [`pf_filter()`];
#'
#' Observation model sub-types and parameters should be specified as a named `list` of [`data.table`]s. To assemble a [`data.table`] of parameters for a given [`ModelObs`] structure, see [`assemble`] functions. A named list can be created manually from individual [`data.table`]s or via `model_obs_*()` `R` functions. The `R` functions simply check the inputs and wrap inputted [`data.table`]s of `ModelObs*` parameters in a named `list`. A [`S3-class`]-`ModelObs*` label is added and enables supporting methods (e.g., [`plot.ModelObs()`]) to be implemented for observation models.
#'
#' Internally in [`patter`] algorithms, observation model sub-types and parameters are instantiated and used to simulate observations or in the particle filter. The simulation of observations is implemented via [`Patter.simulate_obs()`](https://edwardlavender.github.io/Patter.jl). In the particle filter, log-probabilities are evaluated by [`Patter.logpdf_obs()`](https://edwardlavender.github.io/Patter.jl). These are generic functions. Different methods are dispatched according to the input model. For the built-in [`ModelObs`] sub-types, corresponding methods for these routines are also built-in. For custom [`ModelObs`] sub-types, the methods need to be provided.
#'
#' To use custom [`ModelObs`] sub-types, see Examples.
#'
#' @return `model_obs_*()` `R` wrapper functions return a named `list`, with a single element that defines the parameters of the observation models for the corresponding [`ModelObs`] structure.
#'
#' @example man/examples/example-ModelObs.R
#' @inherit State seealso
#' @author Edward Lavender
#' @name ModelObs
#' @aliases .model_obs model_obs ModelObsAcousticLogisTrunc ModelObsAcousticContainer ModelObsDepthUniformSeabed ModelObsDepthNormalTruncSeabed
NULL

#' @rdname ModelObs
#' @export

# Standard acoustic observation model
model_obs_acoustic_logis_trunc <- function(.data, .strict = TRUE) {
  .data <- copy(.data)
  # Set sensor_id column
  if (rlang::has_name(.data, "receiver_id")) {
    setnames(.data, "receiver_id", "sensor_id")
  }
  # Select columns
  cols <- c("sensor_id",
            "receiver_x", "receiver_y",
            "receiver_alpha", "receiver_beta", "receiver_gamma")
  check_names(.data, cols)
  if (.strict) {
    .data <-
      .data |>
      select(all_of(cols)) |>
      as.data.table()
  }
  # Define structure
  .data        <- list(.data)
  names(.data) <- "ModelObsAcousticLogisTrunc"
  structure(
    .data,
    class = c("list", "ModelObs", "ModelObsAcousticLogisTrunc")
  )

}

#' @rdname ModelObs
#' @export

# Acoustic containers
model_obs_acoustic_container <- function(.data, .strict = TRUE) {
  .data <- copy(.data)
  if (rlang::has_name(.data, "receiver_id")) {
    setnames(.data, "receiver_id", "sensor_id")
  }
  cols <- c("sensor_id", "receiver_x", "receiver_y", "radius")
  check_names(.data, cols)
  if (.strict) {
    .data <-
      .data |>
      select(all_of(cols)) |>
      as.data.table()
  }
  # Define structure
  .data        <- list(.data)
  names(.data) <- "ModelObsAcousticContainer"
  structure(
    .data,
    class = c("list", "ModelObs", "ModelObsAcousticContainer")
  )
}

#' @rdname ModelObs
#' @export

# Depth observation uniform around seabed
model_obs_depth_uniform_seabed <- function(.data, .strict = TRUE) {
  .data <- copy(.data)
  cols  <- c("sensor_id", "depth_shallow_eps", "depth_deep_eps")
  check_names(.data, cols)
  if (.strict) {
    .data <-
      .data |>
      select(all_of(cols)) |>
      as.data.table()
  }
  .data <- list(.data)
  names(.data) <- "ModelObsDepthUniformSeabed"
  structure(
    .data,
    class = c("list", "ModelObs", "ModelObsDepthUniformSeabed")
  )
}

#' @rdname ModelObs
#' @export

# Depth observation normal around seabed
model_obs_depth_normal_trunc_seabed <- function(.data, .strict = TRUE) {
  .data <- copy(.data)
  cols  <- c("sensor_id", "depth_sigma", "depth_deep_eps")
  check_names(.data, cols)
  if (.strict) {
    .data <-
      .data |>
      select(all_of(cols)) |>
      as.data.table()
  }
  .data <- list(.data)
  names(.data) <- "ModelObsDepthNormalTruncSeabed"
  structure(
    .data,
    class = c("list", "ModelObs", "ModelObsDepthNormalTruncSeabed")
  )
}


#' @title Observation model plots
#' @description [`plot()`] methods for observation models (see [`ModelObs`]).
#' @param x A named `list` of observation model parameters, including a [`ModelObs`] [`S3-class`] label (from a `model_obs_*()` function).
#' @param .sensor_id,.radius,.seabed Model-specific parameters:
#' * `.sensor_id`: For [`plot.ModelObsAcousticLogisTrunc()`], `.sensor_id` controls the sensors (receivers) for which detection probability curves are shown:
#'      * `missing` (default) plots all unique curves;
#'      *  An `integer` vector of sensor IDs plots curves for selected sensors;
#'      * `NULL` plots curves for all sensors;
#' * `.radius`: For [`plot.ModelObsAcousticContainer()`], `.radius` controls the radii for which distributions are shown:
#'      * `missing` (default) plots distributions for first three unique radii;
#'      *  A vector of radii plots curves for selected radii;
#'      * `NULL` plots distributions for all radii;
#' * `.seabed`: For `plot.ModelObsDepth*Seabed()`, `.seabed` is the seabed depth at which distributions are plotted.
#' @param .par Graphical parameters:
#' * `NULL` uses current graphical parameters;
#' * `list()` uses default graphical parameters;
#' * A named `list` of arguments, passed to [`par()`], customises parameters;
#' @param ... Additional arguments, passed to [`plot()`].
#' @details
#'
#' Observation model ([`ModelObs`]) structures are objects that define the parameters of an observation model. The model specifies the probability of an observation (e.g., a particular depth record) given the data (e.g., a depth measurement).
#'
#' * [`plot.ModelObsAcousticLogisTrunc()`] plots detection probability as a function of distance from a receiver;
#' * [`plot.ModelObsAcousticContainer()`] plots a uniform distribution for the probability of a _future_ acoustic detection given the maximum possible distance (container radius) from the receiver (and a maximum movement speed) at the current time;
#' * [`plot.ModelObsDepthUniformSeabed()`] plots a uniform distribution for the probability of a depth observation around a particular `.seabed` depth;
#' * [`plot.ModelObsDepthNormalTruncSeabed()`] plot a truncated normal distribution for the probability of a depth observation around a particularly `.seabed` depth;
#'
#' @return The functions produce a [`plot`]. `invisible(NULL)` is returned.
#' @example man/examples/example-ModelObs-plot.R
#' @author Edward Lavender
#' @name plot.ModelObs
NULL

# plot_dbn_ModelObsAcousticTruncLogis
# (unused)

# Plot the depth likelihood for ModelObsDepthUniformSeabed
plot_dbn_ModelObsDepthUniformSeabed <- function(.panel) {
  plot_dbn_wrap(.dbn = "dbn_ModelObsDepthUniformSeabed",
                .xlim = NULL,
                .xlab = "Depth (m)", .ylab = "Density",
                .panel = .panel)
}

# Plot the depth likelihood for ModelObsDepthNormalTruncSeabed(
plot_dbn_ModelObsDepthNormalTruncSeabed <- function(.panel) {
  plot_dbn_wrap(.dbn = "dbn_ModelObsDepthNormalTruncSeabed",
                .xlim = NULL,
                .xlab = "Depth (m)", .ylab = "Density",
                .panel = .panel)
}

# Plot the depth likelihood for ModelObsDepthNormal
plot_dbn_ModelObsDepthNormal <- function(.panel) {
  plot_dbn_wrap(.dbn = "dbn_ModelObsDepthNormal",
                .xlim = NULL,
                .xlab = "Depth (m)", .ylab = "Density",
                .panel = .panel)
}

#' @rdname plot.ModelObs
#' @export

# User-facing function: plot.ModelObsAcousticLogisTrunc()
plot.ModelObsAcousticLogisTrunc <- function(x,
                                            .sensor_id,
                                            .par = list(),
                                            ...) {
  # Extract data
  x <- x$ModelObsAcousticLogisTrunc
  if (missing(.sensor_id)) {
    .sensor_id <-
      x |>
      distinct(.data$receiver_alpha, .data$receiver_beta, .data$receiver_gamma,
               .keep_all = TRUE) |>
      pull("sensor_id")
  }
  if (!is.null(.sensor_id)) {
    x <-
      x |>
      filter(.data$sensor_id %in% .sensor_id) |>
      # Select one row (e.g., timestep) for each sensor
      group_by(.data$sensor_id) |>
      slice(1L) |>
      ungroup() |>
      as.data.table()
  }

  # Set graphical parameters
  pp <- set_plot_dbn_par(list(mfrow = par_mf(nrow(x))), .par)
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)
  dots <- list(...)
  if (rlang::has_name(dots, "xlim")) {
    xlim <- dots$xlim
  } else {
    xlim <- c(0, max(x$receiver_gamma) * 1.1)
  }

  # Density plots
  # * plot_dbn_*() is not used here
  # * We do not plot the dbn
  # * We plot the probability mass of a detection as a function of distance
  lapply(split(x, seq_row(x)), function(xi) {
    dist   <- seq(xlim[1], xlim[2], length.out = 1000)
    prop   <- plogis(xi$receiver_alpha + xi$receiver_beta * dist)
    prop[dist > xi$receiver_gamma] <- 0
    args   <- list_args(list(main = xi$sensor_id,
                             xlab = "Distance (m)",
                             ylab = "Probability",
                             type = "l"),
                        .dots = list(...))
    args$x <- dist
    args$y <- prop
    do.call(plot, args)
  })
  nothing()
}

#' @rdname plot.ModelObs
#' @export

plot.ModelObsAcousticContainer <- function(x, .radius, .par = list(), ...) {
  x <- x$ModelObsAcousticContainer
  if (missing(.radius)) {
    .radius <- unique(x$radius)
    .radius <- .radius[seq_len(min(length(.radius), 3L))]
  }
  if (!is.null(.radius)) {
    x <-
      x |>
      filter(.data$radius %in% .radius) |>
      # Select one row (e.g., timestep) for each radius
      group_by(.data$radius) |>
      slice(1L) |>
      ungroup() |>
      as.data.table()
  }
  pp <- set_plot_dbn_par(list(mfrow = par_mf(nrow(x))), .par)
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)
  dots <- list(...)
  if (rlang::has_name(dots, "xlim")) {
    xlim <- dots$xlim
  } else {
    xlim <- c(0, max(x$radius) * 1.1, max(.radius))
  }

  # Density plots
  lapply(split(x, seq_row(x)), function(xi) {
    dist   <- seq(xlim[1], xlim[2], length.out = 1000)
    prop   <- rep(1, 1000)
    prop[dist > xi$radius] <- 0
    args   <- list_args(list(main = xi$radius,
                             xlab = "Distance (m)",
                             ylab = "Probability",
                             type = "l"),
                        .dots = list(...))
    args$x <- dist
    args$y <- prop
    do.call(plot, args)
  })
  nothing()

}

#' @rdname plot.ModelObs
#' @export

# User facing function: plot.ModelObsDepthUniformSeabed()
plot.ModelObsDepthUniformSeabed <- function(x,
                                            .seabed = 100,
                                            .par = list(),
                                            ...) {
  # Extract data
  x <- x$ModelObsDepthUniformSeabed

  # Set graphical parameters
  pp <- set_plot_dbn_par(list(mfrow = par_mf(nrow(x))), .par)
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)

  # Density plots
  lapply(split(x, seq_row(x)), function(xi) {
    julia_command(glue('dbn_ModelObsDepthUniformSeabed = truncated(Uniform({.seabed - xi$depth_shallow_eps}, {.seabed + xi$depth_deep_eps}), lower = 0.0);'))
    panel <- list_args(list(main = xi$sensor_id), .dots = list(...))
    plot_dbn_ModelObsDepthUniformSeabed(panel)
  })
  nothing()
}

#' @rdname plot.ModelObs
#' @export

# User facing function: plot.ModelObsDepthNormalTruncSeabed()
plot.ModelObsDepthNormalTruncSeabed <- function(x,
                                                .seabed = 100,
                                                .par = list(),
                                                ...) {
  # Extract data
  x <- x$ModelObsDepthNormalTruncSeabed

  # Set graphical parameters
  pp <- set_plot_dbn_par(list(mfrow = par_mf(nrow(x))), .par)
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)

  # Density plots
  lapply(split(x, seq_row(x)), function(xi) {
    julia_command(glue('dbn_ModelObsDepthNormalTruncSeabed = truncated(Normal({.seabed}, {xi$depth_sigma}), lower = 0.0, upper = {.seabed + xi$depth_deep_eps});'))
    panel <- list_args(list(main = xi$sensor_id), .dots = list(...))
    plot_dbn_ModelObsDepthNormalTruncSeabed(panel)
  })
  nothing()
}


#' @title Movement models
#' @description [`ModelMove`] is Abstract Type in [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) that groups movement model sub-types, of which instances can be created via an `R` `model_move_*()` function.
#'
#' @param .mobility,.dbn_length,.dbn_heading,.dbn_heading_delta,.dbn_z,.dbn_z_delta `Character` strings that define movement model components:
#' * `.mobility`---the maximum movement distance between two time steps (m);
#' * `.dbn_length`---the distribution of step lengths (m);
#' * `.dbn_heading`---the distribution of headings (rad);
#' * `.dbn_heading_delta`---the distribution of changes in heading, i.e., turning angle (rad);
#' * `.dbn_z`---the distribution of depths;
#' * `.dbn_z_delta`---the distribution of changes in depth;
#'
#' @details Movement model sub-types are `Julia` structures that hold the components of movement models. From an `R`-user perspective, you can think of a [`ModelMove`] sub-type as an `S4`-[`class`]-like object, with slots for the components of a movement model. With a movement model instance, we can simulate movements and evaluate the density of movements from one state (location) to another.
#'
#' The following movement models are built in to [`Patter.jl`](https://edwardlavender.github.io/Patter.jl):
#' * Random walks (RWs):
#'    * [`ModelMoveXY`]
#'    * [`ModelMoveXYZ`]
#' * Correlated random walks (CRWs):
#'    * [`ModelMoveCXY`]
#'    * [`ModelMoveCXYZ`]
#'
#' See [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) or `JuliaCall::julia_help("ModelMove")` for the fields of the built-in sub-types. Briefly, all sub-types include:
#' * A `map` field, that defines the region(s) within which movements are permitted. In `R`, it is convenient to represent `map` as a [`SpatRaster`], where `NAs` define inhospitable habitats (e.g., land). This should made available to `Julia` [`ModelMove`] constructors as `env` via [`set_map()`];
#' * The `mobility` parameter;
#' * Additional model-specific components (such as fields for the distribution of step lengths and headings in the case of two-dimensional random walks);
#'
#' In [`patter`], movement models are required:
#' * To simulate movement paths, via [`sim_path_walk()`];
#' * To run the particle filter, via [`pf_filter()`];
#' * To run the particle smoother, via [`pf_smoother_two_filter()`];
#'
#' In `R` functions, the movement-model instance is specified via the `.model_move` argument. This argument expects a `character` string defining a [`ModelMove`] instance that can be evaluated in `Julia` (that is, a [`ModelMove`] constructor). `model_move_*()` functions are convenience functions for the specification of these constructors for the built-in sub-types. All [`ModelMove`] instances contain a `map` field that defines the region(s) within which movements are permitted. To use a `model_move_*()` function, the map should be available in `Julia` as `env` (see [`set_map()`]). The additional components of the movement model are specified via `model_move_*()` function arguments as `character` strings of `Julia` code. Currently implemented `model_move_*()` functions are:
#' * [`model_move_xy()`], which specifies a RW in X and Y of sub-type [`ModelMoveXY`] in terms of the distributions of step lengths and headings;
#' * [`model_move_xyz()`], which specifies a RW in X, Y and Z of sub-type [`ModelMoveXY`] in terms of the distributions of step lengths, headings and depths;
#' * [`model_move_cxy()`], which specifies a CRW in X and Y of sub-type [`ModelMoveXY`] in terms of the distributions of step lengths and turning angles;
#' * [`model_move_cxyz()`], which specifies a CRW in X, Y and Z of sub-type [`ModelMoveCXYZ`] in terms of the distributions of step lengths, turning angles and changes in depth;
#'
#' See [here](https://discourse.julialang.org/t/a-comparison-of-common-distributions-in-julia-python-and-r/61604) for the translations of distributions in `R` (e.g., `*norm()`) into `Julia` (e.g., `Normal()`).
#'
#' To plot the dimensions of a movement model, see [`plot.ModelMove`]. To visualise realisations of a model, see [`sim_path_walk()`].
#'
#' In `Julia`, [`ModelMove`] instances are used to simulate states via [`Patter.simulate_step()`](https://edwardlavender.github.io/Patter.jl). In the particle smoother, the density of movement from one state to another is evaluated by [`Patter.logpdf_step()`](https://edwardlavender.github.io/Patter.jl). These are generic functions. Different methods are dispatched according to the input model. For the built-in [`ModelMove`] sub-types, corresponding methods for these routines are also built-in. For custom [`ModelMove`] sub-types, the methods need to be provided.
#'
#' To use custom [`ModelMove`] sub-types, see Examples.
#'
#' @returns `model_move_*()` functions return a `character` string that defines a [`ModelMove`] instance for evaluation in `Julia`. The [`class`] of the output is `character` plus `ModelMove` and `ModelMoveXY`, `ModelMoveXYZ`, `ModelMoveCXY` or `ModelMoveCXYZ` (see [`plot.ModelMove`]). If the map (`env`) does not exist in `Julia`, an error is thrown.
#'
#' @example man/examples/example-ModelMove.R
#' @inherit State seealso
#' @author Edward Lavender
#' @name ModelMove
#' @aliases .model_move model_move ModelMoveXY ModelMoveXYZ ModelMoveCXY ModelMoveCXYZ model_move_xy model_move_xyz model_move_cxy model_move_cxyz

#' @rdname ModelMove
#' @export

model_move_xy <- function(.mobility = "750.0",
                    .dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
                    .dbn_heading = "Uniform(-pi, pi)") {
  julia_check_exists("env")
  cmd <- glue('ModelMoveXY(env, {.mobility}, {.dbn_length}, {.dbn_heading});')
  class(cmd) <- c(class(cmd), "ModelMove", "ModelMoveXY")
  cmd
}

#' @rdname ModelMove
#' @export

model_move_xyz <- function(.mobility = "750.0",
                     .dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
                     .dbn_heading = "Uniform(-pi, pi)",
                     .dbn_z = "truncated(Normal(100.0, 250.0), lower = 0.0, upper = 350.0)") {
  julia_check_exists("env")
  cmd <- glue('ModelMoveXYZ(env, {.mobility}, {.dbn_length}, {.dbn_heading}, {.dbn_z});')
  class(cmd) <- c(class(cmd), "ModelMove", "ModelMoveXYZ")
  cmd
}

#' @rdname ModelMove
#' @export

model_move_cxy <- function(.mobility = "750.0",
                     .dbn_length = "truncated(Gamma(1.0, 750.0), upper = 750.0)",
                     .dbn_heading_delta = "Normal(0, 0.5)") {
  julia_check_exists("env")
  cmd <- glue('ModelMoveCXY(env, {.mobility}, {.dbn_length}, {.dbn_heading_delta});')
  class(cmd) <- c(class(cmd), "ModelMove", "ModelMoveCXY")
  cmd
}

#' @rdname ModelMove
#' @export

model_move_cxyz <- function(.mobility = "750.0",
                      .dbn_length = "truncated(Gamma(1.0, 750.0), upper = 750.0)",
                      .dbn_heading_delta = "Normal(0, 0.5)",
                      .dbn_z_delta = "Normal(0, 3.5)") {
  julia_check_exists("env")
  cmd <- glue('ModelMoveCXYZ(env, {.mobility}, {.dbn_length}, {.dbn_heading_delta}, {.dbn_z_delta});')
  class(cmd) <- c(class(cmd), "ModelMove", "ModelMoveCXYZ")
  cmd
}


#' @title Movement model plots
#' @description [`plot()`] methods for movement models (see [`ModelMove`]).
#' @param x A [`ModelMove`]-class object from a `model_move_*()` function (e.g., [`model_move_xy()`].
#' @param .panel_length,.panel_heading,.panel_heading_delta,.panel_z,.panel_z_delta Panel properties:
#' * `NULL` suppresses the panel;
#' * `list()` uses default graphical arguments;
#' * A named `list` of arguments, passed to [`plot()`], customises the panel;
#' @param .par Graphical parameters:
#' * `NULL` uses current graphical parameters;
#' * `list()` uses default graphical parameters;
#' * A named `list` of arguments, passed to [`par()`], customises parameters;
#' @param ... Additional arguments, passed to [`plot()`], which affect all panels;
#' @details For each in-built [`ModelMove`]-class (e.g., [`ModelMoveXY`]), a corresponding [`plot()`] method is provided that plots the component probability-density distributions (e.g., the distribution of step lengths and turning angles) for any model instance. Under default options, a multi-panelled plot is produced, with one panel for each model dimension (e.g., the step length and the heading). Use `.panel_*` arguments to customise individual panels, `.par` to set graphical parameters and `...` to customise all panels. Set `.panel_*` `xlim` to specify the x-axis range over which distributions are shown. If unspecified, probability density is plotted along a predefined range or from the 0.0001 to 0.9999 quantiles of the distribution.
#' @return The functions produce a [`plot`]. `invisible(NULL)` is returned.
#' @example man/examples/example-ModelMove-plot.R
#' @seealso Use [`sim_path_walk()`] to visualise realisations of a movement model (i.e., trajectories).
#' @author Edward Lavender
#' @name plot.ModelMove
NULL

# Set plot parameters for plot_dbn_*()
# * .default is a list of default parameters
# * .par is the list of parameters from the user
# * NULL is permitted in which case par is unset
set_plot_dbn_par <- function(.default, .par) {
  pp <- par(no.readonly = TRUE)
  if (!is.null(.par)) {
    .par <- list_args(.default, .par)
    pp   <- do.call(par, .par)
  }
  pp
}

# Set x limits for plot_dbn_*()
# * .dbn is a character that defines the distribution in model_move e.g., "dbn_length"
# * .panel is a list of graphical arguments for that panel (plot) e.g., xlim
# * If xlim is set, the distribution is plotted over xlim
# * Otherwise, the distribution is plotted over the distribution quantiles
# * (from 0.0001 to 0.9999)
set_plot_dbn_lim <- function(.dbn, .panel) {
  if (!is.null(.panel$xlim)) {
    julia_assign("from", .panel$xlim[1])
    julia_assign("to", .panel$xlim[2])
  } else {
    julia_command(glue('from = quantile({.dbn}, 0.0001);'))
    julia_command(glue('to = quantile({.dbn}, 0.9999);'))
  }
  nothing()
}

# A wrapper function to plot any distribution
# * .xlim, .xlab and .ylab are defaults for a particular distribution
# * These can be overwritten by user-supplied options in .panel or ...
# * .panel = NULL suppresses plotting
plot_dbn_wrap <- function(.dbn,
                          .xlim, .xlab = "x", .ylab = "Density",
                          .panel = list(), ...) {
  # Handle inputs
  if (is.null(.panel)) {
    return(nothing())
  }
  # Set graphical properties
  .panel <- list_args(list(xlim = .xlim,
                           xlab = .xlab,
                           ylab = .ylab,
                           type = "l"),
                      list(...),
                      .panel)
  # Plot distribution
  set_plot_dbn_lim(.dbn = .dbn, .panel = .panel)
  julia_command('x = range(from, stop = to, length = 1000);')
  julia_command(glue('y = pdf({.dbn}, x);'))
  plot_dbn(.panel = .panel)
}

# Plot a distribution (x, y) with .panel graphical parameters
# * .panel contains ... from the parent function (plot_dbn_wrap())
plot_dbn <- function(.panel) {
  julia_check_exists("x", "y")
  .panel$x    <- julia_eval('x')
  .panel$y    <- julia_eval('y')
  do.call(plot, .panel)
  nothing()
}

# Plot a distribution of step lengths
plot_dbn_length <- function(.panel, ...) {
  julia_check_exists("model_move")
  plot_dbn_wrap(.dbn = "model_move.dbn_length",
                .xlim = c(-0.001, julia_eval("model_move.mobility") + 0.001),
                .xlab = "Length (m)", .ylab = "Density",
                .panel = .panel, ...)
}

# Plot a distribution of headings
plot_dbn_heading <- function(.panel, ...) {
  julia_check_exists("model_move")
  plot_dbn_wrap(.dbn = "model_move.dbn_heading",
                .xlim = c(-2*pi, 2 * pi),
                .xlab = "Heading (rad)", .ylab = "Density",
                .panel = .panel, ...)
}

# Plot a distribution of turning angles
plot_dbn_heading_delta <- function(.panel, ...) {
  julia_check_exists("model_move")
  plot_dbn_wrap(.dbn = "model_move.dbn_heading_delta",
                .xlim = c(-2*pi, 2 * pi),
                .xlab = "Turning angle (rad)", .ylab = "Density",
                .panel = .panel, ...)
}

# Plot a distribution of depths
plot_dbn_z <- function(.panel, ...) {
  julia_check_exists("model_move")
  plot_dbn_wrap(.dbn = "model_move.dbn_z",
                .xlim = NULL,
                .xlab = "Depth (m)", .ylab = "Density",
                .panel = .panel, ...)
}

# Plot a distribution of changes in depth
plot_dbn_z_delta <- function(.panel, ...) {
  julia_check_exists("model_move")
  plot_dbn_wrap(.dbn = "model_move.dbn_z_delta",
                .xlim = NULL,
                .xlab = "Depth change (m)", .ylab = "Density",
                .panel = .panel, ...)
}

#' @rdname plot.ModelMove
#' @export

# User-facing function: plot.ModelMoveXY()
plot.ModelMoveXY <- function(x,
                             .panel_length = list(),
                             .panel_heading = list(),
                             .par = list(),
                             ...) {
  # Set graphical parameters
  pp <- set_plot_dbn_par(list(mfrow = c(1, 2)), .par)
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)
  # Density plots
  set_model_move(x)
  plot_dbn_length(.panel = .panel_length, ...)
  plot_dbn_heading(.panel = .panel_heading, ...)
  nothing()
}

#' @rdname plot.ModelMove
#' @export

# User-facing function: plot.ModelMoveXYZ()
plot.ModelMoveXYZ <- function(x,
                              .panel_length = list(),
                              .panel_heading = list(),
                              .panel_z = list(),
                              .par = list(),
                              ...) {
  # Set graphical parameters
  pp <- set_plot_dbn_par(list(mfrow = c(1, 3)), .par)
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)
  # Density plots
  set_model_move(x)
  plot_dbn_length(.panel = .panel_length, ...)
  plot_dbn_heading(.panel = .panel_heading, ...)
  plot_dbn_z(.panel = .panel_z, ...)
  nothing()
}

#' @rdname plot.ModelMove
#' @export

# User-facing function: plot.ModelMoveCXY()
plot.ModelMoveCXY <- function(x,
                              .panel_length = list(),
                              .panel_heading_delta = list(),
                              .par = list(),
                              ...) {
  # Set graphical parameters
  pp <- set_plot_dbn_par(list(mfrow = c(1, 2)), .par)
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)
  # Density plots
  set_model_move(x)
  plot_dbn_length(.panel = .panel_length, ...)
  plot_dbn_heading_delta(.panel = .panel_heading_delta, ...)
  nothing()
}

#' @rdname plot.ModelMove
#' @export

# User-facing function: plot.ModelMoveCXYZ()
plot.ModelMoveCXYZ <- function(x,
                               .panel_length = list(),
                               .panel_heading_delta = list(),
                               .panel_z_delta = list(),
                               .par = list(),
                               ...) {
  # Set graphical parameters
  pp <- set_plot_dbn_par(list(mfrow = c(1, 3)), .par)
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)
  # Density plots
  set_model_move(x)
  plot_dbn_length(.panel = .panel_length, ...)
  plot_dbn_heading_delta(.panel = .panel_heading_delta, ...)
  plot_dbn_z_delta(.panel = .panel_z_delta, ...)
  nothing()
}
