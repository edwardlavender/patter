#' @title PF: particle filter
#' @description This function runs the particle filter. The filter samples possible states (typically locations, termed particles) of an animal at each time point given the data up to (and including) that time point and a movement model.
#'
#' @param .map A [`SpatRaster`] that defines the study area for the simulation (see [`glossary`]). Here, `.map` is used to:
#' * Simulate initial states if `.xinit = NULL` (via [`sim_states_init()`]);
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation. Here, `.timeline` is used to:
#' * Define the time steps of the simulation;
#' @param .state,.xinit,.xinit_pars Arguments used to simulate initial states (see [`sim_states_init()`]).
#' * `.state`---A `character` that defines the state type (see [`glossary`]);
#' * `.xinit`---`NULL` or a [`data.table`] that defines the initial states for the simulation;
#' * `.xinit_pars`---A named `list` of parameters passed to [`sim_states_init()`];
#' @param .yobs,.model_obs Observations and observation models.
#' * `.yobs` is a `list` of formatted datasets, one for each data type (see [`glossary`]).
#' * `.model_obs` is a named `list` of `ModelObs` subtypes, one for each dataset in `yobs` (see [`glossary`]).
#' @param .model_move,.n_move The movement model.
#' * `.model_move`---A `character` string that defines the movement model (see [`move`] and [`glossary`]).
#' * `.n_move`---An `integer` that defines the number of attempts to find a legal move.
#' @param .n_particle An `integer` that defines the number of particles.
#' @param .n_resample A `double` that defines the effective sample size at which to re-sample particles.
#' @param .n_record An `integer` that defines the number of recorded particles at each time step.
#' @param .direction A `character` string that defines the direction of the filter:
#' * `"forward"` runs the filter from `.timeline[1]:.timeline[length(.timeline)]`;
#' * `"backward"` runs the filter from `.timeline[length(.timeline)]:.timeline[1]`
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' # Overview
#' The particle filter iterates over time steps, simulating states (termed 'particles') that are consistent with the preceding data and a movement model at each time step.
#'
#' The initial states for the algorithm are defined by `.xinit` or simulated via [`sim_states_init()`]. The word 'state' typically means location but may include additional parameters. If the initial state of the animal is known, it should be supplied via `.xinit`. Otherwise, [`sim_states_init()`] samples `.n_particle` initial coordinates from `.map` (via [`coords_init()`]), which are then translated into a [`data.table`] of states (that is, `.xinit`, via [`states_init()`]). The regions on `.map` from which initial coordinates are sampled can be restricted by the observations (`.yobs`) and other parameters. For automated handling of custom states and observation models at this stage, see the Details for [`sim_states_init()`].
#'
#' At subsequent time steps, the filter comprises three stages:
#' 1. A movement step, in which we simulate possible states (particles) for the individual.
#' 2. A weights step, in which we calculate particle weights from the log-likelihood of the data at each particle.
#' 3. A re-sampling step, in which we optionally re-sample valid states using the weights.
#'
#' # `Julia` back-end
#'
#' The filter is implemented by the `Julia` function `Patter.particle_filter()`. The algorithm is initiated using a `Vector` of `.n_particle` `State`s, defined from `.xinit` that is used to initiate the filter. For every time step in the `timeline`, the internal function `Patter.simulate_move()` simulates the movement of particles from previous states into new states using the movement model specified by `.model_move`, which defines a `ModelObs` instance in `Julia`. `Patter.simulate_move()` is an iterative wrapper for a `Patter.simulate_step()` method that simulates a new state using the previous state and the movement model. `Patter.simulate_move()` implements `Patter.simulate_step()` iteratively until a legal move is found (or `.n_move` is reached). Illegal moves are those that land in `NaN` locations on the map or, in the case of states that include a depth (`z`) component, are below the depth of the seabed. Particles that fail to generate illegal moves are eventually killed by re-sampling (see below).
#'
#' For each valid state, the log-probability of the data given that state is evaluated via `Patter.logpdf_obs()`. The maximum log-probability across all particles is recorded at each time step as an algorithm diagnostic.
#'
#' To multi-thread movement and likelihood evaluations, set the number of threads via [`julia_connect()`].
#'
#' To implement custom movement and observation models subtypes, see the advanced vignette (TO DO).
#'
#' Particles are periodically re-sampled, with replacement, using the low-variance systematic re-sampling algorithm, when the effective sample size is less than or equal to `.n_resample`. This has the effect of eliminating impossible particles and duplicating likely ones.
#'
#' The algorithm continues in this way, iterating over the `timeline`, simulating, weighting and (re)sampling particles. At each time step, `.n_record` particles are saved in memory. If the function fails to converge, a [`warning`] is returned alongside the outputs up to that time step. Otherwise, the function will continue to the end of the time series.
#'
#' # Algorithms
#'
#' This is highly flexible routine for the reconstruction of the possible locations of an individual through time, given the data up to that time point. By modifying the observation models, it is straightforward to implement the ACPF, DCPF and ACDCPF algorithms introduced by Lavender et al. (2023) for reconstructing movements using (a) acoustic time series, (b) archival time series and (c) acoustic and archival time series. [`pf_filter()`] thus replaces (and enhances) the [`flapper::ac()`](https://edwardlavender.github.io/flapper/reference/ac.html), [`flapper::dc()`](https://edwardlavender.github.io/flapper/reference/dc.html), [`flapper::acdc()`](https://edwardlavender.github.io/flapper/reference/acdc.html) and [`flapper::pf()`](https://edwardlavender.github.io/flapper/reference/pf.html) functions.
#'
#' # Convergence and diagnostics
#'
#' Algorithm convergence is not guaranteed. The algorithm may reach a dead-end---a time step at which there are no valid locations into which the algorithm can step. This may be due to data errors, incorrect assumptions, insufficient sampling effort or poor tuning-parameter settings
#'
#' @returns
#'
#' @example
#'
#' @seealso
#'
#' @author Edward Lavender
#' @export

pf_filter <- function(.map,
                      .timeline,
                      .state = "StateXY", .xinit = NULL, .xinit_pars = list(),
                      .yobs = list(),
                      .model_obs,
                      .model_move,
                      .n_move = 1e5L,
                      .n_particle = 1000L,
                      .n_resample = .n_record * 0.5,
                      .n_record = 1e3L,
                      .direction = c("forward", "backward"),
                      .verbose = getOption("patter.verbose")) {

  #### Set up messages
  t_onset <- Sys.time()
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_filter", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_filter", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Check user inputs
  cat_log(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Checking user inputs..."))
  check_inherits(.state, "character")
  check_inherits(.model_move, "character")
  .direction <- match.arg(.direction)

  #### Set initial state
  # TO DO
  # * Add direction
  cat_log(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Setting initial states..."))
  set_timeline(.timeline)
  .xinit <- sim_states_init(.map = .map,
                            .timeline = .timeline,
                            .direction = .direction,
                            .datasets = .yobs,
                            .models = .model_obs,
                            .pars = .xinit_pars,
                            .state = .state,
                            .xinit = .xinit,
                            .n = .n_particle)
  set_states_init(.xinit = .xinit, .state = .state)

  #### Set filter arguments
  cat_log(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Setting observations..."))
  set_yobs_via_datasets(.datasets = .yobs, .models = .model_obs)
  set_move(.model_move)

  #### Run filter
  cat_log(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Running filter..."))
  pf_obj <- set_pf_filter(.n_move = .n_move,
                          .n_resample = .n_resample,
                          .n_record = .n_record,
                          .direction = .direction)

  #### Get particles in R
  # TO DO
  # * Consider the inclusion of bathymetry data for consistency
  # * Or remove inclusion of bathymetry data from sim_path_walk()
  cat_log(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Collating outputs..."))
  out <- julia_eval(glue('Patter.r_get_particles({pf_obj});'))
  timestep <- timestamp <- NULL
  out$diagnostics <-
    out$diagnostics |>
    as.data.table()
  out$states <-
    out$states |>
    collapse::join(out$diagnostics[, list(timestep, timestamp)],
                   on = "timestep", verbose = FALSE) |>
    select("path_id", "timestep", "timestamp", everything()) |>
    as.data.table()
  out$xinit <- .xinit
  unclass(out)

}
