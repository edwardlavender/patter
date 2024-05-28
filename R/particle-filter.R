#' @title PF: particle filter
#' @description This function runs the particle filter. The filter samples possible states (typically locations, termed particles) of an animal at each time point given the data up to (and including) that time point and a movement model.
#'
#' @param .map A [`SpatRaster`] that defines the study area for the simulation (see [`glossary`]). Here, `.map` is used to:
#' * Simulate initial states if `.xinit = NULL` (via [`sim_states_init()`]);
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation. Here, `.timeline` is used to:
#' * Define the time steps of the simulation;
#' @param .state,.xinit,.xinit_pars Arguments used to simulate initial states (see [`sim_states_init()`]).
#' * `.state`---A `character` that defines the [`State`] sub-type;
#' * `.xinit`---`NULL` or a [`data.table`] that defines the initial states for the simulation;
#' * `.xinit_pars`---A named `list` of parameters passed to the `.pars` argument of [`sim_states_init()`];
#' @param .yobs,.model_obs Observations and observation models.
#' * `.yobs` is a `list` of formatted datasets, one for each data type (see [`glossary`]);
#' * `.model_obs` is a `character` vector of [`ModelObs`] sub-types, one for each dataset in `yobs`;
#' @param .model_move,.n_move The movement model.
#' * `.model_move`---A `character` string that defines the movement model (see [`ModelMove`]);
#' * `.n_move`---An `integer` that defines the number of attempts to find a legal move;
#' @param .n_particle An `integer` that defines the number of particles.
#' @param .n_resample A `double` that defines the effective sample size at which to re-sample particles.
#' @param .n_record An `integer` that defines the number of recorded particles at each time step.
#' @param .direction A `character` string that defines the direction of the filter:
#' * `"forward"` runs the filter from `.timeline[1]:.timeline[length(.timeline)]`;
#' * `"backward"` runs the filter from `.timeline[length(.timeline)]:.timeline[1]`;
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' # Overview
#' The particle filter iterates over time steps, simulating states (termed 'particles') that are consistent with the preceding data and a movement model at each time step.
#'
#' The initial states for the algorithm are defined by `.xinit` or simulated via [`sim_states_init()`]. The word 'state' typically means location but may include additional parameters. If the initial state of the animal is known, it should be supplied via `.xinit`. Otherwise, [`sim_states_init()`] samples `.n_particle` initial coordinates from `.map` (via [`coords_init()`]), which are then translated into a [`data.table`] of states (that is, `.xinit`, via [`states_init()`]). The regions on `.map` from which initial coordinates are sampled can be restricted by the observations (`.yobs`) and other parameters. For automated handling of custom states and observation models at this stage, see the Details for [`sim_states_init()`].
#'
#' The filter comprises three stages:
#' 1. A movement step, in which we simulate possible states (particles) for the individual (for time steps 2, ..., T).
#' 2. A weights step, in which we calculate particle weights from the log-likelihood of the data at each particle.
#' 3. A re-sampling step, in which we optionally re-sample valid states using the weights.
#'
#' The time complexity of the algorithm is \eqn{O(TN)}.
#'
#' The filter is implemented by the `Julia` function [`Patter.particle_filter()`](https://github.com/edwardlavender/Patter.jl). To multi-thread movement and likelihood evaluations, set the number of threads via [`julia_connect()`]. See [`Patter.particle_filter()`](https://github.com/edwardlavender/Patter.jl) or `JuliaCall::julia_help("particle_filter")` for further information.
#'
#' # Algorithms
#'
#' This is highly flexible routine for the reconstruction of the possible locations of an individual through time, given the data up to that time point. By modifying the observation models, it is straightforward to implement the ACPF, DCPF and ACDCPF algorithms introduced by Lavender et al. (2023) for reconstructing animal movements in passive acoustic telemetry systems using (a) acoustic time series, (b) archival time series and (c) acoustic and archival time series. [`pf_filter()`] thus replaces (and enhances) the [`flapper::ac()`](https://edwardlavender.github.io/flapper/reference/ac.html), [`flapper::dc()`](https://edwardlavender.github.io/flapper/reference/dc.html), [`flapper::acdc()`](https://edwardlavender.github.io/flapper/reference/acdc.html) and [`flapper::pf()`](https://edwardlavender.github.io/flapper/reference/pf.html) functions.
#'
#' @returns The function returns a [`pf_particles-class`] object.
#'
#' @example man/examples/example-pf_filter.R
#' @inherit assemble seealso
#' @author Edward Lavender
#' @export

pf_filter <- function(.map,
                      .timeline,
                      .state = "StateXY", .xinit = NULL, .xinit_pars = list(),
                      .yobs = list(),
                      .model_obs,
                      .model_move = move_xy(),
                      .n_move = 1e5L,
                      .n_particle = 1000L,
                      .n_resample = .n_record * 0.5,
                      .n_record = 1e3L,
                      .direction = c("forward", "backward"),
                      .verbose = getOption("patter.verbose")) {

  #### Initiate
  cats <- cat_setup(.fun = "pf_filter", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Check user inputs
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Checking user inputs..."))
  check_inherits(.state, "character")
  check_inherits(.model_move, "character")
  .direction <- match.arg(.direction)
  tzs <- c(tz(.timeline), sapply(.yobs, \(d) tz(d$timestamp)))
  if (length(unique(tzs)) != 1L) {
    abort("There is a mismatch between the time zones of `.timeline` and/or `.yobs` `timestamp`s ({str_items(tzs, quo = '\"')}).",
          .envir = environment())
  }

  #### Set initial state
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Setting initial states..."))
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
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Setting observations..."))
  set_yobs_via_datasets(.datasets = .yobs, .model_obs = .model_obs)
  set_model_move(.model_move)

  #### Run filter
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Running filter..."))
  pf_obj <- set_pf_filter(.n_move = .n_move,
                          .n_resample = .n_resample,
                          .n_record = .n_record,
                          .direction = .direction)

  #### Get particles in R
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Collating outputs..."))
  pf_particles(.xinit = .xinit, .pf_obj = pf_obj)

}
