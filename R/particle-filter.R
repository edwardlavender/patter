#' @title PF: particle filter
#' @description This function runs the particle filter. The filter samples possible states (typically locations, termed particles) of an animal at each time point given the data up to (and including) that time point and a movement model.
#'
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation. Here, `.timeline` is used to:
#' * Define the time steps of the simulation;
#' @param .state,.xinit Arguments used to simulate initial states.
#' * `.state`---A `character` that defines the [`State`] sub-type;
#' * `.xinit`---`NULL` or a [`data.table`] that defines the initial states for the simulation;
#' @param .yobs The observations. Acceptable inputs are:
#' * A named `list` of formatted datasets, one for each data type (see [`glossary`]);
#' * An empty `list`, to run the filter without observations;
#' * `missing`, if the datasets are already available in the `Julia` session (from a previous run of [`pf_filter()`]) and you want to re-run the filter without the penalty of re-exporting the observations;
#' @param .model_move,.n_move The movement model.
#' * `.model_move`---A `character` string that defines the movement model (see [`ModelMove`]);
#' * `.n_move`---An `integer` that defines the number of attempts to find a legal move;
#' @param .n_particle An `integer` that defines the number of particles.
#' @param .n_resample,.t_resample Resampling options.
#' * `.n_resample` is an a `double` that defines the effective sample size (ESS) at which to re-sample particles;
#' * `.t_resample` is `NULL` or an `integer`/`integer` vector that defines the time steps at which to resample particles, regardless of the ESS;
#'
#' Guidance:
#' * To resample at every time step, set `.n_resample = as.numeric(.n_particle)` (default) or `.t_resample = 1:length(.timeline)`;
#' * To only resample at selected time steps, set `.t_resample` as required and set `.n_resample > as.numeric(.n_particle)`;
#' * To only resample based on ESS, set `.t_resample = NULL` and `.n_resample` as required;
#'
#' @param .n_record An `integer` that defines the number of recorded particles at each time step.
#' @param .n_iter An `integer` that defines the number of iterations of the filter.
#' @param .direction A `character` string that defines the direction of the filter:
#' * `"forward"` runs the filter from `.timeline[1]:.timeline[length(.timeline)]`;
#' * `"backward"` runs the filter from `.timeline[length(.timeline)]:.timeline[1]`;
#' @param .collect A `logical` variable that defines whether or not to collect outputs from the `Julia` session in `R`.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' # Overview
#' The particle filter iterates over time steps, simulating states (termed 'particles') that are consistent with the preceding data and a movement model at each time step.
#'
#' A raster map of study area must be exported to `Julia` for this function (see [`set_map()`]).
#'
#' The initial states for the algorithm are defined by `.xinit` or simulated via [`Patter.simulate_states_init()`](https://github.com/edwardlavender/Patter.jl). The word 'state' typically means location but may include additional parameters. If the initial state of the animal is known, it should be supplied via `.xinit`. Otherwise, [`Patter.simulate_states_init()`](https://github.com/edwardlavender/Patter.jl) samples `.n_particle` initial coordinates from `.map` (via [`Patter.coords_init()`](https://github.com/edwardlavender/Patter.jl)), which are then translated into a `DataFrame` of states (that is, `.xinit`, via [`Patter.states_init()`](https://github.com/edwardlavender/Patter.jl)). The regions on the map from which initial coordinates are sampled can be restricted by the observations (`.yobs`) and other parameters. For automated handling of custom states and observation models at this stage, custom `Patter.map_init` and `Patter.states_init` methods are required (see the Details for [`Patter.simulate_states_init()`](https://github.com/edwardlavender/Patter.jl)).
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
#' @returns [`Patter.particle_filter()`](https://github.com/edwardlavender/Patter.jl) creates a NamedTuple in the `Julia` session (named  `pfwd` or `pbwd` depending on `.direction`). If `.collect = TRUE`, [`pf_filter()`] collects the outputs in `R` as a [`pf_particles-class`] object. Otherwise, `invisible(NULL)` is returned.
#'
#' @example man/examples/example-pf_filter.R
#' @inherit assemble seealso
#' @author Edward Lavender
#' @export

pf_filter <- function(.timeline,
                      .state = "StateXY",
                      .xinit = NULL,
                      .model_move = move_xy(),
                      .yobs,
                      .n_move = 1e5L,
                      .n_particle = 1000L,
                      .n_resample = as.numeric(.n_particle),
                      .t_resample = NULL,
                      .n_record = 1e3L,
                      .n_iter = 1L,
                      .direction = c("forward", "backward"),
                      .collect = TRUE,
                      .verbose = getOption("patter.verbose")) {

  #### Initiate
  cats <- cat_setup(.fun = "pf_filter", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Initialise filter
  .direction <- match.arg(.direction)
  pf_filter_init(.timeline = .timeline,
                 .state = .state,
                 .xinit = .xinit,
                 .model_move = .model_move,
                 .yobs = .yobs,
                 .n_particle = .n_particle,
                 .direction = .direction)

  #### Run filter
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Running filter..."))
  pf_obj <- set_pf_filter(.n_move = .n_move,
                          .n_resample = .n_resample,
                          .t_resample = .t_resample,
                          .n_record = .n_record,
                          .n_iter = .n_iter,
                          .direction = .direction)

  #### Check convergence
  # Issue a warning for convergence failures
  if (isFALSE(julia_eval(glue('{pf_obj}.convergence')))) {
    warn("The particle filter failed to converge.")
  }

  #### Get particles in R
  if (.collect) {
    cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Collating outputs..."))
    out <- pf_particles(.pf_obj = pf_obj)
  } else {
    out <- nothing()
  }
  out

}
