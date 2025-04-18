% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/particle-filter.R
\name{pf_filter}
\alias{pf_filter}
\title{PF: particle filter}
\usage{
pf_filter(
  .timeline,
  .state = "StateXY",
  .xinit = NULL,
  .model_move = model_move_xy(),
  .yobs,
  .n_move = 100000L,
  .n_particle = 1000L,
  .n_resample = as.numeric(.n_particle),
  .t_resample = NULL,
  .n_record = 1000L,
  .n_iter = 1L,
  .direction = c("forward", "backward"),
  .batch = NULL,
  .collect = TRUE,
  .progress = julia_progress(),
  .verbose = getOption("patter.verbose")
)
}
\arguments{
\item{.timeline}{A \code{POSIXct} vector of regularly spaced time stamps that defines the timeline for the simulation. Here, \code{.timeline} is used to:
\itemize{
\item Define the time steps of the simulation;
}}

\item{.state, .xinit}{Arguments used to simulate initial states.
\itemize{
\item \code{.state}---A \code{character} that defines the \code{\link{State}} sub-type;
\item \code{.xinit}---\code{NULL} or a \code{\link[data.table:data.table]{data.table::data.table}} that defines the initial states for the simulation;
}}

\item{.model_move, .n_move}{The movement model.
\itemize{
\item \code{.model_move}---A \code{character} string that defines the movement model (see \code{\link{ModelMove}});
\item \code{.n_move}---An \code{integer} that defines the number of attempts to find a legal move;
}}

\item{.yobs}{The observations. Acceptable inputs are:
\itemize{
\item A named \code{list} of formatted datasets, one for each data type (see \code{\link{glossary}});
\item An empty \code{list}, to run the filter without observations;
\item \code{missing}, if the datasets are already available in the \code{Julia} session (from a previous run of \code{\link[=pf_filter]{pf_filter()}}) and you want to re-run the filter without the penalty of re-exporting the observations;
}}

\item{.n_particle}{An \code{integer} that defines the number of particles.}

\item{.n_resample, .t_resample}{Resampling options.
\itemize{
\item \code{.n_resample} is an a \code{double} that defines the effective sample size (ESS) at which to re-sample particles;
\item \code{.t_resample} is \code{NULL} or an \code{integer}/\code{integer} vector that defines the time steps at which to resample particles, regardless of the ESS;
}

Guidance:
\itemize{
\item To resample at every time step, set \code{.n_resample = as.numeric(.n_particle)} (default) or \code{.t_resample = 1:length(.timeline)};
\item To only resample at selected time steps, set \code{.t_resample} as required and set \code{.n_resample > as.numeric(.n_particle)};
\item To only resample based on ESS, set \code{.t_resample = NULL} and \code{.n_resample} as required;
}}

\item{.n_record}{An \code{integer} that defines the number of recorded particles at each time step.}

\item{.n_iter}{An \code{integer} that defines the number of iterations of the filter.}

\item{.direction}{A \code{character} string that defines the direction of the filter:
\itemize{
\item \code{"forward"} runs the filter from \code{.timeline[1]:.timeline[length(.timeline)]};
\item \code{"backward"} runs the filter from \code{.timeline[length(.timeline)]:.timeline[1]};
}}

\item{.batch}{(optional) Batching controls:
\itemize{
\item Use \code{NULL} to retain all particles for the whole \code{.timeline} (\code{.n_record} particles at each time step) in memory;
\item Use \code{character} vector of \code{.jld2} file paths to write particles in sequential batches to file (as \code{Julia} \verb{Matrix\{<:State\}} objects); for example:
\itemize{
\item \verb{./fwd-1.jld2, ./fwd-2.jld2, ...} if \code{.direction = "forward"};
\item \verb{./bwd-1.jld2, ./bwd-2.jld2, ...} if \verb{.direction = "backward};
}
}

If specified, \code{.batch} is sorted alphanumerically (in \code{Julia}) such that 'lower' batches (e.g., \verb{./fwd-1.jld2}, \verb{./bwd-1.jld2}) align with the start of the \code{.timeline} and 'higher' batches (e.g., \verb{./fwd-3.jld2}, \verb{./bwd-3.jld2}) align with the end of the \code{.timeline} (irrespective of \code{.direction}). For smoothing, the same number of batches must be used for both filter runs (see \code{\link[=pf_smoother_two_filter]{pf_smoother_two_filter()}}).

This option is designed to reduce memory demand (especially on clusters). Larger numbers of batches reduce memory demand, but incur the speed penalty of writing particles to disk. When there are only a handful of batches, this should be negligible. For examples of this argument in action, see \code{\link[=pf_smoother_two_filter]{pf_smoother_two_filter()}}.}

\item{.collect}{A \code{logical} variable that defines whether or not to collect outputs from the \code{Julia} session in \code{R}.}

\item{.progress}{Progress controls (see \code{\link{patter-progress}} for supported options). If enabled, one progress bar is shown for each \code{.batch}.}

\item{.verbose}{User output control (see \code{\link{patter-progress}} for supported options).}
}
\value{
\href{https://github.com/edwardlavender/Patter.jl}{\code{Patter.particle_filter()}} creates a NamedTuple in the \code{Julia} session (named  \code{pfwd} or \code{pbwd} depending on \code{.direction}). If \code{.batch = NULL}, the NamedTuple contains particles (\code{states}) ; otherwise, the \code{states} element is \code{nothing} and \code{states} are written to \code{.jld2} files (as variables named \code{xfwd} or \code{xbwd}). If \code{.collect = TRUE}, \code{\link[=pf_filter]{pf_filter()}} collects the outputs in \code{R} as a \code{\linkS4class{pf_particles}} object (the \code{states} element is \code{NULL} is \code{.batch} is used). Otherwise, \code{invisible(NULL)} is returned.
}
\description{
This function runs the particle filter. The filter samples possible states (typically locations, termed particles) of an animal at each time point given the data up to (and including) that time point and a movement model.
}
\section{Overview}{
The particle filter iterates over time steps, simulating states (termed 'particles') that are consistent with the preceding data and a movement model at each time step.

A raster map of study area must be exported to \code{Julia} for this function (see \code{\link[=set_map]{set_map()}}).

The initial states for the algorithm are defined by \code{.xinit} or simulated via \href{https://github.com/edwardlavender/Patter.jl}{\code{Patter.simulate_states_init()}}. The word 'state' typically means location but may include additional parameters. If the initial state of the animal is known, it should be supplied via \code{.xinit}. Otherwise, \href{https://github.com/edwardlavender/Patter.jl}{\code{Patter.simulate_states_init()}} samples \code{.n_particle} initial coordinates from \code{.map} (via \href{https://github.com/edwardlavender/Patter.jl}{\code{Patter.coords_init()}}), which are then translated into a \code{DataFrame} of states (that is, \code{.xinit}, via \href{https://github.com/edwardlavender/Patter.jl}{\code{Patter.states_init()}}). The regions on the map from which initial coordinates are sampled can be restricted by the observations (\code{.yobs}) and other parameters. For automated handling of custom states and observation models at this stage, custom \code{Patter.map_init} and \code{Patter.states_init} methods are required (see the Details for \href{https://github.com/edwardlavender/Patter.jl}{\code{Patter.simulate_states_init()}}).

The filter comprises three stages:
\enumerate{
\item A movement step, in which we simulate possible states (particles) for the individual (for time steps 2, ..., T).
\item A weights step, in which we calculate particle weights from the log-likelihood of the data at each particle.
\item A re-sampling step, in which we optionally re-sample valid states using the weights.
}

The time complexity of the algorithm is \eqn{O(TN)}.

The filter is implemented by the \code{Julia} function \href{https://github.com/edwardlavender/Patter.jl}{\code{Patter.particle_filter()}}.

To multi-thread movement and likelihood evaluations, set the number of threads via \code{\link[=julia_connect]{julia_connect()}}.

The filter record \code{.n_record} particles in memory at each time step. If \code{batch} is provided, the \code{.timeline} is split into \code{length(.batch)} batches. The filter still moves along the whole \code{.timeline}, but only records the particles for the current batch in memory. At the end of each batch, the particles for that batch are written to file. This reduces total memory demand.

See \href{https://github.com/edwardlavender/Patter.jl}{\code{Patter.particle_filter()}} or \code{JuliaCall::julia_help("particle_filter")} for further information.
}

\section{Algorithms}{
This is highly flexible routine for the reconstruction of the possible locations of an individual through time, given the data up to that time point. By modifying the observation models, it is straightforward to implement the ACPF, DCPF and ACDCPF algorithms introduced by Lavender et al. (2023) for reconstructing animal movements in passive acoustic telemetry systems using (a) acoustic time series, (b) archival time series and (c) acoustic and archival time series. \code{\link[=pf_filter]{pf_filter()}} thus replaces (and enhances) the \href{https://edwardlavender.github.io/flapper/reference/ac.html}{\code{flapper::ac()}}, \href{https://edwardlavender.github.io/flapper/reference/dc.html}{\code{flapper::dc()}}, \href{https://edwardlavender.github.io/flapper/reference/acdc.html}{\code{flapper::acdc()}} and \href{https://edwardlavender.github.io/flapper/reference/pf.html}{\code{flapper::pf()}} functions.
}

\examples{
if (patter_run(.julia = TRUE, .geospatial = TRUE)) {

  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)
  library(glue)

  #### Julia set up
  julia_connect()
  set_seed()

  #### Define study period
  # The resolution of the timeline defines the resolution of the simulation
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  as.POSIXct("2016-01-01 03:18:00", tz = "UTC"),
                  by = "2 mins")

  #### Define study area
  # `map` is a SpatRaster that defines the region within which movements are permitted
  # Here, we consider the movements of an aquatic animal in Scotland
  # ... `map` represents the bathymetry in the relevant region
  # Use `set_map()` to export the map to `Julia`
  map <- dat_gebco()
  terra::plot(map)
  set_map(map)


  #### --------------------------------------------------
  #### Simulation-based workflow

  #### Scenario
  # We have studied the movements of flapper skate off the west coast of Scotland.
  # We have collected acoustic detections at receivers and depth time series.
  # Here, we simulate movements and acoustic/archival observations arising from movements.
  # We then apply particle filtering to the 'observations' to reconstruct
  # ... simulated movements.

  #### Simulate an acoustic array
  moorings <- sim_array(.map = map,
                        .timeline = timeline,
                        .n_receiver = 100L)

  # `moorings` includes the following default observation model parameters
  # * These describe how detection probability declines with distance from a receiver
  # * These are the parameters of the `ModelObsAcousticLogisTrunc` observation model
  a <- moorings$receiver_alpha[1]
  b <- moorings$receiver_beta[1]
  g <- moorings$receiver_gamma[1]
  d <- seq(1, 1000, by = 1)
  plot(d, ifelse(d <= g, plogis(a * b * d), 0),
       ylab = "Detection probability",
       xlab = "Distance (m)",
       type = "l")

  #### Simulate a movement path
  # Define `State` sub-type
  # > We will consider the animal's movement in two-dimensions (x, y)
  state    <- "StateXY"
  # Define the maximum moveable distance (m) between two time steps
  mobility <- 750
  # Define the movement model
  # > We consider a two-dimensional random walk
  model_move <-
    model_move_xy(.mobility   = "750.0",
            .dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
            .dbn_heading  = "Uniform(-pi, pi)")
  # Simulate a path
  paths <- sim_path_walk(.map = map,
                         .timeline = timeline,
                         .state = state,
                         .model_move = model_move)

  #### Simulate observations
  # Define observation model(s)
  # * We simulate acoustic observations and depth time series
  # * Acoustic observations are simulated according to `ModelObsAcousticLogisTrunc`
  # * Depth observations are simulated according to `ModelObsDepthUniformSeabed`
  # Define a `list` of parameters for the observation models
  # (See `?ModelObs` for details)
  pars_1 <-
    moorings |>
    select(sensor_id = "receiver_id", "receiver_x", "receiver_y",
           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
    as.data.table()
  pars_2 <- data.table(sensor_id = 1L,
                       depth_shallow_eps = 10,
                       depth_deep_eps = 10)
  model_obs <- list(ModelObsAcousticLogisTrunc = pars_1,
                    ModelObsDepthUniformSeabed = pars_2)
  # Simulate observational datasets
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = model_obs)
  summary(obs)
  head(obs$ModelObsAcousticLogisTrunc[[1]])
  head(obs$ModelObsDepthUniformSeabed[[1]])
  # Identify detections
  detections <-
    obs$ModelObsAcousticLogisTrunc[[1]] |>
    filter(obs == 1L) |>
    as.data.table()
  # Collate datasets for filter
  # > This requires a list of datasets, one for each data type
  # > We could also include acoustic containers here for efficiency (see below)
  yobs <- list(ModelObsAcousticLogisTrunc = obs$ModelObsAcousticLogisTrunc[[1]],
               ModelObsDepthUniformSeabed = obs$ModelObsDepthUniformSeabed[[1]])

  #### Example (1): Run the filter using the default options
  fwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .xinit = NULL,
                   .model_move = model_move,
                   .yobs = yobs,
                   .n_particle = 1e4L,
                   .direction = "forward")

  ## Output object structure:
  # The function returns a `pf_particles` list:
  class(fwd)
  summary(fwd)
  # `states` is a [`data.table::data.table`] of particles:
  fwd$states
  # `diagnostics` is a [`data.table::data.table`] of filter diagnostics
  fwd$diagnostics
  # fwd$callstats is a [`data.table::data.table`] of call statistics
  fwd$callstats

  ## Map output states:
  # Map particle coordinates
  plot_xyt(.map = map, .coord = fwd$states, .steps = 1L)
  # Map a utilisation distribution
  # * Use `.sigma = spatstat.explore::bw.diggle()` for CV bandwidth estimate
  map_dens(.map = map,
           .coord = fwd$states)

  ## Analyse filter diagnostics
  # `maxlp` is the maximum log-posterior at each time step
  # > exp(fwd$diagnostics$maxlp) = the highest likelihood score at each time step (0-1)
  # > This should not be too low!
  plot(fwd$diagnostics$timestamp, fwd$diagnostics$maxlp, type = "l")
  # `ess` is the effective sample size
  # > For a 2D distribution,  >= 500 particles at each time step should be sufficient
  # > But we often have a low ESS at the moment of a detection
  # > If this is too low, we can trying boosting `.n_particle`
  plot(fwd$diagnostics$timestamp, fwd$diagnostics$ess, type = "l")
  points(detections$timestamp, rep(0, nrow(detections)),
         pch = 21, col = "red", bg = "red", cex = 0.5)
  abline(h = 500, col = "royalblue", lty = 3)

  #### Example (2): Customise initial states for the filter
  # Option (A): Mark the known starting location on `.map`
  # > Initial states are automatically sampled from `.map`
  # > We have to reset the initial map in Julia
  # > Use set_map with .as_Raster = TRUE and .as_GeoArray = FALSE
  # > (After the example, we reset the map for later examples)
  origin       <- terra::setValues(map, NA)
  cell         <- terra::cellFromXY(map, cbind(paths$x[1], paths$y[1]))
  origin[cell] <- paths$map_value[1]
  set_map(.x = origin, .as_Raster = TRUE, .as_GeoArray = FALSE)
  fwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .xinit = NULL,
                   .yobs = yobs,
                   .model_move = model_move,
                   .n_particle = 1e4L)
  set_map(map, .as_Raster = TRUE, .as_GeoArray = FALSE)
  # Option (B): Specify `.xinit` manually
  # > `.xinit` is resampled, as required, to generate `.n_particle` initial states
  xinit <- data.table(map_value = paths$map_value[1], x = paths$x[1], y = paths$y[1])
  fwd   <- pf_filter(.timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_move = model_move,
                     .n_particle = 1e4L)

  #### Example (3): Customise selected settings
  # Boost the number of particles
  fwd   <- pf_filter(.timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_move = model_move,
                     .n_particle = 1.5e4L)
  # Change the threshold ESS for resampling
  fwd   <- pf_filter(.timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_move = model_move,
                     .n_particle = 1.5e4L,
                     .n_resample = 1000L)
  # Force resampling at selected time steps
  # * Other time steps are resampled if ESS < .n_resample
  fwd   <- pf_filter(.timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_move = model_move,
                     .t_resample = which(timeline \%in\% detections$timestamp),
                     .n_particle = 1.5e4L,
                     .n_resample = 1000L)
  # Change the number of particles retained in memory
  fwd   <- pf_filter(.timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_move = model_move,
                     .n_particle = 1.5e4L,
                     .n_record = 2000L)

  #### Example (2): Run the filter backwards
  # (A forward and backward run is required for the two-filter smoother)
  fwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .xinit = NULL,
                   .yobs = yobs,
                   .model_move = model_move,
                   .n_particle = 1e4L,
                   .direction = "backward")


  #### --------------------------------------------------
  #### Real-world examples

  #### Assemble datasets

  # Define datasets for a selected animal
  # > Here, we consider detections and depth time series from an example flapper skate
  individual_id <- NULL
  det <- dat_detections[individual_id == 25L, ][, individual_id := NULL]
  arc <- dat_archival[individual_id == 25L, ][, individual_id := NULL]

  # Define a timeline
  # * We can do this manually or use the observations to define a timeline:
  timeline <- assemble_timeline(list(det, arc), .step = "2 mins", .trim = TRUE)
  timeline <- timeline[1:1440]
  range(timeline)

  # Assemble a timeline of acoustic observations (0, 1) and model parameters
  # * The default acoustic observation model parameters are taken from `.moorings`
  acoustics <- assemble_acoustics(.timeline = timeline,
                                  .detections = det,
                                  .moorings = dat_moorings)

  # Assemble corresponding acoustic containers
  # * This is recommended with acoustic observations
  # * Note this dataset is direction specific (see below)
  containers <- assemble_acoustics_containers(.timeline = timeline,
                                              .acoustics = acoustics,
                                              .mobility = mobility)

  # Assemble a timeline of archival observations and model parameters
  # * Here, we include model parameters for `ModelObsDepthNormalTruncSeabed`
  archival <- assemble_archival(.timeline = timeline,
                                .archival =
                                  arc |>
                                  rename(obs = depth) |>
                                  mutate(depth_sigma = 50,
                                         depth_deep_eps = 20))

  # Define the .yobs list for each run of the particle filter
  yobs_fwd <- list(ModelObsAcousticLogisTrunc     = acoustics,
                   ModelObsContainer              = containers$forward,
                   ModelObsDepthNormalTruncSeabed = archival)
  yobs_bwd <- list(ModelObsAcousticLogisTrunc     = acoustics,
                   ModelObsContainer              = containers$backward,
                   ModelObsDepthNormalTruncSeabed = archival)

  #### Visualise realisations of the movement model (the prior)
  # We will use the same movement model as in previous examples
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = state,
                .model_move = model_move,
                .n_path = 10L, .one_page = TRUE)

  #### Example (1): Run filter forwards
  fwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .yobs = yobs_fwd,
                   .model_move = model_move)

  #### Example (2): Run the filter backwards
  bwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .yobs = yobs_bwd,
                   .model_move = model_move,
                   .direction = "backward")


  #### --------------------------------------------------
  #### Joint inference of states and static parameters

  if (patter_run_expensive()) {

    # This example shows how we can infer both states and static parameters:
    # * States are our latent locations;
    # * Static parameters are the parameters in the movement and observation models;

    # For illustration, we'll use simulated data:
    # * We consider a passive acoustic telemetry system;
    # * The movement model is a Gaussian random walk;
    # * The standard deviation of the Gaussian distribution is the 'diffusivity';
    # * The observation model is a Bernoulli model for acoustic detections/non-detections;

    # Using simulated data, we'll pretend we don't know the movement 'diffusivity':
    # * For simplicity, we'll assume we know the other parameters;
    # * We'll attempt to estimate the true diffusivity from multiple runs;

    # We show how to use the filter log-likelihood score to estimate parameters via:
    # * grid-search;
    # * maximum likelihood optimisation;
    # * Bayesian inference;

    # We recommend a simulation analysis like this before real-world analyses:
    # * Follow the example below;
    # * For your study system and species, simulate observations;
    # * Examine whether you have sufficient information to estimate parameters;

    # Beware that the log-likelihood value from the filter is 'noisy':
    # * This can create all kinds of issues with optimisation & sampling;
    # * You should check the sensitivity of the results with regard to the noise;
    # * I.e., Re-run the routines a few times to check result consistency;

    # Note also that parameter estimation can be computationally expensive:
    # * We should select an appropriate inference procedure depending on filter cost;
    # * Compromises may be required;
    # * We may assume some static parameters are known;
    # * We may estimate other parameters for one or two individuals with good data;
    # * We may use best-guess parameters + sensitivity analysis;
    # * This example just provides a minimal workflow;

    #### Define study system
    # Define study timeline
    timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                    as.POSIXct("2016-01-01 03:18:00", tz = "UTC"),
                    by = "2 mins")
    # Define study area
    map      <- dat_gebco()
    set_map(map)

    #### Simulate a movement path
    # Define movement model
    # * `theta` is the movement 'diffusivity'
    # * We will attempt to estimate this parameter
    state    <- "StateXY"
    theta    <- 250.0
    mobility <- 750
    model_move <-
      model_move_xy(.mobility    = mobility,
                    .dbn_length  = glue("truncated(Normal(0, {theta}),
                                         lower = 0.0, upper = {mobility})"),
                    .dbn_heading  = "Uniform(-pi, pi)")
    # Simulate a path
    path <- sim_path_walk(.map        = map,
                          .timeline   = timeline,
                          .state      = state,
                          .model_move = model_move)

    #### Simulate observations
    # Simulate array (using default parameters)
    # * Note we simulate a dense, regular array here
    # * With real-world array designs, there may be less information available
    moorings <- sim_array(.map         = map,
                          .timeline    = timeline,
                          .arrangement = "regular",
                          .n_receiver  = 100L)
    # Collate observation model parameters
    model_obs <- model_obs_acoustic_logis_trunc(moorings)
    # Simulate observations arising from path
    obs <- sim_observations(.timeline  = timeline,
                            .model_obs = model_obs)
    acoustics <- obs$ModelObsAcousticLogisTrunc[[1]]
    # (optional) Compute containers
    containers <- assemble_acoustics_containers(.timeline  = timeline,
                                                .acoustics = acoustics,
                                                .mobility  = mobility,
                                                .map       = map)
    # Collate observations for forward filter
    yobs_fwd <-
      list(ModelObsAcousticLogisTrunc = obs$ModelObsAcousticLogisTrunc[[1]],
           ModelObsContainer = containers$forward)

    #### Prepare to estimate the diffusivity using the observations

    # i) Visualise movement models with different standard deviations
    # > This gives us a feel for how we should constrain the estimation process
    # > (if we didn't know the true parameter values)
    pp  <- par(mfrow = c(3, 3))
    thetas <- seq(100, 500, by = 50)
    cl_lapply(thetas, function(theta) {
      plot(model_move_xy(.mobility   = mobility,
                         .dbn_length  = glue::glue("truncated(Normal(0, {theta}),
                                                  lower = 0.0, upper = {mobility})"),
                         .dbn_heading = "Uniform(-pi, pi)"),
           .panel_length = list(main = theta, font = 2),
           .panel_heading = NULL,
           .par = NULL)
    })
    par(pp)

    # ii) Define a function that computes the log-likelihood given input parameters
    # * `theta` denotes a parameter/parameter vector
    # * (In this case, it is standard deviation in the movement model)
    pf_filter_loglik <- function(theta) {

      # (safety check) The movement standard deviation cannot be negative
      if (theta < 0) {
        return(-Inf)
      }

      # Instantiate movement model
      model_move <-
        model_move_xy(.mobility   = mobility,
                      .dbn_length = glue::glue("truncated(Normal(0, {theta}),
                                                lower = 0.0, upper = {mobility})"),
                      .dbn_heading  = "Uniform(-pi, pi)")

      # Run filter
      # * Large .n_particle reduces noise in the likelihood (& filter speed)
      fwd <- pf_filter(.timeline   = timeline,
                       .state      = state,
                       .xinit      = NULL,
                       .model_move = model_move,
                       .yobs       = yobs_fwd,
                       .n_particle = 1e4L,
                       .direction  = "forward",
                       .progress   = julia_progress(enabled = TRUE),
                       .verbose    = FALSE)

      # Return log-lik
      # (-Inf is returned for convergence failures)
      fwd$callstats$loglik

    }

    #### (A) Use grid-search optimisation (~3 s)
    # This is a good option for an initial parameter search
    # (especially when there is only one parameter to optimise)
    # i) Run a coarse grid search
    theta_grid_loglik <- cl_lapply(thetas, pf_filter_loglik) |> unlist()
    # ii) Check the noise around the optimum (~20 s)
    noise <- lapply(1:5, function(i) {
      loglik <- cl_lapply(thetas, pf_filter_loglik) |> unlist()
      data.frame(iter = i, theta = thetas, loglik = loglik)
    }) |> rbindlist()
    # iii) Visualise log-likelihood profile
    ylim <- range(c(theta_grid_loglik, noise$loglik))
    plot(thetas, theta_grid_loglik, type = "b")
    points(noise$theta, noise$loglik)

    #### (B) Use optimisation routine e.g., optim or ADMB (~10 s)
    # This may be a good option with multiple parameters
    # We need to run the optimisation a few times to check result consistency
    # Here, we only need 1-dimensional optimisation
    # (So we use method = "Brent" & set the bounds on the optimisation)
    theta_optim <- optim(par = 200, fn = pf_filter_loglik,
                         method = "Brent", lower = 100, upper = 500,
                         control = list(fnscale = -1))
    theta_optim

    #### (C) Use MCMC e.g., via adaptMCMC (~30 s)
    # This is a good option for incorporating prior knowledge
    # & characterising the uncertainty in theta

    # i) Define log-posterior of parameters
    pf_filter_posterior <- function(theta) {
      # Log prior
      # (For multiple thetas, simply take the product)
      lp <- dunif(theta, 100, 500, log = TRUE)
      if (!is.finite(lp)) {
        return(-Inf)
      } else {
        # Posterior = prior * likelihood
        lp <- lp + pf_filter_loglik(theta)
      }
      lp
    }

    # ii) Select variance of jump distribution
    sd_of_jump <- 10
    curve(dnorm(x, 0, sd_of_jump), -20, 20)

    # iii) Run MCMC
    # * For a real analysis, many more samples are recommended
    theta_mcmc <- adaptMCMC::MCMC(pf_filter_posterior,
                                  n = 100L,
                                  init = 200, scale = sd_of_jump^2,
                                  adapt = TRUE, acc.rate = 0.4)

    # iv) Visualise samples (log-likelihoods, histogram, MCMC chain)
    # > Note the noise in the log-likelihoods due to the stochastic nature of the filter
    pp <- par(mfrow = c(1, 3))
    o <- order(theta_mcmc$samples)
    plot(theta_mcmc$samples[o], theta_mcmc$log.p[o], type = "b")
    hist(theta_mcmc$samples)
    plot(theta_mcmc$samples, type = "l")
    par(pp)

  }

}
}
\seealso{
Particle filters and smoothers sample states (particles) that represent the possible locations of an individual through time, accounting for all data and the individual's movement.
\itemize{
\item To simulate artificial datasets, see \verb{sim_*()} functions (especially \code{\link[=sim_path_walk]{sim_path_walk()}}, \code{\link[=sim_array]{sim_array()}} and \code{\link[=sim_observations]{sim_observations()}}).
\item To assemble real-world datasets for the filter, see \code{\link{assemble}}\verb{_*()} functions.
\item \code{\link[=pf_filter]{pf_filter()}} runs the filter:
\itemize{
\item For state types, see \code{\link{State}};
\item For observation models, see \code{\link{ModelObs}};
\item For movement models, see \code{\link{ModelMove}};
}
\item To run particle smoothing, use \code{\link[=pf_smoother_two_filter]{pf_smoother_two_filter()}}.
\item To map emergent patterns of space use, use a \verb{map_*()} function (such as \code{\link[=map_pou]{map_pou()}}, \code{\link[=map_dens]{map_dens()}} and \code{\link[=map_hr]{map_hr()}}).
}
}
\author{
Edward Lavender
}
