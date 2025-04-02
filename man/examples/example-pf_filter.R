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
  # `states` is a `data.table` of particles:
  fwd$states
  # `diagnostics` is a `data.table` of filter diagnostics
  fwd$diagnostics
  # fwd$callstats is a `data.table` of call statistics
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
                     .t_resample = which(timeline %in% detections$timestamp),
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
    pp <- par(mfrow = c(1, 3))
    o <- order(theta_mcmc$samples)
    plot(theta_mcmc$samples[o], theta_mcmc$log.p[o], type = "b")
    hist(theta_mcmc$samples)
    plot(theta_mcmc$samples, type = "l")
    par(pp)

  }

}
