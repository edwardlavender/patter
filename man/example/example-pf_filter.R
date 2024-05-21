if (julia_run()) {

  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)

  #### Julia set up
  julia_connect()
  set_seed()

  #### Define study period
  # The resolution of the timeline defines the resolution of the simulation
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  length.out = 100L, by = "2 mins")

  #### Define study area
  # `map` is a SpatRaster that defines the region within which movements are permitted
  # Here, `map` represents the bathymetry of the west coast of Scotland
  # Use `set_map()` to export the map to `Julia`
  map <- dat_gebco()
  terra::plot(map)
  set_map(map)


  #### --------------------------------------------------
  #### Simulation-based workflow

  #### Simulate an acoustic array
  moorings <- sim_array(.map = dat_gebco(),
                        .timeline = timeline,
                        .n_receiver = 100L)

  # `moorings` includes the following default detection probability parameters:
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
  state    <- "StateXY"
  # Define the maximum moveable distance between two time steps
  mobility <- 750
  # Define the movement model
  move     <-
    move_xy(dbn_length = glue::glue("truncated(Gamma(1, 250.0), upper = {mobility})"),
            dbn_angle = "Uniform(-pi, pi)")
  # Simulate a path
  paths <- sim_path_walk(.map = dat_gebco(),
                         .timeline = timeline,
                         .state = state,
                         .model_move = move)

  #### Simulate observations
  # Define observation model(s)
  models <- c("ModelObsAcousticLogisTrunc", "ModelObsDepthUniform")
  # Define a `list` of parameters
  pars_1 <-
    moorings |>
    select(sensor_id = "receiver_id", "receiver_x", "receiver_y",
           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
    as.data.table()
  pars_2 <- data.table(sensor_id = 1L,
                       depth_shallow_eps = 10,
                       depth_deep_eps = 10)
  pars <- list(pars_1, pars_2)
  # Simulate observational datasets
  obs <- sim_observations(.timeline = timeline,
                          .models = models,
                          .parameters = pars)
  summary(obs)
  head(obs$ModelObsAcousticLogisTrunc[[1]])
  head(obs$ModelObsDepthUniform[[1]])
  # Identify detections
  detections <-
    obs$ModelObsAcousticLogisTrunc[[1]] |>
    filter(obs == 1) |>
    as.data.table()
  # Collate datasets for filter
  yobs <- list(obs$ModelObsAcousticLogisTrunc[[1]], obs$ModelObsDepthUniform[[1]])

  #### Example (1): Run the filter using the default options
  fwd <- pf_filter(.map = map,
                   .timeline = timeline,
                   .state = state,
                   .xinit = NULL, .xinit_pars = list(mobility = mobility),
                   .yobs = yobs,
                   .model_obs = models,
                   .model_move = move,
                   .n_particle = 1e4L)

  ## Output object structure:
  # The function returns a `pf_particles` list:
  class(fwd)
  summary(fwd)
  # `xinit` is a `data.table` of initial particles
  fwd$xinit
  # `states` is a `data.table` of particles:
  fwd$states
  # `diagnostics` is a `data.table` of filter diagnostics
  fwd$diagnostics
  # fwd$convergence is a Boolean that defines whether or not the algorithm converged
  fwd$convergence

  ## Map output states:
  # TO DO

  ## Analyse filter diagnostics
  # `maxlp` is the maximum log-posterior at each time step
  # > exp(fwd$diagnostics$maxlp) = the highest likelihood score at each time step
  # > This should not be too low!
  plot(fwd$diagnostics$timestamp, fwd$diagnostics$maxlp, type = "l")
  # `ess` is the effective sample size
  # > For a 2D distribution,  >= 500 particles at each time step should be sufficient
  # > But we often have a low ESS at the moment of a detection
  # > If this is too low, we can trying boosting `.n_particle`
  plot(fwd$diagnostics$timestamp, fwd$diagnostics$ess, type = "l")
  points(detections$timestamp, rep(0, nrow(detections)),
         pch = 21, col = "red", bg = "red", cex = 0.5)
  abline(h = 500, col = "blue", lty = 3)

  #### Example (2): Customise initial states for the filter
  # Mark the known starting location on `.map`
  # > Initial states are automatically sampled from `.map`
  origin <- terra::setValues(map, NA)
  origin[paths$cell_id] <- paths$map_value[1]
  fwd <- pf_filter(.map = origin,
                   .timeline = timeline,
                   .state = state,
                   .xinit = NULL, .xinit_pars = list(mobility = mobility),
                   .yobs = yobs,
                   .model_obs = models,
                   .model_move = move,
                   .n_particle = 1e4L)
  # Specify `.xinit` manually
  # > `.xinit` is resampled, as required, to generate `.n_particle` initial states
  xinit <- data.table(map_value = paths$map_value[1], x = paths$x[1], y = paths$y[1])
  fwd   <- pf_filter(.map = map,
                     .timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_obs = models,
                     .model_move = move,
                     .n_particle = 1e4L)

  #### Example (3): Customise selected settings
  # Boost the number of particles
  fwd   <- pf_filter(.map = map,
                     .timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_obs = models,
                     .model_move = move,
                     .n_particle = 1e5L)
  # Change the threshold ESS for resampling
  fwd   <- pf_filter(.map = map,
                     .timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_obs = models,
                     .model_move = move,
                     .n_particle = 1e5L,
                     .n_resample = 1000L)
  # Change the number of particles retained in memory
  fwd   <- pf_filter(.map = map,
                     .timeline = timeline,
                     .state = state,
                     .xinit = xinit,
                     .yobs = yobs,
                     .model_obs = models,
                     .model_move = move,
                     .n_particle = 1e5L,
                     .n_record = 2000L)

  #### Example (2): Run the filter backwards
  # (A forward and backward run is required for the two-filter smoother)
  fwd <- pf_filter(.map = map,
                   .timeline = timeline,
                   .state = state,
                   .xinit = NULL, .xinit_pars = list(mobility = mobility),
                   .yobs = yobs,
                   .model_obs = models,
                   .model_move = move,
                   .n_particle = 1e4L,
                   .direction = "backward")


  #### --------------------------------------------------
  #### Real-world examples

  #### Assemble datasets

  # Define datasets for a selected individual
  # > Here, we consider acoustic detections and depth time series
  individual_id <- NULL
  acc <- dat_acoustics[individual_id == 25L, ][, individual_id := NULL]
  arc <- dat_archival[individual_id == 25L, ][, individual_id := NULL]

  # Define a timeline
  # * We can do this manually or use the observations to define a timeline:
  timeline <- assemble_timeline(list(acc, arc), .step = "2 mins", .trim = TRUE)
  timeline <- timeline[1:1440]
  range(timeline)

  # Assemble a timeline of acoustic observations (0, 1) and model parameters
  # * The default acoustic observation model parameters are taken from `.moorings`
  model_1   <- "ModelObsAcousticLogisTrunc"
  acoustics <- assemble_acoustics(.timeline = timeline,
                                  .acoustics = acc,
                                  .moorings = dat_moorings)

  # Assemble a timeline of archival observations and model parameters
  # * Here, we include model parameters for `ModelObsDepthNormalTrunc`
  model_2  <- "ModelObsDepthNormalTrunc"
  archival <- assemble_archival(.timeline = timeline,
                                .archival =
                                  arc |>
                                  rename(obs = depth) |>
                                  mutate(depth_sigma = 50,
                                         depth_deep_eps = 20))

  #### Visualise realisations of the movement model (the prior)
  # We will use the same movement model as in previous examples
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = state,
                .model_move = move,
                .n_path = 10L, .one_page = TRUE)

  #### Example (1): Run filter forwards
  fwd <- pf_filter(.map = map,
                   .timeline = timeline,
                   .state = state,
                   .xinit = NULL, .xinit_pars = list(mobility = mobility),
                   .yobs = list(acoustics, archival),
                   .model_obs = c(model_1, model_2),
                   .model_move = move)

  #### Example (2): Run the filter backwards
  bwd <- pf_filter(.map = map,
                   .timeline = timeline,
                   .state = state,
                   .xinit = NULL, .xinit_pars = list(mobility = mobility),
                   .yobs = list(acoustics, archival),
                   .model_obs = c(model_1, model_2),
                   .model_move = move,
                   .direction = "backward")

}
