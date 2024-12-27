if (patter_run()) {

  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)

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
    move_xy(.mobility   = "750.0",
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
  pf_plot_xy(.map = map, .coord = fwd$states, .steps = 1L)
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
  yobs_fwd <- list(ModelObsAcousticLogisTrunc = acoustics,
                   ModelObsAcousticContainer = containers$forward,
                   ModelObsDepthNormalTruncSeabed = archival)
  yobs_bwd <- list(ModelObsAcousticLogisTrunc = acoustics,
                   ModelObsAcousticContainer = containers$backward,
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

}
