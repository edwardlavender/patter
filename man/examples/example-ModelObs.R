if (patter_run()) {

  library(JuliaCall)
  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)

  #### Julia set up
  # Connect to Julia
  julia <- julia_connect()
  # Set seed
  set_seed()

  #### Set map in Julia
  map <- dat_gebco()
  set_map(map)

  #### Simulate path(s)
  # > We simulate a path in four dimensions (see `?StateCXYZ`)
  timeline <- seq(as.POSIXct("2016-01-01 00:00:00", tz = "UTC"),
                  as.POSIXct("2016-01-02 09:18:00", tz = "UTC"),
                  by = "2 mins")
  paths <- sim_path_walk(.map        = map,
                         .timeline   = timeline,
                         .state      = "StateCXYZ",
                         .xinit      = NULL,
                         .n_path     = 1L,
                         .model_move = model_move_cxyz())


  #### --------------------------------------------------
  #### In-built ModelObs types

  #### Example (1): ModelObsAcousticLogisTrunc

  # ModelObsAcousticLogisTrunc is an in-built ModelObs structure
  # This holds the parameters of a
  # ... truncated logistic acoustic observation model
  # To evaluate the probability of an acoustic observation,
  # ... under this model, we need to know receiver positions
  # ... and the coefficients in the logistic equation.
  # We can then simulate observations or run the particle filter.

  # Simulate an acoustic array
  array <- sim_array(.map = map,
                     .timeline = timeline,
                     .n_receiver = 100L,
                     .arrangement = "regular",
                     # Define logistic equation coefficients
                     .receiver_alpha = 4,
                     .receiver_beta = -0.01,
                     .receiver_gamma = 750)

  # This is a data.table of our acoustic observation model parameters
  array

  # The model_obs_*() function simply returns these in a list
  # ... with a `ModelObsAcousticLogisTrunc` S3-class label
  model_obs_acoustic_logis_trunc(array)

  # We can plot observation model structures via corresponding `plot` methods
  # See `?plot.ModelObs`
  model_obs <- model_obs_acoustic_logis_trunc(array)
  plot(model_obs)

  # Simulate observations with a named list of observation model parameters
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = model_obs)

  # In long-form, the code above can be expressed as follows
  sim_observations(
    .timeline = timeline,
    .model_obs =
      list(
        ModelObsAcousticLogisTrunc =
          array |>
          select(sensor_id = "receiver_id",
                 "receiver_x", "receiver_y",
                 "receiver_alpha", "receiver_beta", "receiver_gamma") |>
          as.data.table()))

  # In real-world scenarios, we must assemble a `data.table` of acoustic observations
  # (For convenience, here we simply use our simulated observations)
  acc <- assemble_acoustics(
    .timeline   = timeline,
    .detections = obs$ModelObsAcousticLogisTrunc[[1]][obs == 1L, ],
    .moorings   = array
    )

  # The particle filter expects observations & parameters to be supplied as a named list
  fwd <- pf_filter(.timeline   = timeline,
                   .state      = "StateCXYZ",
                   .model_move = model_move_cxyz(),
                   .yobs       = model_obs_acoustic_logis_trunc(acc, .strict = FALSE),
                   .n_particle = 1000L)

  # This implementation is identical
  fwd <- pf_filter(.timeline   = timeline,
                   .state      = "StateCXYZ",
                   .model_move = model_move_cxyz(),
                   .yobs       = list(ModelObsAcousticLogisTrunc = acc),
                   .n_particle = 1000L)

  # But with the model_obs_*() implementation we benefit from in-built `plot()` methods
  plot(model_obs_acoustic_logis_trunc(acc))

  # See `?ModelObs` for other in-built `ModelObs` structures


  #### --------------------------------------------------
  #### Custom ModelObs types

  # Patter contains multiple built-in `ModelObs` sub-types that you can use
  # ... (with custom parameters) simulate observations and for particle filtering.
  # To use a new sub-type, follow the workflow below. Some extra work is required
  # ... because we have to register the sub-type in `Julia` and write the
  # ... required methods to simulate observations and/or calculate log probabilities.


  #### Simulate observations arising from the simulated path
  # Register a custom `ModelObs` sub-type in Julia
  # * We imagine a pelagic animal in which the depth at each time step
  # * ... is normally distributed around the previous depth.
  # * We write a `ModelObs` sub-type in `Julia` that contains the parameters
  # * ... for this model (i.e., the sigma parameter of the normal distribution).
  julia_command(
    '
    struct ModelObsDepthNormal <: Patter.ModelObs
      sensor_id::Int64
      depth_sigma::Float64
    end
  '
  )
  # Define a `Patter.simulate_obs()` method
  # * We need to specify a function that simulates depths for `ModelObsDepthNormal`
  # * We simulate depths around the previous depth (`state.z`), truncated between
  # * ... the depth of the seabed (`state.map_value`) and the surface.
  julia_command(
    '
  function Patter.simulate_obs(state::StateCXYZ, model::ModelObsDepthNormal, t::Int64)
    dbn   = truncated(Normal(state.z, model.depth_sigma), 0, state.map_value)
    rand(dbn)
  end
  '
  )
  # Simulate observations
  model_obs <-
    list("ModelObsDepthNormal" = data.table(sensor_id = 1L, depth_sigma = 5))
  obs  <- sim_observations(.timeline = timeline, .model_obs = model_obs)
  obs  <- obs$ModelObsDepthNormal[[1]]
  yobs <- list(ModelObsDepthNormal = obs)
  # Plot simulated depth trajectory
  # * Blue: simulated time series
  # * Grey: seabed depth for simulated time series
  ylim <- range(c(obs$obs, paths$map_value) * -1)
  plot(obs$timestamp, obs$obs * -1, ylim = ylim, col = "royalblue", type = "l")
  lines(paths$timestamp, paths$map_value * -1, col = "grey")

  #### Run the forward filter
  # (optional) Define initial states, by:
  # A) Starting the filter in the correct location by masking `.map`
  # B) Specifying a `map_init()` method based on the observation model
  # C) Specifying a complete data.table of initial state(s)
  origin       <- terra::setValues(map, NA)
  cell         <- terra::cellFromXY(map, cbind(paths$x[1], paths$y[1]))
  origin[cell] <- paths$map_value[1]
  set_map(origin, .as_Raster = TRUE, .as_GeoArray = FALSE)
  # Define a `Patter.logpdf_obs()` method
  # * This is used to evaluate the log probability of a depth observation
  julia_command(
    '
  function Patter.logpdf_obs(state::State, model::ModelObsDepthNormal, t::Int64, obs::Float64)
    dbn   = truncated(Normal(state.map_value, model.depth_sigma),
                      0.0, state.map_value)
    logpdf(dbn, obs)
  end
  '
  )
  # Run the filter
  fwd <- pf_filter(.timeline = timeline,
                   .state = "StateCXYZ",
                   .yobs = yobs,
                   .model_move = model_move_cxyz(),
                   .n_particle = 1000L)
  # Visualise reconstructed time series
  # * Black: particle depths
  # * Blue: simulated time series
  # * Grey: seabed depth for simulated time series
  ylim <- range(c(fwd$states$z, obs$obs, paths$map_value) * -1)
  plot(fwd$states$timestamp, fwd$states$z * -1, ylim = ylim, pch = ".")
  lines(obs$timestamp, obs$obs * -1 , col = "royalblue")
  lines(paths$timestamp, paths$map_value * -1, col = "grey")

}
