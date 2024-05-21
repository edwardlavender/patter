# Patter contains multiple built-in `ModelObs` subtypes that you can use
# ... (with custom parameters) simulate observations and for particle filtering.
# To use a new subtype, follow the workflow below. Some extra work is required
# ... because we have to register the subtype in `Julia` and write the
# ... required methods to simulate observations and/or calculate log probabilities.

if (julia_run()) {

  library(JuliaCall)
  library(data.table)

  #### Julia set up
  # Connect to Julia
  julia <- julia_connect()
  # Set seed
  set_seed()
  # Export map to Julia
  map <- dat_gebco()
  set_map(map)

  #### Simulate path(s)
  # > We simulate a path in four dimensions (see `?StateXYZD`)
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  length.out = 1000L, by = "2 mins")
  paths <- sim_path_walk(.map = map,
                         .timeline = timeline,
                         .state = "StateXYZD",
                         .xinit = NULL, .n_path = 1L,
                         .move = move_xyzd(),
                         .plot = TRUE,
                         .one_page = TRUE)

  #### Simulate observations arising from the simulated path
  # Register a custom `ModelObs` subtype in Julia
  # * We imagine a pelagic animal in which the depth at each time step
  # * ... is normally distributed around the previous depth.
  # * We write a `ModelObs` subtype in `Julia` that contains the parameters
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
  function Patter.simulate_obs(state::StateXYZD, model::ModelObsDepthNormal, t::Int64)
    dbn   = truncated(Normal(state.z, model.depth_sigma), 0, state.map_value)
    rand(dbn)
  end
  '
  )
  # Simulate observations
  obs <- sim_observations(.timeline = timeline,
                          .models = "ModelObsDepthNormal",
                          .parameters = list(data.table(sensor_id = 1L, depth_sigma = 5)))
  obs <- obs$ModelObsDepthNormal[[1]]
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
  origin <- terra::setValues(map, NA)
  origin[paths$cell_id[1]] <- terra::extract(map, paths$cell_id[1])
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
  fwd <- pf_filter(.map = origin,
                   .timeline = timeline,
                   .state = "StateXYZD",
                   .yobs = list(obs),
                   .model_obs = "ModelObsDepthNormal",
                   .model_move = move_xyzd(),
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
