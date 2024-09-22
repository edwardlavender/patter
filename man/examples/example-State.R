# Patter contains multiple built-in `State` and `ModelMove` sub-types that you can use
# ... (with custom parameters) simulate movements and for particle filtering.
# To use a new sub-type, follow the workflow below. Some extra work is required
# ... because we have to register the sub-type in `Julia` and write the
# ... required methods to simulate initial states, movement and/or
# ... evaluate movement densities.

if (julia_run()) {

  library(data.table)
  library(JuliaCall)
  library(testthat)

  #### Julia set up
  # Connect to Julia
  julia <- julia_connect()
  # Set seed
  set_seed()
  # Export map to Julia
  map <- dat_gebco()
  set_map(map)

  #### Define a `State` and a corresponding `ModelMove` instance
  # Define a custom `State` sub-type
  # > We consider the location of the animal in three-dimensions
  julia_command(
    '
  struct StateXYZ <: Patter.State
    # Map value
    # > This is required for all states & is the value on the map at (x, y)
    map_value::Float64
    # Coordinates
    x::Float64
    y::Float64
    z::Float64
  end
  '
  )
  # Define a corresponding movement model sub-type
  # > We will consider a random walk in 3D
  julia_command(
    '
  struct ModelMoveXYZ{T, U, V, W, X} <: Patter.ModelMove
    # The environment (i.e., map)
    # > This defines the regions within which movements are permitted (i.e., in water)
    map::T
    # Distribution for step lengths
    mobility::U
    dbn_length::V
    # Distribution for turning angles
    dbn_angle::W
    # Distribution for changes in depth
    dbn_z_delta::X
  end
  '
  )
  # Instantiate the movement model
  # > We will write an R function to instantiate the movement model
  move_xyz <- function(mobility = "750.0",
                       length = "truncated(Gamma(1.0, 750.0), upper = 750.0)",
                       angle = "Uniform(-pi, pi)",
                       z_delta = "Normal(0, 3.5)") {
    # (optional) Verify the map (`env` in Julia) exists:
    # patter:::julia_check_exists("env")
    # Define the movement model `Julia` code as a String
    glue::glue('ModelMoveXYZ(env, {mobility}, {length}, {angle}, {z_delta});')
  }
  # (optional) Define an `R` `states_init()` method to simulate initial states
  # * This function should accept:
  # * `.state`: the state;
  # * `.coords`: A `data.table` with initial coordinates (x, y and map_value);
  # * (see `?states_init` for further information)
  states_init.StateXYZ <- function(.state, .coords) {
    # Define global variables
    z <- map_value <- NULL
    # Add initial z values
    .coords[, z := map_value * runif(.N)]
    .coords
  }
  # Define a `Patter.simulate_step()` method to update the state in Julia
  julia_command(
    '
  function Patter.simulate_step(state::StateXYZ, model::ModelMoveXYZ, t::Int64)
    # Simulate a step length
    length = rand(model.dbn_length)
    # Simulate a turning angle
    angle  = rand(model.dbn_angle)
    # Calculate new x, y, z coordinates
    x      = state.x + length * cos(angle)
    y      = state.y + length * sin(angle)
    z      = state.z + rand(model.dbn_z_delta)
    # Include the map value
    map_value = extract(model.map, x, y)
    # Define the state
    StateXYZ(map_value, x, y, z)
  end
  '
  )

  #### Simulate path(s)
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  length.out = 1000L, by = "2 mins")
  paths <- sim_path_walk(.map = map,
                         .timeline = timeline,
                         .state = "StateXYZ",
                         .model_move = move_xyz(),
                         .plot = TRUE)

  #### Simulate observations arising from the simulated path
  # We consider a pelagic animal & simulate depth observations
  # At each time step, the animal may be anywhere from the surface to the seabed
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = "ModelObsDepthUniform",
                          .model_obs_pars = list(data.table(sensor_id = 1L,
                                                            depth_shallow_eps = 500,
                                                            depth_deep_eps = 0)))
  obs <- obs$ModelObsDepthUniform[[1]]
  # Plot simulated depth trajectory
  # * Blue: simulated time series
  # * Grey: seabed depth for simulated time series
  ylim <- range(c(obs$obs, paths$map_value) * -1)
  plot(obs$timestamp, obs$obs * -1, ylim = ylim, col = "royalblue", type = "l")
  lines(paths$timestamp, paths$map_value * -1, col = "grey")
  # > The individual ranges from the seabed to the surface
  expect_true(max(obs$obs - paths$map_value) < 0)

  #### Run the forward filter
  # (optional) Define initial states, by:
  # A) Starting the filter in the correct location by masking `.map`
  # B) Specifying a complete data.table of initial state(s)
  origin       <- terra::setValues(map, NA)
  cell         <- terra::cellFromXY(map, cbind(paths$x[1], paths$y[1]))
  origin[cell] <- paths$map_value[1]
  # Run the filter
  fwd <- pf_filter(.map = origin,
                   .timeline = timeline,
                   .state = "StateXYZ",
                   .yobs = list(obs),
                   .model_obs = "ModelObsDepthUniform",
                   .model_move = move_xyz())
  # Visualise reconstructed time series
  # * Black: particle depths
  # * Blue: simulated time series
  ylim <- range(c(fwd$states$z, obs$obs, paths$map_value) * -1)
  plot(fwd$states$timestamp, fwd$states$z * -1, ylim = ylim, pch = ".")
  lines(obs$timestamp, obs$obs * -1 , col = "royalblue")

  #### Run the backward filter
  # Define origin
  origin <- terra::setValues(map, NA)
  n            <- nrow(paths)
  cell         <- terra::cellFromXY(map, cbind(paths$x[n], paths$y[n]))
  origin[cell] <- paths$map_value[n]
  # Run the filter
  bwd <- pf_filter(.map = origin,
                   .timeline = timeline,
                   .state = "StateXYZ",
                   .yobs = list(obs),
                   .model_obs = "ModelObsDepthUniform",
                   .model_move = move_xyz(),
                   .direction = "backward")

  #### Run the smoother
  # Write a `Patter.logpdf_step()` method
  # ... to evaluate the unnormalised log probability (density)
  # ... of an unrestricted step from one state to another
  # See: julia_help("Patter.logpdf_step")
  julia_command(
    '
    function Patter.logpdf_step(state_from::StateXYZ, state_to::StateXYZ,
                                model_move::ModelMoveXYZ,
                                t::Int64,
                                length::Float64, angle::Float64)
        # Calculate change in depth
        z_delta = state_to.z - state_from.z
        # Evaluate unnormalised log probability
        logpdf(model_move.dbn_length, length) +
          logpdf(model_move.dbn_angle, angle) +
          logpdf(model_move.dbn_z_delta, z_delta)
    end
    '
  )
  # Run the smoother
  smo <- pf_smoother_two_filter()

  #### (optional) Map UD
  # map_dens(map, .coord = smo$states)

}
