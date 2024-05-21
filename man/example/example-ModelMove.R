if (julia_run()) {

  library(data.table)
  library(JuliaCall)
  library(truncdist)

  #### Connect to Julia
  julia_connect()
  set_seed()

  #### Define the `map`
  # `map` is the region within which movements are permitted
  # In `R`, we represent this as a `SpatRaster`
  # Here, we have a bathymetry `SpatRaster` for the west coast of Scotland:
  # * NaNs define regions into which movement is not permitted
  # * (i.e., on land, in the case of aquatic animals)
  map <- dat_gebco()
  terra::plot(map)
  # Using `set_map()` makes the map available as a object called 'env' in `Julia`
  # > This is required as a component of all movement models
  set_map(map)

  #### Example (1): Use `move_xy()` with default options
  # `move_*()` functions simply return a character string of Julia code
  # (Downstream functions can evaluate this code, as shown below)
  move_xy()

  #### Example (2): Customise `move_xy()`
  # Use a truncated normal distribution for step lengths:
  hist(rtrunc(1e5L, "norm", a = 0, b = 750, mean = 250, sd = 50))
  move_xy(dbn_length = "truncated(Normal(250, 50), lower = 0.0, upper = 750.0)")
  # Use an exponential distribution for step lengths
  hist(rtrunc(1e5L, "exp", b = 750, rate = 0.001))
  move_xy(dbn_length = "truncated(Exponential(0.01), upper = 750.0)")
  # Use a biased random walk
  move_xy(dbn_angle = "VonMises(0, 1)")
  # Get help on a distribution in Julia:
  julia_help("Exponential")

  #### Example (3): Customise `move_xyzd()`
  # Use default options
  move_xyzd()
  # Customise model components
  move_xyzd(dbn_length = "truncated(Normal(250, 50), lower = 0.0, upper = 750.0)",
            dbn_angle_delta = "Normal(0, 0.25)",
            dbn_z_delta = "Normal(0, 2.5)")

  #### Example (4): Visualise different movement models
  # Define a timeline for the simulation
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  length.out = 1000L, by = "2 mins")
  # Define an initial location
  x <- 708212.6
  y <- 6251684
  origin <- data.table(map_value = terra::extract(map, cbind(x, y))[1, 1],
                       x = x, y = y)
  # Collect essential arguments for `sim_path_walk()`
  args <- list(.map = map,
               .xinit = origin,
               .timeline = timeline,
               .state = "StateXY",
               .n_path = 2L, .one_page = FALSE)
  # Compare different movement models via `sim_path_walk()`
  pp <- par(mfrow = c(2, 2))
  args$.move <- move_xy()
  do.call(sim_path_walk, args)
  args$.move <- move_xy(dbn_angle = "VonMises(0.1, 0.1)")
  do.call(sim_path_walk, args)
  par(pp)

  #### Example (5): Use movement models in the particle filter
  # See `?pf_filter()`

  #### Example (6): Use custom movement model types
  # Patter contains multiple built-in `State` and `ModelMove` subtypes that you can use
  # ... (with custom parameters) simulate movements and for particle filtering.
  # See the help file for `?State` to use a new subtype.

}
