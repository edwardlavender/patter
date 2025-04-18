if (patter_run(.julia = TRUE, .geospatial = TRUE)) {

  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)

  #### Connect to Julia
  julia_connect()
  set_seed()

  #### Set up study system
  # Define `map` (the region within which movements are permitted)
  map <- dat_gebco()
  set_map(map)
  # Define study period
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  length.out = 1000L, by = "2 mins")

  #### Example (1): Simulate path with default options
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .model_move = model_move_xy())

  #### Example (2): Set the starting location via `.xinit`
  # Define an initial location
  x <- 708212.6
  y <- 6251684
  origin <- data.table(map_value = terra::extract(map, cbind(x, y))[1, 1],
                       x = x, y = y)
  # Run the simulation
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .xinit = origin,
                .model_move = model_move_xy())
  points(origin$x, origin$y)

  #### Example (3): Simulate multiple paths with the same origin via `.xinit`
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .xinit = origin,
                .model_move = model_move_xy(),
                .n_path = 4L,
                .one_page = TRUE)

  #### Example (4): Simulate multiple paths with different origins via `.xinit`
  # Manually specify origins
  origins <-
    map |>
    terra::spatSample(size = 4, xy = TRUE, na.rm = TRUE) |>
    select("map_value", "x", "y") |>
    as.data.table()
  # Run simulation
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .xinit = origins,
                .model_move = model_move_xy(),
                .n_path = 4L,
                .one_page = TRUE)

  #### Example (5): Customise two-dimensional random walks via `model_move_xy()`
  # Adjust distributions for step lengths and headings
  model_move <-
    model_move_xy(.mobility = "750.0",
            .dbn_length = "truncated(Normal(250, 50), lower = 0.0, upper = 750.0)",
            .dbn_heading = "VonMises(0.1, 0.1)")
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .model_move = model_move)
  # Experiment with other options
  model_move <-
    model_move_xy(.mobility   = "300.0",
            .dbn_length = "truncated(Normal(10.0, 50.0), lower = 0.0, upper = 300.0)")
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .model_move = model_move)

  #### Example (6): Use other .state/.model_move combinations
  # Simulate a random walk in XYZ
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXYZ",
                .model_move = model_move_xyz())
  # Simulate a correlated random walk in XY
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateCXY",
                .model_move = model_move_cxy())
  # Simulate a correlated random walk in XYZ
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateCXYZ",
                .model_move = model_move_cxyz())
  # Modify movement model parameters
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateCXYZ",
                .model_move = model_move_cxyz(.dbn_heading_delta = "Normal(0, 1)",
                                        .dbn_z_delta = "Normal(0, 0.5)"))

  #### Example (7): Use custom .state/.model_move sub-types
  # See `?State` and ?ModelMove`

  #### Example (8): Simulate numerous paths via `.n_path`
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .model_move = model_move_xy(),
                .n_path = 10L)

  #### Example (9): Customise plotting options via `.plot` & `.one_page`
  # Use one page via `.one_page = TRUE`
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .model_move = model_move_xy(),
                .n_path = 2L, .one_page = TRUE)
  # Suppress plots via `.plot = FALSE`
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .model_move = model_move_xy(),
                .plot = FALSE)

}
