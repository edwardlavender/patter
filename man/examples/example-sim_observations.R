if (patter_run()) {

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

  #### Simulate path with default options
  paths <- sim_path_walk(.map = map,
                         .timeline = timeline,
                         .state = "StateXY",
                         .model_move = move_xy())

  #### Example (1): Simulate observations via `ModelObsAcousticLogisTrunc`

  # Overview:
  # * `ModelObsAcousticLogisTrunc`: observation model structure for acoustic observations
  # * See ?ModelObsAcousticLogisTrunc
  # * See JuliaCall::julia_help("ModelObs")
  # * This structure holds:
  #   - sensor_id (the receiver_id)
  #   - receiver_x, receiver_y (the receiver coordinates)
  #   - receiver_alpha, receiver_beta, receiver_gamma
  #   - (these are parameters of a truncated logistic detection probability model)
  # * Using these fields, it is possible to simulate detections at receivers

  # Simulate an acoustic array
  a <- 4
  b <- -0.01
  g <- 750
  moorings <- sim_array(.map = map,
                        .timeline = timeline,
                        .n_receiver = 100L,
                        # (optional) Define constant detection probability parameters
                        .receiver_alpha = a,
                        .receiver_beta = b,
                        .receiver_gamma = g)

  # This is the shape of detection probability model for the parameters we have chosen
  d <- seq(1, 1000, by = 1)
  plot(d, ifelse(d <= g, plogis(a * b * d), 0),
       ylab = "Detection probability",
       xlab = "Distance (m)",
       type = "l")

  # Define a data.table of observation model parameters
  moorings <-
    moorings |>
    select(sensor_id = "receiver_id",
           "receiver_x", "receiver_y",
           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
    as.data.table()

  # Simulate observations
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = list(ModelObsAcousticLogisTrunc = moorings))

  # Examine simulated observations
  # * sim_observations() returns a list, with one element for every `.model_obs`
  # * Each element is a `list`, with one element for each simulated path
  # * Each element is a `data.table` that contains the observations
  str(obs)

  # Plot detections
  detections <-
    obs$ModelObsAcousticLogisTrunc[[1]] |>
    lazy_dt() |>
    filter(obs == 1L) |>
    as.data.table()
  plot(detections$timestamp, detections$obs)

  # Customise `ModelObsAcousticLogisTrunc` parameters
  # > Receiver-specific parameters are permitted
  moorings[, receiver_alpha := runif(.N, 4, 5)]
  moorings[, receiver_beta := runif(.N, -0.01, -0.001)]
  moorings[, receiver_gamma := runif(.N, 500, 1000)]
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = list(ModelObsAcousticLogisTrunc = moorings))

  #### Example (2): Simulate observations via `ModelObsDepthUniformSeabed`
  # `ModelObsDepthUniformSeabed` is an observation model for depth observations
  # * See ?ModelObsAcousticLogisTrunc
  # * See JuliaCall::julia_help("ModelObsAcousticLogisTrunc")
  pars <- data.frame(sensor_id = 1,
                     depth_shallow_eps = 10,
                     depth_deep_eps = 20)
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = list(ModelObsDepthUniformSeabed = pars))

  #### Example (3): Simulate observations via `ModelObsDepthNormalTruncSeabed`
  # `ModelObsDepthNormalTruncSeabed` is an observation model for depth observations
  pars <- data.frame(sensor_id = 1,
                     depth_sigma = 10,
                     depth_deep_eps = 20)
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = list(ModelObsDepthNormalTruncSeabed = pars))

  #### Example (4): Simulate observations via custom `ModelObs` sub-types
  # See `?ModelObs`

  #### Example (5): Use multiple observation models
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = list(ModelObsAcousticLogisTrunc = moorings,
                                            ModelObsDepthNormalTruncSeabed = pars))
  str(obs)

}
