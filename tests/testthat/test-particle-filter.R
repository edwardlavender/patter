test_that("pf_filter() reconstructs the true path", {

  set_seed()

  # Define map
  map      <- dat_gebco()
  map[]    <- as.numeric(1:terra::ncell(map))
  set_map(map)

  # Define timeline
  timeline <- seq(as.POSIXct("2023-01-01 12:00:00", tz = "UTC"),
                  as.POSIXct("2023-01-31 23:58:00", tz = "UTC"),
                  by = "2 mins")
  timeline <- timeline[1:500]

  # Simulate an acoustic array
  moorings <- sim_array(.map = map,
                        .timeline = timeline,
                        .n_receiver = 100L,
                        .arrangement = "regular")

  # Simulate a movement path
  x <- 708903.5
  y <- 6252070
  xinit <- data.table(map_value = terra::extract(map, cbind(x, y))[1, 1],
                      x = x, y = y)
  expect_true(!is.na(xinit$map_value))
  state      <- "StateXY"
  model_move <- move_xy()
  paths      <- sim_path_walk(.map = map,
                              .timeline = timeline,
                              .state = state,
                              .model_move = model_move,
                              .xinit = xinit)
  xinit_end <- paths[.N, .(map_value, x, y)]

  # Simulate observations
  model_obs <- c("ModelObsAcousticLogisTrunc", "ModelObsDepthUniform")
  pars_acc <-
    moorings |>
    select(sensor_id = "receiver_id",
           "receiver_x", "receiver_y",
           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
    as.data.table()
  pars_arc <- data.table(sensor_id = 1L,
                         depth_shallow_eps = 0,
                         depth_deep_eps = 0)
  obs <-
    sim_observations(.timeline = timeline,
                     .model_obs = model_obs,
                     .model_obs_pars = list(pars_acc, pars_arc))

  # Run forward filter
  pff <- pf_filter(.map = map,
                   .timeline = timeline,
                   .state = "StateXY",
                   .xinit = xinit,
                   .yobs = list(obs$ModelObsAcousticLogisTrunc[[1]],
                                obs$ModelObsDepthUniform[[1]]),
                   .model_obs = model_obs,
                   .model_move = model_move,
                   .n_particle = 1e5L) |>
    # Suppress convergence warnings
    suppressWarnings()

  # Validate forward filter
  states <-
    pff$states |>
    left_join(paths, by = "timestep", suffix = c(".state", ".path"))
  expect_true(all.equal(states$map_value.path, states$map_value.state))

  # Run backward filter
  pfb <- pf_filter(.map = map,
                   .timeline = timeline,
                   .state = "StateXY",
                   .xinit = xinit_end,
                   .yobs = list(obs$ModelObsAcousticLogisTrunc[[1]],
                                obs$ModelObsDepthUniform[[1]]),
                   .model_obs = model_obs,
                   .model_move = model_move,
                   .n_particle = 1e5L,
                   .direction = "backward") |>
    # Suppress convergence warnings
    suppressWarnings()

  # Validate backward filter
  states <-
    pfb$states |>
    left_join(paths, by = "timestep", suffix = c(".state", ".path"))
  expect_true(all.equal(states$map_value.path, states$map_value.state))

})


test_that("pf_filter() works", {

  #########################
  #### Run the particle filter

  # Set map
  set_seed()
  map <- dat_gebco()
  set_map(map)

  # Define datasets for a selected individual
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
  acoustics <- assemble_acoustics(.timeline = timeline,
                                  .acoustics = acc,
                                  .moorings = dat_moorings)

  # Assemble a timeline of archival observations and model parameters
  # * Here, we include model parameters for `ModelObsDepthNormalTrunc`
  archival <- assemble_archival(.timeline = timeline,
                                .archival =
                                  arc |>
                                  rename(obs = depth) |>
                                  mutate(depth_sigma = 50,
                                         depth_deep_eps = 20))

  # Examine movement prior
  # sim_path_walk(.map = map, .timeline = timeline)

  # Run the filter (with errors)
  timeline_cet <- timeline
  lubridate::tz(timeline_cet) <- "CET"
  fwd <- pf_filter(.map = map,
                   .timeline = timeline_cet,
                   .state = "StateXY",
                   .xinit = NULL, .xinit_pars = list(mobility = 750, plot = TRUE),
                   .yobs = list(acoustics, archival),
                   .model_obs = c("ModelObsAcousticLogisTrunc", "ModelObsDepthNormalTrunc"),
                   .model_move = move_xy()) |>
    expect_error('There is a mismatch between the time zones of `.timeline` and/or `.yobs` `timestamp`s ("CET", "UTC", "UTC").', fixed = TRUE)

  # Run the filter
  # * Note that we do not expect convergence given the small & coarse bathymetric data
  fwd <- pf_filter(.map = map,
                   .timeline = timeline,
                   .state = "StateXY",
                   .xinit = NULL, .xinit_pars = list(mobility = 750, plot = TRUE),
                   .yobs = list(acoustics, archival),
                   .model_obs = c("ModelObsAcousticLogisTrunc", "ModelObsDepthNormalTrunc"),
                   .model_move = move_xy(),
                   .n_move = 1e6L,
                   .n_particle = 1000L,
                   .n_resample = 1000,
                   .n_record = 1e3L,
                   .direction = "forward")


  #########################
  #### Validate outputs

  #### Validate object output
  expect_true(all(c("list", "pf_particles") %in% class(fwd)))
  check_names(fwd, c("xinit", "states", "diagnostics", "convergence"))
  check_names(fwd$states, c("path_id", "timestep", "timestamp", "map_value", "x", "y"))
  check_names(fwd$diagnostics, c("timestep", "timestamp", "ess", "maxlp"))
  check_inherits(fwd$convergence, "logical")

  #### Test that that movement distances are within mobility
  # This is not possible directly since we do not track particle histories
  # But we can confirm that at least some some movement distances are < mobility at each time step
  # NB: Define states from 1:(T - 1)
  # (We focus on valid states only, since non-convergence is expected)
  states <- fwd$states[timestep %in% 1:(max(timestep) - 1), ]
  for (t in sort(unique(states$timestep))) {
    if (t < max(t)) {
      mat <- terra::distance(states[timestep == t, .(x, y)] |> as.matrix(),
                      states[timestep == t, .(x, y)] |> as.matrix(),
                      lonlat = FALSE)
      expect_true(any(mat < 750))
    }
  }

  #### Test that at the moment of detection particles are widthin detection ranges (in R)
  # Define detections
  detections <-
    acoustics |>
    filter(obs == 1) |>
    select(timestamp, receiver_x, receiver_y) |>
    as.data.table()
  # Calculate distances between particles & receivers @ moment of detection
  check_detection_distance <-
    states |>
    # Focus on a subsample of path(s) for speed
    filter(path_id %in% 1:100) |>
    select(timestamp, x, y) |>
    filter(timestamp %in% detections$timestamp) |>
    join(detections, on = "timestamp") |>
    mutate(dist = terra::distance(cbind(x, y), cbind(receiver_x, receiver_y), lonlat = FALSE, pairwise = TRUE)) |>
    as.data.table()
  # Validate distances
  expect_true(max(check_detection_distance$dist) <= 750)

  #### Test the distribution of distances from receivers during detection gaps
  # * TO DO

  #### Test that particles are valid given depth observations
  check <- left_join(states, archival, by = "timestamp")
  check$test <- check$obs <= check$map_value + check$depth_deep_eps
  expect_true(all(check$test))

})
