test_that("pf_filter() reconstructs the true path", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

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
  pars_acc <-
    moorings |>
    select(sensor_id = "receiver_id",
           "receiver_x", "receiver_y",
           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
    as.data.table()
  pars_arc <- data.table(sensor_id = 1L,
                         depth_shallow_eps = 0,
                         depth_deep_eps = 0)
  model_obs <- list(ModelObsAcousticLogisTrunc = pars_acc,
                    ModelObsDepthUniformSeabed = pars_arc)
  sobs <-
    sim_observations(.timeline = timeline,
                     .model_obs = model_obs)
  yobs <-
    list(ModelObsAcousticLogisTrunc = sobs$ModelObsAcousticLogisTrunc[[1]],
         ModelObsDepthUniformSeabed = sobs$ModelObsDepthUniformSeabed[[1]])

  # Run forward filter
  fwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .xinit = xinit,
                   .yobs = yobs,
                   .model_move = model_move,
                   .n_particle = 1e5L) |>
    # Suppress convergence warnings
    suppressWarnings()

  # Validate forward filter
  states <-
    fwd$states |>
    left_join(paths, by = "timestep", suffix = c(".state", ".path"))
  expect_true(all.equal(states$map_value.path, states$map_value.state))

  # Run backward filter
  bwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .xinit = xinit_end,
                   .yobs = yobs,
                   .model_move = model_move,
                   .n_particle = 1e5L,
                   .direction = "backward") |>
    # Suppress convergence warnings
    suppressWarnings()

  # Validate backward filter
  states <-
    bwd$states |>
    left_join(paths, by = "timestep", suffix = c(".state", ".path"))
  expect_true(all.equal(states$map_value.path, states$map_value.state))

})

test_that("pf_filter() works", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  #########################
  #### Run the particle filter

  # Set map
  set_seed()
  map <- dat_gebco()
  set_map(map)

  # Define datasets for a selected individual
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

  # Assemble acoustic containers
  containers <- assemble_acoustics_containers(.timeline = timeline,
                                              .acoustics = acoustics,
                                              .mobility = 750.0)

  # Assemble a timeline of archival observations and model parameters
  # * Here, we include model parameters for `ModelObsDepthNormalTruncSeabed`
  archival <- assemble_archival(.timeline = timeline,
                                .archival =
                                  arc |>
                                  rename(obs = depth) |>
                                  mutate(depth_sigma = 50,
                                         depth_deep_eps = 20))

  # Assemble yobs
  yobs_fwd <- list(ModelObsAcousticLogisTrunc = acoustics,
               ModelObsAcousticContainer = containers$forward,
               ModelObsDepthNormalTruncSeabed = archival)

  # Examine movement prior
  # sim_path_walk(.map = map, .timeline = timeline)

  # Run the filter (with errors)
  timeline_cet <- timeline
  lubridate::tz(timeline_cet) <- "CET"
  fwd <- pf_filter(.timeline = timeline_cet,
                   .state = "StateXY",
                   .xinit = NULL,
                   .yobs = yobs_fwd,
                   .model_move = move_xy()) |>
    expect_error('There is a mismatch between the time zones of `.timeline` and/or `.yobs` `timestamp`s ("CET", "UTC", "UTC", "UTC").', fixed = TRUE)

  # Run the filter
  # * Note that we do not expect convergence given the small & coarse bathymetric data
  fwd <- pf_filter(.timeline = timeline,
                   .state = "StateXY",
                   .xinit = NULL,
                   .yobs = yobs_fwd,
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
  check_names(fwd, c("states", "diagnostics", "callstats"))
  check_names(fwd$states, c("path_id", "timestep", "timestamp", "map_value", "x", "y"))
  check_names(fwd$diagnostics, c("timestep", "timestamp", "ess", "maxlp"))
  check_names(fwd$callstats, c("timestamp", "routine",
                               "n_particle", "n_iter", "convergence",
                               "time"))

  #### Validate timestamps
  times <- data.table(timestep = seq_len(length(timeline)),
                      timestamp = timeline)
  expect_equal(fwd$states$timestep,
               times$timestep[match(fwd$states$timestamp, times$timestamp)]
               )
  expect_equal(fwd$states$timestamp,
               times$timestamp[match(fwd$states$timestep, times$timestep)]
  )
  expect_equal(fwd$diagnostics[, .(timestep, timestamp)], times)

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

  #### Test that at the moment of detection particles are within detection ranges (in R)
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

  #### Test that particles are valid given detection containers
  lapply(split(containers$forward, seq_row(containers$forward)), function(container) {
    # container <- containers$forward[1, ]
    s    <- fwd$states[timestamp == container$timestamp, ]
    dist <- terra::distance(cbind(s$x, s$y), cbind(container$receiver_x, container$receiver_y), lonlat = FALSE)[, 1]
    expect_true(all(dist <= container$radius))
  }) |> invisible()

  #### Test that particles are valid given depth observations
  check <- left_join(states, archival, by = "timestamp")
  check$test <- check$obs <= check$map_value + check$depth_deep_eps
  expect_true(all(check$test))

})

test_that("pf_filter() permits .yobs = list()", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  setup      <- example_setup("pf_smoother_two_filter", .connect = FALSE)
  args       <- setup$pf_filter_args
  args$.yobs <- list()
  fwd_1      <- do.call(pf_filter, args)
  check_named_list(fwd_1)
  check_inherits(fwd_1$states, "data.table")
  expect_true(all(!is.na(fwd_1$states)))
})

test_that("pf_filter() & pf_smoother_two_filter() work for all states", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  set_seed()

  #### Define map
  map <- dat_gebco()
  set_map(map)

  #### Define timeline
  timeline <- seq(as.POSIXct("2023-01-01 12:00:00", tz = "UTC"),
                  as.POSIXct("2023-01-31 23:58:00", tz = "UTC"),
                  by = "2 mins")
  timeline <- timeline[1:100]

  #### Define observation model parameters
  # Simulate array
  moorings <- sim_array(.map = map,
                        .timeline = timeline,
                        .n_receiver = 100L,
                        .arrangement = "regular")
  # Collect acoustic parameters
  pars_acc <-
    moorings |>
    select(sensor_id = "receiver_id",
           "receiver_x", "receiver_y",
           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
    as.data.table()
  # Collect archival parameters
  pars_arc <- data.table(sensor_id = 1L,
                         depth_shallow_eps = 0,
                         depth_deep_eps = 0)
  # Collect the model_obs list
  model_obs <- list(ModelObsAcousticLogisTrunc = pars_acc,
                    ModelObsDepthUniformSeabed = pars_arc)

  #### Implement algorithms
  mapply(function(.state, .model_move, .columns) {

    # Simulate a movement path
    paths <- sim_path_walk(.map         = map,
                           .timeline    = timeline,
                           .state       = .state,
                           .model_move  = .model_move)
    expect_equal(colnames(paths),
                 c("path_id", "timestep", "timestamp", "map_value", .columns))

    # Simulate observations
    sobs <-
      sim_observations(.timeline = timeline,
                       .model_obs = model_obs)
    yobs <-
      list(ModelObsAcousticLogisTrunc = sobs$ModelObsAcousticLogisTrunc[[1]],
           ModelObsDepthUniformSeabed       = sobs$ModelObsDepthUniformSeabed[[1]])

    # Run forward filter
    fwd <- pf_filter(.timeline   = timeline,
                     .state      = .state,
                     .yobs       = yobs,
                     .model_move = .model_move,
                     .n_particle = 1e5L)
    expect_equal(colnames(fwd$states),
                 c("path_id", "timestep", "timestamp", "map_value", .columns))

    # Run backward filter
    bwd <- pf_filter(.timeline   = timeline,
                     .state      = .state,
                     .yobs       = yobs,
                     .model_move = .model_move,
                     .n_particle = 1e5L,
                     .direction  = "backward")
    expect_equal(colnames(bwd$states),
                 c("path_id", "timestep", "timestamp", "map_value", .columns))

    # Run smoother
    set_vmap()
    smo <- pf_smoother_two_filter(.n_particle = 100L)
    expect_equal(colnames(smo$states),
                 c("path_id", "timestep", "timestamp", "map_value", .columns))

    NULL

  },
  list("StateXY", "StateXYZ", "StateCXY", "StateCXYZ"),
  list(move_xy(), move_xyz(), move_cxy(), move_cxyz()),
  list(c("x", "y"), c("x", "y", "z"), c("x", "y", "heading"), c("x", "y", "z", "heading"))
  )

})

test_that("pf_filter() permits missing .yobs", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  setup <- example_setup("pf_smoother_two_filter", .connect = FALSE)
  args  <- setup$pf_filter_args
  # Run the particle filter forwards
  set_seed()
  fwd_1 <- do.call(pf_filter, args)
  # Set yobs to missing
  yobs      <- args$.yobs
  args$yobs <- NULL
  set_seed()
  fwd_2 <- do.call(pf_filter, args)
  # Expect identical outputs b/c pf_filter() uses `yobs` already defined in Julia
  elms <- c("states", "diagnostics")
  expect_equal(fwd_1[elms], fwd_2[elms])
  # Repeat
  args$.yobs <- yobs
  set_seed()
  fwd_3 <- do.call(pf_filter, args)
  expect_equal(fwd_2[elms], fwd_3[elms])

})

test_that("pf_filter() callstats are correct", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)

  set_seed()

  #### Define study period
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  as.POSIXct("2016-01-01 03:18:00", tz = "UTC"),
                  by = "2 mins")

  ### Define study period
  map <- dat_gebco()
  set_map(map)

  #### Simulate an acoustic array
  moorings <- sim_array(.map = map,
                        .timeline = timeline,
                        .n_receiver = 100L)

  #### Simulate a movement path
  state    <- "StateXY"
  mobility <- 750
  model_move <-
    move_xy(.mobility   = "750.0",
            .dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
            .dbn_heading  = "Uniform(-pi, pi)")
  paths <- sim_path_walk(.map = map,
                         .timeline = timeline,
                         .state = state,
                         .model_move = model_move)

  #### Simulate observations
  obs <- sim_observations(
    .timeline = timeline,
    .model_obs =
      list(ModelObsAcousticLogisTrunc =
             moorings |>
             select(sensor_id = "receiver_id", "receiver_x", "receiver_y",
                    "receiver_alpha", "receiver_beta", "receiver_gamma") |>
             as.data.table(),
           ModelObsDepthUniformSeabed =
             data.table(sensor_id = 1L,
                        depth_shallow_eps = 10,
                        depth_deep_eps = 10)))
  yobs <- list(ModelObsAcousticLogisTrunc = obs$ModelObsAcousticLogisTrunc[[1]],
               ModelObsDepthUniformSeabed = obs$ModelObsDepthUniformSeabed[[1]])

  #### Run forward filter with few particles
  fwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .xinit = NULL,
                   .model_move = model_move,
                   .yobs = yobs,
                   .n_particle = 10L,
                   .n_record = 10L,
                   .n_iter = 3,
                   .direction = "forward")

  expect_equal(fwd$callstats$routine, "filter: forward")
  expect_equal(fwd$callstats$n_particle, 10L)
  expect_equal(fwd$callstats$n_iter, 3L)
  expect_false(fwd$callstats$convergence)

  #### Run forward filter with more particles
  fwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .xinit = NULL,
                   .model_move = model_move,
                   .yobs = yobs,
                   .n_particle = 1e4L,
                   .direction = "forward")

  expect_equal(fwd$callstats$routine, "filter: forward")
  expect_equal(fwd$callstats$n_particle, 1e4L)
  expect_equal(fwd$callstats$n_iter, 1L)
  expect_true(fwd$callstats$convergence)

  #### Run backward filter with more particles
  bwd <- pf_filter(.timeline = timeline,
                   .state = state,
                   .xinit = NULL,
                   .model_move = model_move,
                   .yobs = yobs,
                   .n_particle = 1e4L,
                   .direction = "backward")

  expect_equal(bwd$callstats$routine, "filter: backward")
  expect_equal(bwd$callstats$n_particle, 1e4L)
  expect_equal(bwd$callstats$n_iter, 1L)
  expect_true(bwd$callstats$convergence)

  #### Run smoother
  smo <- pf_smoother_two_filter(.n_particle = 10)
  expect_equal(smo$callstats$routine, "smoother: two-filter")
  expect_equal(smo$callstats$n_particle, 10L)
  expect_equal(smo$callstats$n_iter, NA_integer_)
  expect_true(smo$callstats$convergence)

})
