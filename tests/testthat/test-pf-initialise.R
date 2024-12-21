test_that("pf_filter_init() works", {

  #########################
  #########################
  #### Define helpers

  # Validate map_value
  expect_map_value_correct <- function(.map, .input, .output) {
    output_map_value   <- .output$map_value
    expected_map_value <- terra::extract(.map, cbind(.output$x, .output$y))[, 1]
    expect_true(all(!is.na(output_map_value)))
    expect_true(all(!is.na(expected_map_value)))
    # expect_equal(output_map_value, expected_map_value) # v. slow if mismatch
    expect_true(all(abs(output_map_value - expected_map_value) < 0.01))
  }

  # Validate ModelObsAcousticLogisTrunc
  # * Expect sampled coordinates lie within receiver detection ranges
  expect_ModelObsAcousticLogisTrunc_correct <- function(.map, .input, .output) {
    # Compute distances between points and receivers
    dist <-
      terra::distance(cbind(.input$receiver_x, .input$receiver_y),
                      cbind(.output$x, .output$y),
                      lonlat = FALSE) |>
      unlist() |>
      as.vector()
    # Confirm that distances are within the detection range
    expect_true(max(dist) <= .input$receiver_gamma[1] + terra::res(.map)[1])
    expect_map_value_correct(.map = .map, .input = .input, .output = .output)
  }

  # Validate ModelObsDepthUniform
  # > Confirm map_value lies within valid limits
  expect_ModelObsDepthUniform_correct <- function(.map, .input, .output) {
    depth <- .input$obs[1]
    es    <- .input$depth_shallow_eps[1]
    ed    <- .input$depth_deep_eps[1]
    expect_true(all(depth >= .output$map_value - es & depth <= .output$map_value + ed))
    expect_map_value_correct(.map = .map, .input = .input, .output = .output)
  }

  # Validate ModelObsDepthNormalTrunc
  # > Confirm map_value lies within valid limits
  expect_ModelObsDepthNormalTrunc_correct <- function(.map, .input, .output) {
    depth <- .input$obs[1]
    ed    <- .input$depth_deep_eps[1]
    expect_true(all(depth <= .output$map_value + ed))
    expect_map_value_correct(.map = .map, .input = .input, .output = .output)
  }

  # Expect that the values on two SpatRasters are equal
  expect_spatequal <- function(output, expected) {
    expect_equal(terra::values(output)[, 1], terra::values(expected)[, 1])
  }

  # Compute the detection container
  spatContainer <- function(cinfo, .map) {
    # Define a list of containers
    vcontainers <-
      split(cinfo, seq_row(cinfo)) |>
      lapply(\(d) {
        terra::vect(x = cbind(d$receiver_x, d$receiver_y),
                    crs = terra::crs(.map)) |>
          terra::buffer(width = d$buffer, quadsegs = 1000L)
      })
    # Intersect the containers
    vcontainer <- spatIntersect(vcontainers)
    # Mask the map
    # * Use touches = FALSE for Julia compatibility
    terra::mask(.map, vcontainer, touches = FALSE)
  }

  # Compute the uniform depth container
  spatDepthUniform <- function(.arc, .map) {
    msk <- .map > .arc$obs - .arc$depth_shallow_eps &
      .map < .arc$obs + .arc$depth_deep_eps
    terra::mask(.map, msk, maskvalue = FALSE)
  }

  # Compute the normal depth container
  spatDepthNormalTrunc <- function(.arc, .map) {
    msk <- .map + .arc$depth_deep_eps >= .arc$obs
    terra::mask(.map, msk, maskvalue = FALSE)
  }

  # `Patter.map_init()` wrapper
  map_init <- function(.timeline, .yobs, .direction = "forward") {
    julia_check_exists("env_init", "model_move")
    set_timeline(.timeline)
    set_yobs_vect(.timeline, .yobs)
    set_model_obs_types(.yobs)
    set_direction(.direction)
    julia_command('if !isnothing(yobs_vect) yobs_vect = yobs_vect[1] end;')
    julia_command('if !isnothing(model_obs_types) model_obs_types = model_obs_types[1] end;')
    julia_command('map_init = Patter.map_init(deepcopy(env_init), timeline, model_move, yobs_vect, model_obs_types, direction);')
    julia_eval.map_init()
  }

  # `julia_eval()` wrapper to pass a raster ('map_init') back to R
  julia_eval.map_init <- function() {
    # Write Raster to file
    julia_check_exists("map_init")
    julia_code(
      '
      map_init = Rasters.replace_missing(map_init, NaN)
      map_init_tif = joinpath(tempdir(), "map_init.tif")
      write(map_init_tif, map_init, force = true)
    '
    )
    file <- julia_eval("map_init_tif")
    terra::rast(file)
  }


  #########################
  #########################
  #### Define datasets

  #### Set map
  map <- dat_gebco()
  set_map(map)

  #### Define timeline
  timeline <- seq(as.POSIXct("2016-01-01 09:00:00", tz = "UTC"),
                  as.POSIXct("2016-01-02 00:00:00", tz = "UTC"),
                  by = "2 mins")

  #### Define `yobs` named list
  # Define acoustics data.table
  receiver_alpha <- 4
  receiver_beta <- -0.01
  receiver_gamma <- 500
  acc <- data.table(timestamp = timeline[1],
                    sensor_id = 1L,
                    receiver_x = 709111.2,
                    receiver_y = 6259809,
                    receiver_alpha = 1,
                    receiver_beta = -0.001,
                    receiver_gamma = receiver_gamma,
                    buffer = receiver_gamma,
                    obs = 1L)
  # Define archival data.table
  arc <- data.table(
    timestamp = as.POSIXct(c("2016-01-01 09:00:00",
                             "2016-01-01 12:00:00",
                             "2016-01-02 00:00:00"),
                           tz = "UTC"),
    sensor_id = 1L,
    obs = 50,
    depth_shallow_eps = 10,
    depth_deep_eps = 10,
    depth_sigma = 50)
  # Define list of observations
  yobs <- list(ModelObsAcousticLogisTrunc = acc,
               ModelObsDepthUniform = arc)


  #########################
  #########################
  #### Test sim_states_init()

  #### Test error handling
  # Maps that only contain NAs throw an error
  map_na <- terra::setValues(map, NA)
  set_map(map_na)
  output <- pf_filter_init(.timeline = timeline,
                           .state = "StateXY",
                           .model_move = move_xy(),
                           .yobs = list(),
                           .n_particle = 2L,
                           .direction = "forward") |>
    expect_error("The map only contains missing values.")
  set_map(map)
  # Multi-layered maps throw an error
  map_m <- c(map, map)
  map_m_tif <- tempfile(fileext = ".tif")
  terra::writeRaster(map_m, map_m_tif)
  set_map(map_m_tif) |>
    expect_error("The map should contain two dimensions (one layer) only.",
                 fixed = TRUE)
  unlink(map_m_tif)
  set_map(map)

  #### Test method dispatch
  # Test that `pf_filter_init()` samples from sensible locations from maps with/without NaNs
  map_1 <- terra::deepcopy(map)
  map_2 <- terra::classify(map_1, cbind(NA, 0))
  lapply(list(map_1, map_2), function(map_i) {

    # map_i <- map_2
    set_map(map_i)
    np <- 1e5L

    #### ModelObsAcousticLogisTrunc
    yobs <- list(ModelObsAcousticLogisTrunc = acc)
    output <- pf_filter_init(.timeline = timeline,
                             .state = "StateXY",
                             .model_move = move_xy(),
                             .yobs = yobs,
                             .n_particle = np,
                             .direction = "forward")
    expect_ModelObsAcousticLogisTrunc_correct(.map = map_i, .input = acc, .output = output)

    #### ModelObsDepthUniform
    yobs <- list(ModelObsDepthUniform = arc)
    output <- pf_filter_init(.timeline = timeline,
                             .state = "StateXY",
                             .model_move = move_xy(),
                             .yobs = yobs,
                             .n_particle = np,
                             .direction = "forward")
    expect_ModelObsDepthNormalTrunc_correct(.map = map_i, .input = arc, .output = output)

    #### ModelObsDepthNormalTrunc
    yobs <- list(ModelObsDepthNormalTrunc = arc)
    output <- pf_filter_init(.timeline = timeline,
                             .state = "StateXY",
                             .model_move = move_xy(),
                             .yobs = yobs,
                             .n_particle = np,
                             .direction = "forward")
    expect_ModelObsDepthNormalTrunc_correct(.map = map_i, .input = arc, .output = output)

    #### ModelObsAcousticLogisTrunc, ModelObsDepthUniform
    yobs <- list(ModelObsAcousticLogisTrunc = acc,
                 ModelObsDepthUniform = arc)
    output <- pf_filter_init(.timeline = timeline,
                             .state = "StateXY",
                             .model_move = move_xy(),
                             .yobs = yobs,
                             .n_particle = np,
                             .direction = "forward")
    expect_ModelObsAcousticLogisTrunc_correct(.map = map_i, .input = acc, .output = output)
    expect_ModelObsDepthUniform_correct(.map = map_i, .input = arc, .output = output)

    #### ModelObsDepthUniform, ModelObsAcousticLogisTrunc
    yobs <- list(ModelObsDepthUniform = arc,
                 ModelObsAcousticLogisTrunc = acc)
    output <- pf_filter_init(.timeline = timeline,
                             .state = "StateXY",
                             .model_move = move_xy(),
                             .yobs = yobs,
                             .n_particle = np,
                             .direction = "forward")
    expect_ModelObsAcousticLogisTrunc_correct(.map = map_i, .input = acc, .output = output)
    expect_ModelObsDepthUniform_correct(.map = map_i, .input = arc, .output = output)

  })
  # Reset map
  set_map(map)

  #### Test use of `.xinit`
  xinit  <- data.table(map_value = 1, x = 2, y = 3)
  output <- pf_filter_init(.timeline = timeline,
                           .state = "StateXY",
                           .xinit = data.table(map_value = 1, x = 2, y = 3),
                           .model_move = move_xy(),
                           .yobs = list(),
                           .n_particle = 2L,
                           .direction = "forward")
  expect_equal(output, rbind(xinit, xinit))


  #########################
  #########################
  #### Test individual map_init() methods

  #### Define movement model
  mobility = 750.0
  set_model_move(move_xy())

  #### `map_init.default()`
  # `map_init.default()` simply returns `.map`
  set_yobs_vect(timeline, list())
  set_model_obs_types(list())
  expect_spatequal(map_init(timeline, list()), map)

  #### `map_init.ModelObsAcousticLogisTrunc()`

  # No detections
  acc <- data.table(timestamp = timeline[1],
                    sensor_id = 1L,
                    receiver_x = 709111.2,
                    receiver_y = 6259809,
                    receiver_alpha = receiver_alpha,
                    receiver_beta = receiver_beta,
                    receiver_gamma = receiver_gamma,
                    buffer = NA,
                    obs = 0L)
  yobs <- list(ModelObsAcousticLogisTrunc = acc)
  expect_spatequal(map_init(timeline, yobs), map)

  # One detection at timeline[1]
  acc <- data.table(timestamp = timeline[1],
                    sensor_id = 1L,
                    receiver_x = 709111.2,
                    receiver_y = 6259809,
                    receiver_alpha = receiver_alpha,
                    receiver_beta = receiver_beta,
                    receiver_gamma = receiver_gamma,
                    buffer = receiver_gamma,
                    obs = 1L)
  yobs <- list(ModelObsAcousticLogisTrunc = acc)
  expect_spatequal(map_init(timeline, yobs), spatContainer(acc, map))

  # Multiple starting detections (before, at, after)
  acc <- data.table(timestamp = as.POSIXct(c("2016-01-01 08:58:00",
                                             "2016-01-01 09:00:00",
                                             "2016-01-01 09:02:00"),
                                           tz = "UTC"),
                    sensor_id = 1:3,
                    receiver_x = c(706487.1, 706487.1, 706566.6),
                    receiver_y = c(6265773, 6265694, 6265694),
                    receiver_alpha = receiver_alpha,
                    receiver_beta = receiver_beta,
                    receiver_gamma = receiver_gamma,
                    buffer = c(receiver_gamma + mobility, receiver_gamma, receiver_gamma + mobility),
                    obs = 1L)
  yobs <- list(ModelObsAcousticLogisTrunc = acc)
  expect_spatequal(map_init(timeline, yobs), spatContainer(acc, map))

  # One detection at timeline[T]
  acc <- data.table(timestamp = timeline[length(timeline)],
                    sensor_id = 1L,
                    receiver_x = 709111.2,
                    receiver_y = 6259809,
                    receiver_alpha = receiver_alpha,
                    receiver_beta = receiver_beta,
                    receiver_gamma = receiver_gamma,
                    buffer = receiver_gamma,
                    obs = 1L)
  yobs <- list(ModelObsAcousticLogisTrunc = acc)
  expect_spatequal(map_init(timeline, yobs, "backward"), spatContainer(acc, map))

  #### `map_init.ModelObsDepthUniform()`

  # No observation
  yobs <- list(ModelObsDepthUniform = arc[2, ])
  expect_spatequal(map_init(timeline, yobs), map)

  # Observation at timeline[1]
  yobs <- list(ModelObsDepthUniform = arc[1, ])
  expect_spatequal(map_init(timeline, yobs), spatDepthUniform(arc[1, ], map))

  # Observation at timeline[T]
  yobs <- list(ModelObsDepthUniform = arc[3, ])
  expect_spatequal(map_init(timeline, yobs, "backward"), spatDepthUniform(arc[3, ], map))

  #### `map_init.ModelObsDepthNormalTrunc()`

  # No observation
  yobs <- list(ModelObsDepthNormalTrunc = arc[2, ])
  expect_spatequal(map_init(timeline, yobs), map)

  # Observation at timeline[1]
  yobs <- list(ModelObsDepthNormalTrunc = arc[1, ])
  expect_spatequal(map_init(timeline, yobs), spatDepthNormalTrunc(arc[1, ], map))

  # Observation at timeline[T]
  yobs <- list(ModelObsDepthNormalTrunc = arc[3, ])
  expect_spatequal(map_init(timeline, yobs, "backward"), spatDepthNormalTrunc(arc[3, ], map))

  # map_init_iter()
  # (optional) TO DO
  # * This is indirectly tested above.

  # Clean up
  unlink(julia_eval("map_init_tif"))


  #########################
  #########################
  #### coords_init()

  # Patter.coords_init() R wrapper
  coords_init <- function(size = 1L) {
    julia_eval(glue('coords_init = Patter.coords_init(env_init, {size});'))
  }

  # Test basic implementation
  sxy <- coords_init(size = 100L)
  check_names(sxy, c("map_value", "x", "y"))
  expect_true(nrow(sxy) == 100L)

  # Test massive sampling
  sxy <- coords_init(1e6L)
  expect_true(nrow(sxy) == 1e6L)

  # Test sampling when only one valid cell
  one <- terra::setValues(map, NA)
  one[10] <- 1
  xy <- terra::xyFromCell(one, 10)
  set_map(one, .as_Raster = TRUE, .as_GeoArray = FALSE)
  sxy <- coords_init(2L)
  expect_equal(sxy,
               data.frame(map_value = c(1, 1),
                          x = c(xy[, 1], xy[, 1]),
                          y = c(xy[, 2], xy[, 2])))

  # Repeat test with large sample
  sxy <- coords_init(1000L)
  expect_true(all(sxy$map_value == 1L))


  #########################
  #########################
  #### states_init()

  # `Patter.states_init()` R wrapper that uses `coords_init` object in Julia defined by the `coords_init()` R wrapper
  states_init <- function(state_type = "StateXY") {
    julia_eval(glue('Patter.states_init({state_type}, coords_init)'))
  }

  # `states_init.default()`
  julia_command('struct StateCustom <: Patter.State end')
  states_init(state_type = "StateCustom") |>
    expect_error("For custom states, you need to define a `Patter.states_init()` method or provide `.xinit`.",
                 fixed = TRUE)

  # `states_init.StateXY()`
  output <- states_init(state_type = "StateXY")
  expect_equal(output, sxy)

  # `states_init.StateXYZ()`
  # TO DO

  # `states_init.StateCXY`
  # TO DO

  # `states_init.StateCXYZ()`
  output <- states_init(state_type = "StateCXYZ")
  check_names(output, c("map_value", "x", "y", "z", "heading"))



})

