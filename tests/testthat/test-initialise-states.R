test_that("sim_states_init() and associates work", {


  #########################
  #### Set up

  #### Define helper functions

  # Test that two SpatRasters are equal
  expect_spatequal <- function(output, expected, ...) {
    expect_true(terra::all.equal(output, expected, ...))
  }

  # Compute the detection container
  spatContainer <- function(cinfo, .map) {
    vcontainers <-
      split(cinfo, seq_row(cinfo)) |>
      lapply(\(d) {
        terra::vect(x = cbind(d$receiver_x, d$receiver_y),
                    crs = terra::crs(.map)) |>
          terra::buffer(width = d$buffer, quadsegs = 1000L)
      })
    vcontainer <- spatIntersect(vcontainers)
    rcontainer <- terra::crop(.map, vcontainer,
                              mask = TRUE, touches = TRUE,
                              extend = FALSE, snap = "out")
    rcontainer
  }

  # Compute the uniform depth container
  spatDepthUniform <- function(.map, .arc) {
    msk <- .map > .arc$obs - .arc$depth_shallow_eps &
      .map < .arc$obs + .arc$depth_deep_eps
    terra::mask(.map, msk, maskvalue = FALSE)
  }

  # Compute the normal depth container
  spatDepthNormalTrunc <- function(.map, .arc) {
    msk <- .map + .arc$depth_deep_eps >= .arc$obs
    terra::mask(.map, msk, maskvalue = FALSE)
  }

  #### Define datasets

  # Map
  map <- dat_gebco()

  # Timeline
  timeline <- seq(as.POSIXct("2016-01-01 09:00:00", tz = "UTC"),
                  as.POSIXct("2016-01-02 00:00:00", tz = "UTC"),
                  by = "2 mins")

  # Acoustics
  gamma    <- 500
  mobility <- 400
  acc <- data.table(timestamp = timeline[1],
                    receiver_x = 709111.2,
                    receiver_y = 6259809,
                    receiver_gamma = gamma,
                    buffer = gamma,
                    obs = 1)

  # Archival
  arc <- data.table(
    timestamp = as.POSIXct(c("2016-01-01 09:00:00",
                             "2016-01-01 12:00:00",
                             "2016-01-02 00:00:00"),
                           tz = "UTC"),
    sensor_id = 1L,
    obs = 50,
    depth_shallow_eps = 10,
    depth_deep_eps = 10)



  #########################
  #### sim_states_init()

  #### Use .xinit
  xinit  <- data.table(map_value = 1, x = 2, y = 3)
  output <- sim_states_init(.map = map,
                            .timeline = timeline,
                            .direction = "forward",
                            .datasets = list(),
                            .models = NULL,
                            .pars = list(),
                            .state = "StateXY",
                            .xinit = data.table(map_value = 1, x = 2, y = 3),
                            .n = 2L)
  expect_equal(output, rbind(xinit, xinit))

  #### Test checks
  # All NAs
  # Missing .pars
  output <- sim_states_init(.map = terra::setValues(map, NA),
                            .timeline = timeline,
                            .direction = "forward",
                            .datasets = list(acc, arc),
                            .models = c("ModelObsAcousticLogisTrunc",
                                        "ModelObsDepthUniform"),
                            .pars = list(mobility  = 750),
                            .state = "StateXY",
                            .xinit = NULL,
                            .n = 2L) |>
    expect_warning("[spatSample] fewer samples than requested are available", fixed = TRUE) |>
    expect_warning("`.map` from `map_init()` only contains NAs.",
                   fixed = TRUE) |>
    expect_error("Failed to sample initial coordinates from `.map`.",
                 fixed = TRUE)
  output <- sim_states_init(.map = map,
                            .timeline = timeline,
                            .direction = "forward",
                            .datasets = list(acc, arc),
                            .models = c("ModelObsAcousticLogisTrunc",
                                        "ModelObsDepthUniform"),
                            .pars = list(),
                            .state = "StateXY",
                            .xinit = NULL,
                            .n = 2L) |>
    expect_error("`.pars = list(mobility = ...)` is required.",
                 fixed = TRUE)

  #### Test method dispatch
  output <- sim_states_init(.map = map,
                            .timeline = timeline,
                            .direction = "forward",
                            .datasets = list(acc, arc),
                            .models = c("ModelObsAcousticLogisTrunc",
                                        "ModelObsDepthUniform"),
                            .pars = list(mobility = mobility),
                            .state = "StateXY",
                            .xinit = NULL,
                            .n = 1e6L)
  # Confirm that all sampled coordinates are within the detection range
  dist <- terra::distance(cbind(acc$receiver_x, acc$receiver_y),
                          cbind(output$x, output$y),
                          lonlat = FALSE) |> unlist() |> as.vector()
  expect_true(max(dist) <= gamma + terra::res(map)[1])
  # Confirm map_value lies within valid limits
  depth <- arc$obs[1]
  es    <- arc$depth_shallow_eps[1]
  ed    <- arc$depth_deep_eps[1]
  expect_true(all(depth >= output$map_value - es & depth <= output$map_value + ed))


  #########################
  #### map_init()

  #### map_init.default()
  # map_init.default() simply returns `.map`
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "forward",
                     .dataset = list(),
                     .model = char_to_class("ModelObsCustom"),
                     .pars = list())
  expect_equal(output, map)

  #### map_init.ModelObsAcousticLogisTrunc()
  # No detections
  acc <- data.table(timestamp = timeline[1],
                    receiver_x = 709111.2,
                    receiver_y = 6259809,
                    receiver_gamma = gamma,
                    obs = 0)
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "forward",
                     .dataset = acc,
                     .model = char_to_class("ModelObsAcousticLogisTrunc"),
                     .pars = list(mobility = mobility, plot = TRUE))
  expect_spatequal(output, map)
  # One detection at timeline[1]
  acc <- data.table(timestamp = timeline[1],
                    receiver_x = 709111.2,
                    receiver_y = 6259809,
                    receiver_gamma = gamma,
                    buffer = gamma,
                    obs = 1)
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "forward",
                     .dataset = acc,
                     .model = char_to_class("ModelObsAcousticLogisTrunc"),
                     .pars = list(mobility = mobility, plot = TRUE))
  expect_spatequal(output, spatContainer(acc, map))
  # Multiple starting detections (before, at, after)
  acc <- data.table(timestamp = as.POSIXct(c("2016-01-01 08:58:00",
                                             "2016-01-01 09:00:00",
                                             "2016-01-01 09:02:00"),
                                           tz = "UTC"),
                    receiver_x = c(706487.1, 706487.1, 706566.6),
                    receiver_y = c(6265773, 6265694, 6265694),
                    receiver_gamma = gamma,
                    buffer = c(gamma + mobility, gamma, gamma + mobility),
                    obs = 1)
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "forward",
                     .dataset = acc,
                     .model = char_to_class("ModelObsAcousticLogisTrunc"),
                     .pars = list(mobility = mobility, plot = TRUE))
  expect_spatequal(output, spatContainer(acc, map))
  # One detection at timeline[T]
  acc <- data.table(timestamp = timeline[length(timeline)],
                    receiver_x = 709111.2,
                    receiver_y = 6259809,
                    receiver_gamma = 500,
                    buffer = 500,
                    obs = 1)
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "backward",
                     .dataset = acc,
                     .model = char_to_class("ModelObsAcousticLogisTrunc"),
                     .pars = list(mobility = 500))
  expect_spatequal(output, spatContainer(acc, map))

  ##### map_init.ModelObsDepthUniform()
  # No observation
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "forward",
                     .dataset = arc[2, ],
                     .model = char_to_class("ModelObsDepthUniform"),
                     .pars = list())
  expect_spatequal(output, map)
  # Observation at timeline[1]
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "forward",
                     .dataset = arc[1, ],
                     .model = char_to_class("ModelObsDepthUniform"))
  expect_spatequal(output,
                   spatDepthUniform(map, arc[1, ]))
  # Observation at timeline[T]
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "backward",
                     .dataset = arc[3, ],
                     .model = char_to_class("ModelObsDepthUniform"))
  expect_spatequal(output,
                   spatDepthUniform(map, arc[3, ]))

  # map_init.ModelObsDepthNormalTrunc()
  # No observation
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "forward",
                     .dataset = arc[2, ],
                     .model = char_to_class("ModelObsDepthNormalTrunc"),
                     .pars = list())
  expect_spatequal(output, map)
  # Observation at timeline[1]
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "forward",
                     .dataset = arc[1, ],
                     .model = char_to_class("ModelObsDepthNormalTrunc"))
  expect_spatequal(output,
                   spatDepthNormalTrunc(map, arc[1, ]))
  # Observation at timeline[T]
  output <- map_init(.map = map,
                     .timeline = timeline,
                     .direction = "backward",
                     .dataset = arc[3, ],
                     .model = char_to_class("ModelObsDepthNormalTrunc"))
  expect_spatequal(output,
                   spatDepthNormalTrunc(map, arc[3, ]))

  # map_init_iter()
  # * TO DO


  #########################
  #### coords_init()

  # Test basic implementation
  sxy <- coords_init(map, .n = 100L)
  check_inherits(sxy, "data.table")
  check_names(sxy, c("map_value", "x", "y"))
  expect_true(nrow(sxy) == 100L)
  # Test massive sampling
  sxy <- coords_init(map, .n = 1e6L)
  check_inherits(sxy, "data.table")
  expect_true(nrow(sxy) == 1e6L)
  # Test exhaustive sampling
  # * This returns one cell
  # * sim_states_init() then resamples n copies
  one <- terra::setValues(map, NA)
  one[10] <- 1
  xy <- terra::xyFromCell(one, 10)
  sxy <- suppressWarnings(coords_init(one, .n = 2L))
  expect_equal(sxy,
               data.table(map_value = c(1, 1),
                          x = c(xy[, 1], xy[, 1]),
                          y = c(xy[, 2], xy[, 2])))

  #########################
  #### states_init()

  # states_init.default()

  states_init(.state = char_to_class("StateCustom"), .coords = data.table()) |>
    expect_error("For custom states, you need to define a `states_init()` S3 method or provide `.xinit`.",
                 fixed = TRUE)

  # states_init.StateXY()
  output <- states_init(.state = char_to_class("StateXY"), .coord = copy(sxy))
  expect_equal(output, sxy)

  # states_init.StateXYZD()
  output <- states_init(.state = char_to_class("StateXYZD"), .coord = copy(sxy))
  check_names(output, c("map_value", "x", "y", "z", "angle"))

})


