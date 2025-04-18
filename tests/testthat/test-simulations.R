test_that("sim_array() works", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  map      <- dat_gebco()
  timeline <- assemble_timeline(list(dat_detections), .step = "2 mins")

  #### Test basic output properties
  a <- sim_array(.map = map,
                 .timeline = timeline,
                 .n_receiver = 10)
  check_inherits(a, "data.table")
  expect_equal(colnames(a), c("array_id", "receiver_id",
                              "receiver_start", "receiver_end",
                              "receiver_x", "receiver_y",
                              "receiver_alpha", "receiver_beta", "receiver_gamma"))
  expect_equal(nrow(a), 10)
  expect_equal(1:10L, a$receiver_id)

  #### Test multiple array implementation
  a <- sim_array(.map = map,
                 .timeline = timeline,
                 .n_receiver = 2,
                 .n_array = 2)
  expect_equal(a$array_id, c(1, 1, 2, 2))
  expect_equal(a$receiver_id, c(1, 2, 1, 2))

})

test_that("sim_path_walk() works", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  #### Set up test
  timeline <- assemble_timeline(list(dat_detections), .step = "2 mins")[1:1000]
  map      <- dat_gebco()
  set_map(map)
  set_seed()

  #### Test error handling
  # Expect error for incorrectly specified maps or models
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = "StateXY",
                .model_move = model_move_xy(.dbn_heading = "VonMises(0, 100)")) |>
    expect_error()

  #### Test correct implementation
  # Run function
  path <- sim_path_walk(.map = map, .timeline = timeline)
  # Check output properties
  check_inherits(path, "data.table")
  check_names(path, c("path_id", "timestep", "timestamp", "map_value", "x", "y"))
  # Validate map_value()
  expect_equal(path$map_value, terra::extract(map, cbind(path$x, path$y))$map_value)
  # Validate movement distances
  # * A manual computation of sequential distances is used
  # * (terra 1.8-42 introduced a bug in this function)
  path[, dist := NA_real_]
  for (i in 1:(nrow(path) - 1)) {
    # Compute distance from s_{t} to s_{t + 1}
    euclid <- dist_2d(cbind(path$x[i], path$y[i]), cbind(path$x[i + 1], path$y[i + 1]))
    path[i, dist := euclid]
  }
  dist <- path$dist[1:(nrow(path) - 1)]
  expect_true(all(dist <= 750.0))

})
