test_that("sim_array() works", {

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
                .model_move = move_xy(dbn_angle = "VonMises(0, 100)")) |>
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
  expect_true(all(terra::distance(cbind(path$x, path$y), lonlat = FALSE, sequential = TRUE) <= 750.0))

})
