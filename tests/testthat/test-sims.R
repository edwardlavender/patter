test_that("sim_array() works", {

  #### Test checks
  sim_array(.receiver_range = 1:10) |>
    expect_error("Single inputs are expected for `.receiver_start`, `.receiver_end` and `.receiver_range`.", fixed = TRUE)
  sim_array(.receiver_start = as.Date("2016-01-01"),
            .receiver_end = as.Date("2015-01-01")) |>
    expect_warning("`.receiver_end` should be after `.receiver_start`.",
                   fixed = TRUE)

  #### Test basic output properties
  # Test default implementation
  a <- sim_array(.n_receiver = 10)
  check_inherits(a, "data.table")
  expect_equal(colnames(a), c("array_id", "receiver_id",
                              "receiver_easting", "receiver_northing"))
  expect_equal(nrow(a), 10)
  expect_equal(1:10L, a$receiver_id)
  # Test column names with .lonlat = TRUE
  a <- sim_array(.lonlat = TRUE)
  expect_equal(colnames(a), c("array_id", "receiver_id",
                              "receiver_lon", "receiver_lat"))
  # Test column names/values with receiver_start/end/range
  a <- sim_array(.receiver_start = as.Date("2016-01-01"),
                 .receiver_end = as.Date("2017-01-01"),
                 .receiver_range = 500)
  expect_equal(colnames(a), c("array_id", "receiver_id",
                              "receiver_easting", "receiver_northing",
                              "receiver_start", "receiver_end", "receiver_range"))
  expect_true(all(a$receiver_start == as.Date("2016-01-01")))
  expect_true(all(a$receiver_end == as.Date("2017-01-01")))
  expect_true(all(a$receiver_range == 500))

  #### Test multiple array implementation
  a <- sim_array(.n_receiver = 2, .n_array = 2)
  expect_equal(a$array_id, c(1, 1, 2, 2))
  expect_equal(a$receiver_id, c(1, 2, 1, 2))

  #### Test reproducibility
  set.seed(1)
  a <- sim_array()
  set.seed(1)
  b <- sim_array()
  expect_equal(a, b)

})
