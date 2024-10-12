test_that("coa() works", {

  #### Tests with synthetic data

  # Define map
  map <- dat_gebco()

  # Define acoustic detections
  detections <-
    data.table(timestamp = as.POSIXct(c("2016-01-01 00:00:20", "2016-01-01 00:00:23",
                                        "2016-01-01 00:02:40", "2016-01-01 00:02:30"),
                                      tz = "UTC"),
               receiver_id = c(1, 2, 2, 3))

  # Define moorings
  moorings <- data.table(receiver_id = c(1, 2, 3),
                         receiver_x = c(1, 2, 3),
                         receiver_y = c(1, 2, 3))

  # Test 1
  output   <- coa(.map = map, .detections = detections, .moorings = moorings, .delta_t = "10 mins")
  expected <- data.table(timestep = 1,
                         timestamp = floor_date(as.POSIXct("2016-01-01 00:00:20", tz = "UTC"), "10 mins"),
                         map_value = NA_real_,
                         x = weighted.mean(moorings$receiver_x, w = c(1, 2, 1)),
                         y = weighted.mean(moorings$receiver_y, w = c(1, 2, 1))
  )
  expect_equal(output, expected)

  # Test 2
  output   <- coa(.map = map, .detections = detections, .moorings = moorings, .delta_t = "1 minute")
  expected <- data.table(timestep = c(1L, 2L),
                         timestamp = as.POSIXct(c("2016-01-01 00:00:00",
                                                  "2016-01-01 00:02:00"),
                                                tz = "UTC"),
                         map_value = NA_real_,
                         x = c(mean(c(1, 2)), mean(c(2, 3))),
                         y = c(mean(c(1, 2)), mean(c(2, 3)))
  )
  expect_equal(output, expected)

  # Test 3: sim_observations() outputs permitted
  detections <-
    detections |>
    # sensor_id should be permitted
    rename(sensor_id = receiver_id) |>
    # obs = 0L should raise a warning
    mutate(obs = 0L) |>
    as.data.table()
  output <- coa(.map = map,
                .detections = detections, .moorings = moorings,
                .delta_t = "1 minute") |>
    expect_warning("`.detections` contains an `obs` column with '0(s)'.",
                   fixed = TRUE)
  output <- coa(.map = map,
                .detections = detections, .moorings = moorings,
                .delta_t = "1 minute") |>
    suppressWarnings()
  expect_equal(output, expected)

  #### Tests with real data

  # Define acoustic detections
  detections <-
    dat_detections |>
    group_by(individual_id) |>
    # Select a subset of rows for speed
    slice(1:100) |>
    ungroup() |>
    as.data.table()
  detections_1  <- detections[individual_id == individual_id[1], ]

  # Define moorings
  moorings <- dat_moorings

  # Test output columns
  # * Test outputs with .split = NULL
  z <- coa(.map = map,
           .detections = detections_1, .moorings = moorings,
           .delta_t = "2 hours")
  check_inherits(z, "data.table")
  expect_equal(colnames(z), c("timestep", "timestamp", "map_value", "x", "y"))
  # * Test outputs with .split
  z <- coa(.map = map,
           .detections = detections_1, .moorings = moorings,
           .delta_t = "2 hours", .split = "individual_id")
  expect_equal(colnames(z), c("individual_id", "timestep", "timestamp", "map_value", "x", "y"))

  # Check dot handling
  z <- coa(.map = map,
           .detections = detections_1, .moorings = moorings,
           .delta_t = "2 hours",  .split = "individual_id",
           xlab = "x-title")
  z <- coa(.map = map,
           .detections = detections_1, .moorings = moorings,
           .delta_t = "2 hours",
           blah = "x") |>
    expect_warning('"blah" is not a graphical parameter') |> # plot.window()
    expect_warning('"blah" is not a graphical parameter') |> # title()
    expect_warning('"blah" is not a graphical parameter') |> # axis(1, ...)
    expect_warning('"blah" is not a graphical parameter')    # axis(2, ...)

  # Compare coa() outputs & manual calculations for multiple delta_t values
  lapply(c("2 hours", "4 hours"), function(delta_t) {

    # Use coa()
    detections <- dplyr::left_join(detections, moorings, by = "receiver_id")
    output <- coa(.map = map, .detections = detections,
                  .delta_t = delta_t, .split = "individual_id")

    # Re-compute COAs manually
    expected <-
      # Loop over individuals
      lapply(split(detections, detections$individual_id), function(d) {
        # Define time stamps
        d$timestamp <- floor_date(d$timestamp, delta_t)
        # Calculate COAs in each time stamp
        lapply(split(d, d$timestamp), function(.d) {
          # Summarise the frequency of detections at each receiver
          .d <-
            .d |>
            group_by(receiver_id) |>
            mutate(n = n()) |>
            slice(1L) |>
            ungroup()
          # Calculate COAs over all receivers
          .d$x <- weighted.mean(.d$receiver_x, .d$n)
          .d$y <- weighted.mean(.d$receiver_y, .d$n)
          .d[1, , drop = FALSE]
        }) |> rbindlist()
      }) |>
      rbindlist()

    # Confirm rows match
    expect_equal(nrow(output), nrow(expected))

    # Confirm outputs are equal
    expect_equal(output[, .(individual_id, timestamp, x, y)],
                 expected[, .(individual_id, timestamp, x, y)])

  }) |> invisible()

})
