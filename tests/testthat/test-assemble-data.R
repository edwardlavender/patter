test_that("assemble_timeline() works", {

  dt1 <- data.table(timestamp = as.POSIXct(c("2016-01-01",
                                             "2016-02-01")))
  dt2 <- data.table(timestamp = as.POSIXct(c("2016-01-01 00:02:00",
                                             "2016-02-01 00:00:00",
                                             "2016-04-01 00:00:00")))

  output <- assemble_timeline(list(dt1, dt2), .step = "2 mins")
  expected <- seq(as.POSIXct("2016-01-01"), as.POSIXct("2016-04-01 00:00:00"), "2 mins")
  expect_equal(output, expected)

  output <- assemble_timeline(list(dt1, dt2), .step = "1 hour")
  expected <- seq(as.POSIXct("2016-01-01"), as.POSIXct("2016-04-01 00:00:00"), "1 hour")
  expect_equal(output, expected)

  output <- assemble_timeline(list(dt1, dt2), .step = "1 hour", .trim = TRUE)
  expected <- seq(as.POSIXct("2016-01-01 00:00:00"), as.POSIXct("2016-02-01 00:00:00"), "1 hour")
  expect_equal(output, expected)

})

test_that("assemble_acoustics() works", {

  # Define example acoustics data.table
  acoustics <- data.table(receiver_id = c(1L, 1L, 2L, 3L),
                          timestamp = as.POSIXct(c(
                            # Two overlapping detections at one receiver
                            "2016-01-01 00:00:00",
                            "2016-01-01 00:00:30",
                            # Another detection at a different receiver at approx. same time
                            "2016-01-01 00:00:38",
                            # A future detection at another receiver
                            "2016-01-01 00:04:00"
                          ), tz = "UTC"))

  # Define moorings
  moorings <- data.table(receiver_id = c(1L, 2L, 3L),
                         receiver_start = as.POSIXct("2016-01-01", tz = "UTC"),
                         receiver_end = as.POSIXct("2017-01-01", tz = "UTC"))

  # Define services
  services <- data.table(receiver_id = c(1, 2),
                         service_start = as.POSIXct("2016-01-01 00:02:00", tz = "UTC"),
                         service_end = as.POSIXct("2016-01-01 00:04:00", tz = "UTC"))

  # Define timeline
  timeline <- assemble_timeline(list(acoustics), .step = "2 mins")
  expected <- seq.POSIXt(min(acoustics$timestamp), max(acoustics$timestamp), "2 mins")
  expect_equal(timeline, expected)

  # Assemble acoustics, ignoring servicing events
  output   <- assemble_acoustics(.timeline = timeline,
                                 .acoustics = acoustics, .moorings = moorings)
  expected <- data.table(timestamp =
                           as.POSIXct(c(
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:02:00",
                             "2016-01-01 00:02:00",
                             "2016-01-01 00:02:00",
                             "2016-01-01 00:04:00",
                             "2016-01-01 00:04:00",
                             "2016-01-01 00:04:00"),
                             tz = "UTC"),
                         sensor_id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
                         obs = c(1, 1, 0, 0, 0, 0, 0, 0, 1)
  )


  # Assemble acoustics, accounting for servicing events
  expect_equal(output, expected)
  output   <- assemble_acoustics(.timeline = timeline,
                                 .acoustics = acoustics, .moorings = moorings,
                                 .services = services)
  expected <- data.table(timestamp =
                           as.POSIXct(c(
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:02:00",
                             "2016-01-01 00:04:00"),
                             tz = "UTC"),
                         sensor_id = c(1, 2, 3, 3, 3),
                         obs = c(1, 1, 0, 0, 1)
  )

  expect_equal(output, expected)

  # Assemble acoustics, including ancillary parameter columns in `.moorings`
  moorings[, receiver_x := 1:3]
  moorings[, receiver_y := 4:6]
  output   <- assemble_acoustics(.timeline = timeline,
                                 .acoustics = acoustics, .moorings = moorings,
                                 .services = services)
  expected <- data.table(timestamp =
                           as.POSIXct(c(
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:02:00",
                             "2016-01-01 00:04:00"),
                             tz = "UTC"),
                         sensor_id = c(1, 2, 3, 3, 3),
                         obs = c(1, 1, 0, 0, 1),
                         receiver_x = c(1, 2, 3, 3, 3),
                         receiver_y = c(4, 5, 6, 6, 6)
  )
  expect_equal(output, expected)

})

test_that("assemble_archival() works", {

  # Multiple archival observations in the same step throw a warning

  archival <- data.table(timestamp =
                           as.POSIXct(c(
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:04:00"), tz = "UTC"),
                         sensor_id = 1L,
                         obs = 1:3)

  timeline <- assemble_timeline(list(archival), "2 mins")

  assemble_archival(.timeline = timeline, .archival = archival) |>
    expect_warning("There are multiple archival observations in one or more time steps.",
                   fixed = TRUE)

  # A correct implementation
  # (sensor_id may be missing)

  archival <- data.table(timestamp =
                           as.POSIXct(c(
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:02:00",
                             "2016-01-01 00:04:00"), tz = "UTC"),
                         obs = 1:3)

  timeline <- assemble_timeline(list(archival), "2 mins")

  output   <- assemble_archival(.timeline = timeline, .archival = archival)
  expected <- data.table(timestamp =
                           as.POSIXct(c(
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:02:00",
                             "2016-01-01 00:04:00"), tz = "UTC"),
                         sensor_id = 1L,
                         obs = 1:3L)
  expect_equal(output, expected)

})





