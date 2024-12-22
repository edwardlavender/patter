test_that("assemble_timeline() works", {

  # Define data.tables
  dt1 <- data.table(timestamp = as.POSIXct(c("2016-01-01",
                                             "2016-02-01"), tz = "UTC"))
  dt2 <- data.table(timestamp = as.POSIXct(c("2016-01-01 00:02:00",
                                             "2016-02-01 00:00:00",
                                             "2016-04-01 00:00:00"), tz = "UTC"))

  # Test with 2 minutes
  output   <- assemble_timeline(list(dt1, dt2), .step = "2 mins")
  expected <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  as.POSIXct("2016-04-01 00:00:00", tz = "UTC"),
                  "2 mins")
  expect_equal(output, expected)

  # Test with 1 hour
  output   <- assemble_timeline(list(dt1, dt2), .step = "1 hour")
  expected <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  as.POSIXct("2016-04-01 00:00:00", tz = "UTC"), "1 hour")
  expect_equal(output, expected)

  # Test with 1 hour and .trim = TRUE
  output <- assemble_timeline(list(dt1, dt2), .step = "1 hour", .trim = TRUE)
  expected <- seq(as.POSIXct("2016-01-01 00:00:00", tz = "UTC"),
                  as.POSIXct("2016-02-01 00:00:00", tz = "UTC"), "1 hour")
  expect_equal(output, expected)

  # Test check on NAs
  dt1_na <- copy(dt1)
  dt1_na$timestamp[1] <- NA
  assemble_timeline(list(dt1_na), .step = "1 hour") |>
    expect_error("`timestamp` column(s) should not contain NA(s).",
                 fixed = TRUE)

  # Test check on time overlap with .trim = TRUE
  dt1 <- data.table(timestamp = as.POSIXct(c("2016-01-01",
                                             "2016-02-01"), tz = "UTC"))
  dt2 <- data.table(timestamp = as.POSIXct(c("2016-04-01",
                                             "2016-05-01"), tz = "UTC"))
  assemble_timeline(list(dt1, dt2), .step = "1 hour", .trim = TRUE) |>
    expect_error("Dataset timelines do not overlap.")

})

test_that("assemble_acoustics() works", {

  # Define example detections data.table
  detections <- data.table(receiver_id = c(1L, 1L, 2L, 3L),
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
  timeline <- assemble_timeline(list(detections), .step = "2 mins")
  expected <- seq.POSIXt(min(detections$timestamp), max(detections$timestamp), "2 mins")
  expect_equal(timeline, expected)

  # Assemble acoustics, ignoring servicing events
  output   <- assemble_acoustics(.timeline = timeline,
                                 .detections = detections,
                                 .moorings = moorings)
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
                                 .detections = detections,
                                 .moorings = moorings,
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
                                 .detections = detections,
                                 .moorings = moorings,
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

  # Assemble acoustics but when moorings deployments do not overlap with detections
  detections <- data.table(receiver_id = dat_moorings$receiver_id[1:2],
                          timestamp = as.POSIXct(c(
                            "2019-01-01 00:00:00",
                            "2020-01-01 00:00:30"
                          ), tz = "UTC"))
  timeline <- assemble_timeline(list(detections), .step = "2 mins")
  assemble_acoustics(.timeline = timeline,
                     .detections = detections,
                     .moorings = dat_moorings) |>
    expect_error("There are no receiver deployments in `timeline.", fixed = TRUE)

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

  # Mismatches between timeline and timestamps throw an error
  timeline <- seq(as.POSIXct("2020-01-01", tz = "UTC"),
                  as.POSIXct("2020-02-01", tz = "UTC"),
                  by = "2 mins")
  archival <- data.table(timestamp =
                           as.POSIXct(c(
                             "2016-01-01 00:00:00",
                             "2016-01-01 00:04:00"), tz = "UTC"),
                         sensor_id = 1L,
                         obs = 1:2)
  assemble_archival(.timeline = timeline, .archival = archival) |>
    expect_error("There are no archival observations in `timeline`.",
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

test_that("assemble_acoustics_containers() works", {

  #### (1) Test with dummy data

  # Define example timeline
  timeline <- seq(as.POSIXct("2016-01-01 00:00:00", tz = "UTC"),
                  as.POSIXct("2016-01-01 00:08:00", tz = "UTC"),
                  by = "2 mins")

  # Define example acoustics dataset
  acoustics <- data.table(timestamp =
                            as.POSIXct(c(
                              "2016-01-01 00:00:00",
                              "2016-01-01 00:00:00",
                              "2016-01-01 00:00:00",
                              "2016-01-01 00:02:00",
                              "2016-01-01 00:02:00",
                              "2016-01-01 00:02:00",
                              "2016-01-01 00:04:00",
                              "2016-01-01 00:04:00",
                              "2016-01-01 00:04:00",
                              "2016-01-01 00:04:00"),
                              tz = "UTC"),
                          sensor_id = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 4),
                          obs = c(1, 1, 0, 0, 0, 0, 0, 0, 1, 1),
                          receiver_x = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 4),
                          receiver_y = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 4),
                          receiver_alpha = 4,
                          receiver_beta = -0.01,
                          receiver_gamma = 1000)

  # Define containers
  containers <- assemble_acoustics_containers(.timeline = timeline,
                                              .acoustics = acoustics,
                                              .mobility = 500)

  # Validate that assemble_acoustics_containers() returns a named list
  check_inherits(containers, "list")
  check_named_list(containers)
  check_names(containers, req = c("forward", "backward"))

  # Validate forward element is correct:
  # "2016-01-01 00:00:00":
  # > next detection @ 2016-01-01 00:04:00" (receivers 3 & 4)
  # > radius: 500 + 500 + 1000
  # "2016-01-01 00:02:00":
  # > next detection at 2016-01-01 00:04:00" (receivers 3 & 4)
  # > radius: 500 + 1000
  expect_equal(containers$forward,
               data.table(
                 timestamp = as.POSIXct(c("2016-01-01 00:00:00", "2016-01-01 00:00:00",
                                          "2016-01-01 00:02:00", "2016-01-01 00:02:00"),
                                        tz = "UTC"),
                 obs = c(1, 1, 1, 1),
                 sensor_id = c(3, 4, 3, 4),
                 receiver_x = c(3, 4, 3, 4),
                 receiver_y = c(3, 4, 3, 4),
                 radius = c(2000, 2000, 1500, 1500)
               ))

  # Validate backward element is correct:
  # "2016-01-01 00:08:00"
  # > next detection @ 2016-01-01 00:04:00 (receivers 3 & 4)
  # > radius: 500 + 500 + 1000
  # "2016-01-01 00:06:00"
  # > next detection @ 2016-01-01 00:04:00 (receivers 3 & 4)
  # > radius: 500 + 1000
  # "2016-01-01 00:04:00"
  # > next detection @ 2016-01-01 00:00:00 (receivers 1 & 2)
  # > radius: 500 + 500 + 1000
  # "2016-01-01 00:04:00"
  # > next detection  @ 2016-01-01 00:00:00 (receivers 1 & 2)
  # > radius: 500 + 1000
  expect_equal(containers$backward,
               data.table(
                 timestamp = as.POSIXct(c("2016-01-01 00:02:00", "2016-01-01 00:02:00", "2016-01-01 00:04:00",
                                          "2016-01-01 00:04:00", "2016-01-01 00:06:00", "2016-01-01 00:06:00",
                                          "2016-01-01 00:08:00", "2016-01-01 00:08:00"), tz = "UTC"),
                 obs = c(1, 1, 1, 1, 1, 1, 1, 1),
                 sensor_id = c(1, 2, 1, 2, 3, 4, 3, 4),
                 receiver_x = c(1, 2, 1, 2, 3, 4, 3, 4),
                 receiver_y = c(1, 2, 1, 2, 3, 4, 3, 4),
                 radius = c(1500, 1500, 2000, 2000, 1500, 1500, 2000, 2000)
               ))

  #### (2) Test `.map` and `.threshold` arguments
  # TO DO, use patter_run()

  #### (3) Test with real data

  # Define detections
  # detections <- dat_detections[individual_id == 25, ]
  detections <- data.table(
    timestamp = as.POSIXct(c("2016-03-28 18:44:00",
                             "2016-03-28 18:46:00",
                             "2016-03-28 23:00:00",
                             "2016-03-28 23:04:00"), tz = "UTC"),
    receiver_id = c(26, 26, 3, 3)
  )

  # Assemble timeline & acoustics
  timeline   <- assemble_timeline(.datasets = list(detections), .step = "2 mins")
  acoustics  <- assemble_acoustics(.timeline = timeline,
                                   .detections = detections,
                                   .moorings = dat_moorings)

  # Assemble containers
  containers <- assemble_acoustics_containers(.timeline = timeline,
                                              .acoustics = acoustics,
                                              .mobility = 500,
                                              .threshold = 10000)

  # Validate example element ("forward")
  expect_equal(containers$forward,
               data.table(
                 timestamp = as.POSIXct(c("2016-03-28 18:44:00", "2016-03-28 22:24:00", "2016-03-28 22:26:00",
                                          "2016-03-28 22:28:00", "2016-03-28 22:30:00", "2016-03-28 22:32:00",
                                          "2016-03-28 22:34:00", "2016-03-28 22:36:00", "2016-03-28 22:38:00",
                                          "2016-03-28 22:40:00", "2016-03-28 22:42:00", "2016-03-28 22:44:00",
                                          "2016-03-28 22:46:00", "2016-03-28 22:48:00", "2016-03-28 22:50:00",
                                          "2016-03-28 22:52:00", "2016-03-28 22:54:00", "2016-03-28 22:56:00",
                                          "2016-03-28 22:58:00", "2016-03-28 23:00:00", "2016-03-28 23:02:00"),
                                        tz = "UTC"),
                 obs = rep(1, 21),
                 sensor_id = c(26, rep(3, 20)),
                 receiver_x = c(dat_moorings$receiver_x[dat_moorings$receiver_id == 26],
                                rep(dat_moorings$receiver_x[dat_moorings$receiver_id == 3], 20)),
                 receiver_y =  c(dat_moorings$receiver_y[dat_moorings$receiver_id == 26],
                                 rep(dat_moorings$receiver_y[dat_moorings$receiver_id == 3], 20)),
                 radius = c(1250, 9750, 9250, 8750, 8250, 7750, 7250, 6750, 6250, 5750, 5250,
                            4750, 4250, 3750, 3250, 2750, 2250, 1750, 1250, 1750, 1250)
               ))

  # Validate .threshold implementation
  expect_true(all(containers$forward$radius < 10000))
  expect_true(all(containers$backward$radius < 10000))

})
