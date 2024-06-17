test_that("check_{dataset}() functions work", {

  expect_true(1 == 1)

  #### check_map()

  expect_null(check_map(NULL))
  check_map(dat_gebco())

  map <- terra::rast(vals = -1, res = c(5, 10))
  check_map(map) |>
    expect_message("`.map`: name updated from 'lyr.1' to 'map_value'.",
                   fixed = TRUE) |>
    expect_message("`.map`: a square `.map` grid resolution is recommended.",
                   fixed = TRUE) |>
    expect_message("`.map`: absolute values are recommended. Use NA to define inhospitable habitats (such as land).", fixed = TRUE)

  #### check_acoustics()

  expect_null(check_acoustics(NULL))
  check_acoustics(dat_acoustics, dat_moorings) |>
    expect_message("`.acoustics`: multiple individuals detected in dataset.") |>
    expect_message("`.acoustics`: time stamps should be ordered chronologically.")

  acc <- copy(dat_acoustics[individual_id == individual_id[1], ])
  acc[, receiver_id := NULL][, receiver_id := 1.1][, na := NA]
  check_acoustics(acc, dat_moorings) |>
    expect_warning("`.acoustics`: not all receivers in `.acoustics` are found in `.moorings`." , fixed = TRUE) |>
    expect_message("`.acoustics`: contains NAs.", fixed = TRUE)

  #### check_moorings()
  expect_null(check_moorings(NULL))
  check_moorings(dat_moorings)

  moorings <- copy(dat_moorings)[1:2, ]
  moorings[, receiver_id := NULL][, receiver_id := c(1.1, 2.2)]
  moorings <- check_moorings(moorings)
  expect_equal(moorings$receiver_id, c(1L, 2L))

  moorings <- copy(dat_moorings)
  moorings[1, receiver_id := -1]
  check_moorings(moorings) |>
    expect_error("`.moorings$receiver_id` cannot contain receiver IDs <= 0.",
                 fixed = TRUE)

  check_moorings(rbind(dat_moorings, dat_moorings)) |>
    expect_error("`.moorings$receiver_id` contains duplicate elements.",
                 fixed = TRUE)

  moorings <- moorings[1, ]
  moorings[, receiver_start := as.POSIXct("2022-01-01", tz = "UTC")]
  check_moorings(moorings) |>
    expect_warning("`.moorings`: some `.moorings$receiver_start` entries are >= `.moorings$receiver_end` entries.",
                   fixed = TRUE)

  check_moorings(dat_moorings[35:36, ], dat_acoustics[1:3, ]) |>
    expect_warning("`.moorings`: the deployment period(s) of some receiver(s) (`52`, `53`) in `.moorings` are entirely outside the range of acoustic observations.",
                   fixed = TRUE)

  moorings <- copy(dat_moorings)[, receiver_alpha := NULL]
  check_moorings(moorings) |>
    expect_message("`.moorings`: the detection probability parameter columns for acoustic observations (i.e., `receiver_alpha`, `receiver_beta` and `receiver_gamma`) expected by the built-in observation model structure for acoustic observations (`ModelObsAcousticLogisTrunc`) are missing from `.moorings`.", fixed = TRUE)

  moorings <- copy(dat_moorings)
  moorings$na <- NA
  check_moorings(moorings) |>
    expect_message("`.moorings`: contains NAs.", fixed = TRUE)


  #### check_services()
  start <- as.POSIXct(c("2016-01-01", "2016-01-01"), tz = "UTC")
  end   <- as.POSIXct(c("2016-01-02", "2016-01-02"), tz = "UTC")
  moorings <- data.table(receiver_id = 1:2,
                         receiver_start = start - 24 * 60 * 60,
                         receiver_end = end + 24 * 60 * 60)
  check_services(data.table(receiver_id = 1:2,
                            service_start = start,
                            service_end = end),
                 moorings)
  check_services(data.table(receiver_id = 3:4,
                            service_start = start,
                            service_end = end),
                 moorings) |>
    expect_warning("`.services`: not all receivers in `.services$receiver_id` are in `.moorings$receiver_id`.", fixed = TRUE)
  check_services(data.table(receiver_id = 1:2,
                            service_start = start,
                            service_end = end,
                            na = NA),
                 moorings) |>
    expect_message("`.services`: contains NAs.", fixed = TRUE)


  #### check_archival()
  expect_null(check_archival(NULL))
  check_archival(dat_archival) |>
    expect_message("`.archival`: multiple individuals detected in dataset.",
                   fixed = TRUE) |>
    expect_message("`.archival`: time stamps should be ordered chronologically.",
                   fixed = TRUE)
  arc <- dat_archival[individual_id == individual_id[1], ]
  arc[1, depth := -1]
  arc[, na := NA]
  check_archival(arc) |>
    expect_message("`.archival`: depths should be a positive-valued numeric vector and not negative.", fixed = TRUE) |>
  expect_message("`.archival`: contains NAs.",
                 fixed = TRUE)

})

