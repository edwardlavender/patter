test_that("check_*() functions work", {

  #### check_moorings()
  check_moorings(dat_moorings)
  m <- dat_moorings
  m$receiver_id[1] <- -1
  check_moorings(m) |>
    expect_error("Argument '.moorings$receiver_id' cannot contain receiver IDs <= 0.", fixed = TRUE)
  m <- dat_moorings
  m$receiver_id[1:2] <- 1
  check_moorings(m) |>
    expect_error("Argument '.moorings$receiver_id' contains duplicate elements.", fixed = TRUE)

  #### check_services()
  services <- data.table(receiver_id = dat_moorings$receiver_id[1],
                         service_start = as.Date("2016-01-01"),
                         service_end = as.Date("2017-01-01"))
  check_services(services, dat_moorings)
  services$receiver_id <- "blah"
  check_services(services, dat_moorings) |>
    expect_error("`.services$receiver_id` must be a integer.", fixed = TRUE)

  #### check_acoustics()
  # Check pass
  check_acoustics(dat_acoustics[individual_id == 25, ])
  # Fail on column names
  check_acoustics(data.table(blah = as.POSIXct("2016-01-01", tz = "UTC"), receiver_id = "A")) |>
    expect_error("Argument '.acoustics' does not contain all required names. One or more of the following name(s) are missing: 'timestamp'.", fixed = TRUE)
  # Fail on column types
  check_acoustics(data.table(timestamp = as.POSIXct("2016-01-01", tz = "UTC"), receiver_id = "A"))  |>
    expect_error("`.acoustics$receiver_id` must be a integer.", fixed = TRUE)
  # Fail on NAs
  check_acoustics(data.table(timestamp = as.POSIXct("2016-01-01", tz = "UTC"), receiver_id = NA_integer_)) |>
    expect_error("The acoustic data contains NAs.", fixed = TRUE)
  # Fail on multiple individuals
  check_acoustics(dat_acoustics) |>
    expect_error("Multiple individuals detected in acoustic data.", fixed = TRUE)
  # Fail on time stamp ordering
  check_acoustics(data.table(timestamp = as.POSIXct(c("2016-01-01", "2015-01-01"), tz = "UTC"), receiver_id = 1:2L)) |>
    expect_error("Acoustic time stamps should be ordered chronologically.", fixed = TRUE)
  # Warn on UTC
  check_acoustics(data.table(timestamp = as.POSIXct(c("2016-01-01", "2016-01-02")), receiver_id = 1:2L)) |>
    expect_warning("Argument '.acoustics$timestamp' time zone currently ''; tz forced to UTC.", fixed = TRUE)

  #### check_archival()
  # Check pass
  check_archival(dat_archival[individual_id == 25, ])
  # Fail on column names
  check_archival(data.table(blah = as.POSIXct("2016-01-01", tz = "UTC"), depth = 1)) |>
    expect_error("Argument '.archival' does not contain all required names. One or more of the following name(s) are missing: 'timestamp'.", fixed = TRUE)
  # Fail on column types
  check_archival(data.table(timestamp = as.Date("2016-01-01", tz = "UTC"), depth = 1)) |>
    expect_error("`.archival$timestamp` must be a POSIXct.", fixed = TRUE)
  # Fail on NAs
  check_archival(data.table(timestamp = as.POSIXct("2016-01-01", tz = "UTC"), depth = NA_real_)) |>
    expect_error("The archival data contains NAs.", fixed = TRUE)
  # Fail on multiple individuals
  check_archival(dat_archival) |>
    expect_error("Multiple individuals detected in archival data.", fixed = TRUE)
  # Fail on time stamp ordering
  check_archival(data.table(timestamp = as.POSIXct(c("2016-01-01", "2015-01-01"), tz = "UTC"), depth = c(1, 2.3))) |>
    expect_error("Archival time stamps should be ordered chronologically.", fixed = TRUE)
  check_archival(data.table(timestamp = as.POSIXct(c("2016-01-01", "2016-01-02", "2016-01-04"), tz = "UTC"), depth = c(1, 2.3, 3))) |>
    expect_error("Archival time steps are assumed to be regularly spaced.", fixed = TRUE)
  # Fail on depth values
  check_archival(data.table(timestamp = as.POSIXct("2016-01-01", tz = "UTC"), depth = -1)) |>
    expect_error("Archival depths should be a positive-valued numeric vector and not negative.", fixed = TRUE)

})

