test_that("check_*() functions work", {

  #### check_moorings()
  check_moorings(dat_moorings)
  m <- dat_moorings
  m$receiver_id[1] <- -1
  expect_error(check_moorings(m))
  m <- dat_moorings
  m$receiver_id[1:2] <- 1
  expect_error(check_moorings(m))

  #### check_services()
  services <- data.table(receiver_id = dat_moorings$receiver_id[1],
                         service_start = as.Date("2016-01-01"),
                         service_end = as.Date("2017-01-01"))
  check_services(services, dat_moorings)
  services$receiver_id <- "blah"
  expect_error(check_services(services, dat_moorings))

  #### check_acoustics()
  # Check pass
  check_acoustics(dat_acoustics[individual_id == 25, ])
  # Fail on column names
  expect_error(check_acoustics(data.table(blah = as.POSIXct("2016-01-01", tz = "UTC"), receiver_id = "A")))
  # Fail on column types
  expect_error(check_acoustics(data.table(timestamp = as.POSIXct("2016-01-01", tz = "UTC"), receiver_id = "A")))
  # Fail on NAs
  expect_error(check_acoustics(data.table(timestamp = as.POSIXct("2016-01-01", tz = "UTC"), receiver_id = NA_integer_)))
  # Fail on multiple individuals
  expect_error(check_acoustics(dat_acoustics))
  # Fail on time stamp ordering
  expect_error(check_acoustics(data.table(timestamp = as.POSIXct(c("2016-01-01", "2015-01-01"), tz = "UTC"), receiver_id = 1:2L)))
  # Warn on UTC
  expect_warning(check_acoustics(data.table(timestamp = as.POSIXct(c("2016-01-01", "2016-01-02")), receiver_id = 1:2L)))

  #### check_archival()
  # Check pass
  check_archival(dat_archival[individual_id == 25, ])
  # Fail on column names
  expect_error(check_archival(data.table(blah = as.POSIXct("2016-01-01", tz = "UTC"), depth = 1)))
  # Fail on column types
  expect_error(check_archival(data.table(timestamp = as.Date("2016-01-01", tz = "UTC"), depth = 1)))
  # Fail on NAs
  expect_error(check_archival(data.table(timestamp = as.POSIXct("2016-01-01", tz = "UTC"), depth = NA_real_)))
  # Fail on multiple individuals
  expect_error(check_archival(dat_archival))
  # Fail on time stamp ordering
  expect_error(check_archival(data.table(timestamp = as.POSIXct(c("2016-01-01", "2015-01-01"), tz = "UTC"), depth = c(1, 2.3))))
  expect_error(check_archival(data.table(timestamp = as.POSIXct(c("2016-01-01", "2016-01-02", "2016-01-04"), tz = "UTC"), depth = c(1, 2.3, 3))))
  # Fail on depth values
  expect_error(check_archival(data.table(timestamp = as.POSIXct("2016-01-01", tz = "UTC"), depth = -1)))


  })

