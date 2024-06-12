test_that("check_{dataset}() functions work", {

  expect_true(1 == 1)

  check_map(dat_gebco())

  check_acoustics(dat_acoustics, dat_moorings)

  check_moorings(dat_moorings)

  start <- as.POSIXct(c("2016-01-01", "2016-01-01"), tz = "UTC")
  end   <- as.POSIXct(c("2016-01-02", "2016-01-02"), tz = "UTC")
  check_services(data.table(receiver_id = 1:2,
                            service_start = start,
                            service_end = end),
                 data.table(receiver_id = 1:2,
                            receiver_start = start - 24 * 60 * 60,
                            receiver_end = end + 24 * 60 * 60))

  check_archival(dat_archival)


})

