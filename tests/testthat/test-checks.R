test_that("check_*() functions work", {

  #### check_inherits()
  check_inherits(iris, "data.frame")
  expect_error(check_inherits(iris, "data.table"))

  #### check_names()
  check_names(iris, req = c("Species", "Sepal.Width"))
  check_names(iris, req = c("Species", "blah"), type = any)
  expect_error(check_names(iris, req = c("Species", "blah"), type = all))

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
  check_acoustics(dat_acoustics)

  # check_archival()
  check_archival(dat_archival)


  })

