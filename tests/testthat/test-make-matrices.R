#########################
#########################
#### make_matrix_receivers()

test_that("make_matrix_receivers() works", {

  #### Define inputs
  # Define some example 'moorings' data with receiver IDs and deployment times
  moorings <- data.table(
    receiver_id = c(1, 2, 3, 4, 5),
    receiver_start = as.Date(c(
      "2016-01-01",
      "2016-01-02",
      "2016-01-03",
      "2016-01-04",
      "2016-01-05"
    )),
    receiver_end = as.Date(c(
      "2016-01-06",
      "2016-01-07",
      "2016-01-08",
      "2016-01-09",
      "2016-01-09"
    ))
  )
  # Define some example 'servicing' data
  services <- data.table(
    receiver_id = c(1, 1, 5),
    service_start = as.Date(c(
      "2016-01-02",
      "2016-01-04",
      "2016-01-08"
    )),
    service_end = as.Date(c(
      "2016-01-03",
      "2016-01-05",
      "2016-01-08"
    ))
  )

  #### Test (1): Test basic implementation
  mat <- make_matrix_receivers(moorings,
                               .services = NULL,
                               .delta_t = "days",
                               .start = NULL, .end = NULL,
                               .as_POSIXct = NULL)
  bins <- attr(mat, "bins")
  expect_true(min(bins) == min(moorings$receiver_start))
  expect_true(max(bins) == max(moorings$receiver_end))
  expect_true(all(mat[, 1] == c(1, 1, 1, 1, 1, 1, 0, 0, 0)))
  expect_true(all(mat[, 2] == c(0, 1, 1, 1, 1, 1, 1, 0, 0)))
  expect_true(all(mat[, 3] == c(0, 0, 1, 1, 1, 1, 1, 1, 0)))
  expect_true(all(mat[, 4] == c(0, 0, 0, 1, 1, 1, 1, 1, 1)))
  expect_true(all(mat[, 5] == c(0, 0, 0, 0, 1, 1, 1, 1, 1)))

  #### Test (2): Test exclusion of servicing dates
  mat <- make_matrix_receivers(moorings,
                               .services = services,
                               .delta_t = "days",
                               .start = NULL, .end = NULL,
                               .as_POSIXct = NULL)
  bins <- attr(mat, "bins")
  expect_true(min(bins) == min(moorings$receiver_start))
  expect_true(max(bins) == max(moorings$receiver_end))
  expect_true(all(mat[, 1] == c(1, 0, 0, 0, 0, 1, 0, 0, 0)))
  expect_true(all(mat[, 2] == c(0, 1, 1, 1, 1, 1, 1, 0, 0)))
  expect_true(all(mat[, 3] == c(0, 0, 1, 1, 1, 1, 1, 1, 0)))
  expect_true(all(mat[, 4] == c(0, 0, 0, 1, 1, 1, 1, 1, 1)))
  expect_true(all(mat[, 5] == c(0, 0, 0, 0, 1, 1, 1, 0, 1)))

  #### Test (3): Test implementation with other times
  mat <- make_matrix_receivers(moorings,
                               .services = services,
                               .delta_t = "12 hours",
                               .as_POSIXct = function(x) as.POSIXct(paste(x, "00:00:00"), tz = "UTC"))
  expect_true(all(mat[, 1] == c(1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0)))

  #### Test (4): Modify start & end dates
  mat <-  make_matrix_receivers(moorings,
                                .start = as.Date("2015-01-01"),
                                .end = as.Date("2018-01-01"),
                                .delta_t = "days",
                                .as_POSIXct = NULL)
  bins <- attr(mat, "bins")
  expect_true(min(bins) == as.Date("2015-01-01"))
  expect_true(max(bins) == as.Date("2018-01-01"))

})

