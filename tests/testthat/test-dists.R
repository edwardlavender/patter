test_that("dist_along_path() works", {

  #### Example (1):
  # Define example matrix
  xy <- matrix(c(0, 0,
                 1, 1,
                 2, 2), ncol = 2L)
  # terra::distance() starts with 0
  expect_equal(
    c(0, 1, 1),
    terra::distance(xy, lonlat = FALSE, sequential = TRUE)
  )
  # dist_along_path() ends with NA to match Tools4ETS::serial_difference() behaviour
  expect_equal(
    dist_along_path(xy, .lonlat = FALSE),
    c(1, 1, NA_real_)
  )

  #### Example (2):
  xy <- cbind(c(1, 2, 3), c(3, 2, 1))
  expect_equal(
    dist_along_path(xy, .lonlat = FALSE),
    c(terra::distance(xy, lonlat = FALSE, sequential = TRUE)[2:nrow(xy)], NA_real_)
  )

})
