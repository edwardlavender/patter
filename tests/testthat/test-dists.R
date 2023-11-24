test_that("dist_along_path() works", {
  xy <- cbind(c(1, 2, 3), c(3, 2, 1))
  expect_equal(
    dist_along_path(xy, .lonlat = FALSE),
    terra::distance(xy, lonlat = FALSE, sequential = TRUE)
  )
})
