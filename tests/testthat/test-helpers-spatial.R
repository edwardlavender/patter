test_that("rast_template() works", {
  # Test default options
  r <- rast_template()
  expect_equal(dim(r), c(100, 100, 1))
  expect_equal(terra::res(r), c(10, 10))
  expect_equal(
    terra::ext(r)[],
    c(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000)
  )
  # Test updated options
  r <- rast_template(.xmin = 0, .xmax = 100,
                     .ymin = 0, .ymax = 100,
                     .res = 1)
  expect_equal(dim(r), c(100, 100, 1))
  expect_equal(terra::res(r), c(1, 1))
  expect_equal(
    terra::ext(r)[],
    c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  )
})

test_that("normalise() works", {
  r <- rast_template(.value = 10)
  expect_equal(1,
               terra::global(normalise(r), "sum")[, 1])

})

test_that("dist_along_path() works", {
  xy <- cbind(c(1, 2, 3), c(3, 2, 1))
  expect_equal(
    dist_along_path(xy, .lonlat = FALSE),
    terra::distance(xy, lonlat = FALSE, sequential = TRUE)
  )
})

test_that("geomean() works", {
  # Verify weights
  xy <- cbind(x = c(-179, 179, 177), y = c(12, 14, 16))
  geomean(xy, w = 1) |>
    expect_error("length of weights not correct. It should be: 3", fixed = TRUE)
  # Test that geomean() handles one-row matrices
  expect_equal(cbind(0, 1), geomean(cbind(0, 1)))
  # Otherwise, outputs should match geosphere::geomean()
  xy <- cbind(x = c(-179, 179, 177), y = c(12, 14, 16))
  expect_equal(geosphere::geomean(xy), geomean(xy))
  # Run simulation
  n_runs <- 100
  lapply(seq_len(n_runs), function(i) {
    n <- 100
    xy <- cbind(runif(n, -180, 180), runif(n, -90, 90))
    expect_equal(geosphere::geomean(xy),
                 geomean(xy))
  }) |> invisible()

})
