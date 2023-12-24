test_that("spatTemplate() works", {
  # Test default options
  r <- spatTemplate()
  expect_equal(dim(r), c(100, 100, 1))
  expect_equal(terra::res(r), c(10, 10))
  expect_equal(
    terra::ext(r)[],
    c(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000)
  )
  # Test updated options
  r <- spatTemplate(.xmin = 0, .xmax = 100,
                    .ymin = 0, .ymax = 100,
                    .res = 1)
  expect_equal(dim(r), c(100, 100, 1))
  expect_equal(terra::res(r), c(1, 1))
  expect_equal(
    terra::ext(r)[],
    c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  )
})
