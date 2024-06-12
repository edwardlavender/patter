test_that("spat*() functions work", {

  #### `spatContainsNA()`
  expect_true(spatContainsNA(dat_gebco()))
  expect_false(spatContainsNA(terra::rast(vals = 1)))

  #### `spatIntersect()`
  # Define SpatVectors
  v <-
    cbind(c(706047.3, 706150.8, 706461.1),
          c(6264098, 6263891, 6264098)) |>
    terra::vect(crs = terra::crs(dat_gebco())) |>
    terra::buffer(width = 1000)
  # One polygon
  expect_true(all.equal(v[1], spatIntersect(list(v[1]))))
  # Two polygons
  expect_true(all.equal(
    terra::intersect(v[1], v[2]),
    spatIntersect(list(v[1], v[2]))
  ))
  # Three polygons
  expect_true(all.equal(
    terra::intersect(terra::intersect(v[1], v[2]), v[3]),
    spatIntersect(list(v[1], v[2], v[3]))
  ))

  # spatAllNA()
  expect_false(spatAllNA(dat_gebco()))
  expect_true(spatAllNA(terra::rast(vals = NA)))

  # spatMobilityBox()
  spatMobilityBox(.x = dat_gebco(), .mobility = 750) |>
    expect_null() |>
    expect_warning("`Patter.two_filter_smoother()`'s `box` argument set to `nothing`: `.map` contains NAs.",
                   fixed = TRUE)

  r <- terra::setValues(dat_gebco(), 1)
  bb <- terra::ext(r)
  box0 <- spatMobilityBox(.x = r, .mobility = 750)
  box1 <- c(min_x = bb[1] + 750, max_x = bb[2] - 750, min_y = bb[3] + 750, max_y = bb[4] - 750)
  expect_equal(box0, box1)

})
