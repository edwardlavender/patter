test_that("spat*() functions work", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

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

  # spatVmap()
  vmap <- spatVmap(dat_gebco(), .mobility = 750.0)
  vmap_vals <- terra::values(vmap)[, 1]
  expect_true(all(vmap_vals %in% c(FALSE, TRUE)))
  terra::plot(vmap)

})
