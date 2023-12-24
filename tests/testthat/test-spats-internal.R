# spatContainsNA()


# spatSampleDT()


# spatIntersect()


# spatIntersect.SpatRaster()


# spatIntersect.SpatVector()


# spatIsEmpty()
test_that("spatIsEmpty works", {
  spatIsEmpty(spatTemplate()) |> expect_false()
  r <- spatTemplate()
  r[1] <- NA
  spatIsEmpty(r)  |> expect_false()
  spatIsEmpty(spatTemplate(.value = NA))  |> expect_true()
  spatIsEmpty(terra::vect(cbind(1, 2))) |> expect_false()
  spatIsEmpty(terra::vect()) |> expect_true()
})

# spatNormalise()
test_that("spatNormalise() works", {
  r <- spatTemplate(.value = 10)
  expect_equal(1,
               terra::global(spatNormalise(r), "sum")[, 1])

})
