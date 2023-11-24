test_that("spatIsEmpty works", {
  spatIsEmpty(spatTemplate()) |> expect_false()
  r <- spatTemplate()
  r[1] <- NA
  spatIsEmpty(r)  |> expect_false()
  spatIsEmpty(spatTemplate(.value = NA))  |> expect_true()
  spatIsEmpty(terra::vect(cbind(1, 2))) |> expect_false()
  spatIsEmpty(terra::vect()) |> expect_true()
})
