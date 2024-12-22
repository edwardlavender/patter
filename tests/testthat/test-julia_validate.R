test_that("Julia validate works", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = FALSE))

  if (!julia_session()) {
    julia_validate() |>
      expect_error()
  }

  julia_connect()
  julia_validate() |>
    expect_null()

})
