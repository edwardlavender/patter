test_that("get_hr_*() functions work", {

  # Define hypothetical input SpatRaster
  require(terra)
  r <- rast()
  n <- ncell(r)
  i <- 2e4
  r[i] <- 1
  r <- distance(r)
  r <- r / global(r, "sum")[1, 1]
  terra::plot(r)

  # Check error handling
  get_hr_prop(r, .prop = c(0.2, 0.3)) |>
    expect_error("`.prop` should be a single number (proportion).",
                 fixed = TRUE)

  get_hr_prop(r, .prop = 0) |>
    expect_error("`.prop` equals zero.", fixed = TRUE)

  get_hr_prop(r, .prop = 10) |>
    expect_error("`.prop` should be a proportion (between zero and one).",
                 fixed = TRUE)

  # Check outputs match `spatialEco::raster.vol()`
  all.equal(
    get_hr_prop(r, .prop = 0.2, .add = TRUE),
    spatialEco::raster.vol(r, 0.2),
  ) |> expect_true()
  all.equal(
    get_hr_prop(r, .prop = 0.8, .add = TRUE),
    spatialEco::raster.vol(r, 0.8)
  ) |> expect_true()
  all.equal(
    get_hr_full(r, .add = TRUE),
    spatialEco::raster.vol(r, 1)
  ) |> expect_true()
  all.equal(
    get_hr_home(r, .add = TRUE),
    spatialEco::raster.vol(r, 0.95)
  ) |> expect_true()
  all.equal(
    get_hr_core(r, .add = TRUE),
    spatialEco::raster.vol(r, 0.5)
  ) |> expect_true()

})

