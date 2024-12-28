# set_*() functions are tested via wrapper functions

test_that("set_seed() works", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  # Test that set_seed() works in R
  set_seed(1)
  a <- rnorm(1)
  set_seed(1)
  b <- rnorm(1)
  expect_equal(a, b)

  # Test that set_seed() works with terra e.g., spatSample()
  set_seed(2)
  a <- terra::spatSample(dat_gebco(), size = 100,
                         replace = TRUE, na.rm = TRUE)
  set_seed(2)
  b <- terra::spatSample(dat_gebco(), size = 100,
                         replace = TRUE, na.rm = TRUE)
  expect_equal(a, b)

  # Test that set_seed() works in Julia
  set_seed(3)
  a <- julia_eval('rand()')
  set_seed(3)
  b <- julia_eval('rand()')
  expect_equal(a, b)

  # Test that set_seed() is thread safe in Julia
  code <-
    '
  x = Vector{Float64}(undef, 10)
  Threads.@threads for i in 1:10
    x[i] = rand()
  end
  '
  set_seed()
  julia_code(code)
  a <- julia_eval("x")
  set_seed()
  julia_code(code)
  b <- julia_eval("x")
  expect_equal(a, b)

  # Test that set_seed() works with pf_filter()
  # * TO DO (informally verified)
})

test_that("set_map() works", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  # Specify file path
  f <- system.file("extdata", "dat_gebco.tif", package = "patter", mustWork = TRUE)
  set_map(f)

  # Check file path error handling
  set_map("broken/file/path.tif") |>
    expect_error("File broken/file/path.tif does not exist.")

  # Use SpatRaster on file
  map <- dat_gebco()
  expect_true(terra::sources(map) != "")
  set_map(map)
  expect_true(julia_exists("env"))

  # Use SpatRaster in memory
  map <- terra::unwrap(terra::wrap(map))
  set_map(map)
  expect_true(julia_exists("env"))

})

test_that("Additional Julia set_*() functions work", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = FALSE))

  # set_JULIA_NUM_THREADS()
  JULIA_NUM_THREADS <- Sys.getenv("JULIA_NUM_THREADS")
  set_JULIA_NUM_THREADS(Inf) |>
    expect_warning("There are multiple values for `JULIA_NUM_THREADS`.", fixed = TRUE)
  Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)

  # set_yobs_vect()
  dt1 <- data.table(timestamp = integer(),
                    sensor_id = integer(),
                    obs = numeric())

  set_yobs_vect(.timeline = NULL, .yobs = list(dt1)) |>
    expect_error("Argument '.yobs' must be a named list.")

  set_yobs_vect(.timeline = NULL, .yobs = list(a = dt1)) |>
    expect_error("There are no rows in the dataset.")

  # set_t_resample()
  set_t_resample(NULL)
  expect_true(julia_eval('isnothing(t_resample)'))
  set_t_resample(5)
  expect_equal(5L, julia_eval('t_resample'))
  set_t_resample(c(5, 10, 20))
  expect_equal(c(5L, 10L, 20L), julia_eval('t_resample'))

})
