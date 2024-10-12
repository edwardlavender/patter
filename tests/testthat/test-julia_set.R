# set_*() functions are tested via wrapper functions

test_that("set_map() works", {

  expect_true(1 == 1)

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
  terra:::readAll(map)
  set_map(map)
  expect_true(julia_exists("env"))

})

test_that("Additional Julia set_*() functions work", {

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

})



