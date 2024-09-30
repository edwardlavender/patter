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

  # set_threads()
  JULIA_NUM_THREADS <- Sys.getenv("JULIA_NUM_THREADS")
  set_threads(Inf) |>
    expect_warning("Restart `R` to update the number of threads in `Julia`.")
  Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)

  # set_datasets()
  dt1 <- data.table(timestamp = integer(),
                    sensor_id = integer(),
                    obs = numeric())

  set_datasets(list(dt1)) |>
    expect_error("There are no rows in the dataset.")

})



