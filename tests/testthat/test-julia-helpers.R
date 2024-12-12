test_that("Julia helpers work", {

  # Define file.path.norm() helper
  file.path.norm <- function(...) {
    x <- file.path(...)
    normalizePath(x, winslash = "/", mustWork = FALSE)
  }
  expect_true(dir.exists(file.path.norm(tempdir())))

  # Define julia_connect_mimic() helper
  julia_connect_mimic <- function(JULIA_OPTION) {
    julia_option(JULIA_OPTION)
  }

  # Expect timestamps are set properly in Julia
  expect_Dates.DateTime <- function(x) {
    x <- julia_timeline(x) |> suppressWarnings()
    julia_assign("tx", x)
    expect_equal("Dates.DateTime", julia_eval('string(eltype(tx))'))
  }

  #### Test julia_run() and julia_works()
  check_inherits(julia_run(), "logical")
  expect_true(julia_works())

  #### Test julia_option()
  # Use missing
  julia_connect_mimic() |> expect_null()
  # Use function argument
  julia_connect_mimic("auto") |> expect_equal("auto")
  # Use global option
  op <- options(JULIA_OPTION = 1)
  julia_connect_mimic() |> expect_equal(1)
  options(op)
  # Use environment variable
  Sys.setenv(JULIA_OPTION = 2)
  julia_connect_mimic() |> expect_equal("2")
  Sys.unsetenv("JULIA_OPTION")
  # Try conflicting settings
  op <- options(JULIA_OPTION = 1)
  Sys.setenv(JULIA_OPTION = 2)
  julia_connect_mimic() |>
    expect_warning("There are multiple values for `JULIA_OPTION`.")
  options(op)
  Sys.unsetenv("JULIA_OPTION")

  #### julia_proj_path()

  #### julia_proj_generate()
  jproj <- file.path.norm(tempdir(), "JuliaTmp")
  julia_proj_generate(jproj)
  file_cleanup(jproj)

  #### julia_proj_activate()

  #### julia_proj_temp()
  expect_equal(julia_proj_temp(),
               file.path.norm(tempdir(), "Julia"))

  #### julia_packages_dev_Patter.jl()

  #### julia_packages_install()

  #### julia_packages_library()

  #### julia_packages()

  #### julia_threads()
  julia_threads(999) |>
    expect_warning("`JULIA_NUM_THREADS` could not be set.", fixed = TRUE)

  #### julia_*() display
  julia_glimpse(data.frame(x = 1))
  julia_assign("x", 1)
  julia_print("x")
  julia_summary("x")

  #### julia_load()
  file <- tempfile(fileext = ".jld2")
  julia_save(.x = "x", .file = file)
  expect_true(file.exists(file))
  julia_load(.file = file, .x = "x")
  expect_true(julia_exists("x"))
  unlink(file)

  #### julia_timeline()

  # julia_timeline() works with some real data
  x <- dat_detections$timestamp
  expect_identical(x, julia_timeline(x))
  expect_Dates.DateTime(x)

  # julia_timeline() handles a 'problematic' time series created by seq.POSIXt(..., length.out)
  # See: https://github.com/edwardlavender/patter/issues/17
  x <- seq.POSIXt(as.POSIXct("2016-01-01 12:00:00", tz = "UTC"), by = "2 mins", length.out = 10)
  t1 <-
    julia_timeline(x) |>
    expect_warning("Use `seq.POSIXt()` with `from`, `to` and `by` rather than `length.out` for faster handling of time stamps.", fixed = TRUE)
  expect_Dates.DateTime(x)

  # julia_timeline() handling of problematic time series works for tz = "GMT"
  # > GMT and UTC trigger one warning (fasttime used)
  x <- seq.POSIXt(as.POSIXct("2016-01-01 12:00:00", tz = "GMT"), by = "2 mins", length.out = 10)
  julia_timeline(x) |>
    expect_warning("Use `seq.POSIXt()` with `from`, `to` and `by` rather than `length.out` for faster handling of time stamps.", fixed = TRUE)

  # julia_timeline() handling of problematic time series works for other time zones
  # > This triggers a second warning (fasttime not used: slower)
  x <- seq.POSIXt(as.POSIXct("2016-01-01 12:00:00", tz = "US/Eastern"), by = "2 mins", length.out = 10)
  julia_timeline(x) |>
    expect_warning("Use `seq.POSIXt()` with `from`, `to` and `by` rather than `length.out` for faster handling of time stamps.", fixed = TRUE) |>
    expect_warning("Use `fasttime` for faster formatting of time stamps.", fixed = TRUE)
  expect_Dates.DateTime(x)

  # julia_timeline() handles other sequences correctly, without warnings
  x <- seq.POSIXt(as.POSIXct("2016-01-01 12:00:00", tz = "UTC"), as.POSIXct("2016-02-01", tz = "UTC"), by = "2 mins")
  expect_Dates.DateTime(x)

  #### julia_check_exists()
  julia_check_exists("x")
  julia_check_exists("blah") |>
    expect_error("'blah' does not exist in Julia", fixed = TRUE)

  #### julia_code()
  julia_code(
    '
  x = 1
  y = 2
  z = x + y
  '
  )
  expect_identical(julia_eval("z"), 3L)

})

