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

  # Test julia_run() and julia_works()
  check_inherits(julia_run(), "logical")
  expect_true(julia_works())

  # Test julia_option()
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

  # julia_proj_path()

  # julia_proj_generate()
  jproj <- file.path.norm(tempdir(), "JuliaTmp")
  julia_proj_generate(jproj)
  file_cleanup(jproj)

  # julia_proj_activate()

  # julia_proj_temp()
  expect_equal(julia_proj_temp(),
               file.path.norm(tempdir(), "Julia"))

  # julia_packages_dev_Patter.jl()

  # julia_packages_install()

  # julia_packages_library()

  # julia_packages()

  # julia_threads()
  julia_threads(999) |>
    expect_warning("`JULIA_NUM_THREADS` could not be set.", fixed = TRUE)

  julia_glimpse(data.frame(x = 1))

  julia_assign("x", 1)
  julia_print("x")
  julia_summary("x")

  file <- tempfile(fileext = ".jld2")
  julia_save(.x = "x", .file = file)
  expect_true(file.exists(file))
  julia_load(.file = file, .x = "x")
  expect_true(julia_exists("x"))

  input <- dat_detections$timestamp
  output <- julia_timeline(input)
  expect_identical(input, output)

  julia_check_exists("x")
  julia_check_exists("blah") |>
    expect_error("'blah' does not exist in Julia", fixed = TRUE)

  julia_code(
    '
  x = 1
  y = 2
  z = x + y
  '
  )
  expect_identical(julia_eval("z"), 3L)

})

