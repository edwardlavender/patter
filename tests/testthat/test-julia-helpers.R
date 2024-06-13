test_that("Julia helpers work", {

  check_inherits(julia_run(), "logical")

  check_inherits(julia_skip(), "logical")

  expect_true(julia_works())

  # julia_proj_path()

  # julia_proj_generate()

  # julia_proj_activate()

  # julia_proj_temp()
  expect_equal(julia_proj_temp(),
               file.path(tempdir(), "Julia"))

  # julia_packages_dev_Patter.jl()

  # julia_packages_install()

  # julia_packages_library()

  # julia_packages()

  # julia_threads()
  julia_threads(.threads = 999) |>
    expect_warning("`JULIA_NUM_THREADS` could not be set via `.threads`.", fixed = TRUE)

  julia_glimpse(data.frame(x = 1))

  julia_assign("x", 1)
  julia_print("x")
  julia_summary("x")

  file <- tempfile(fileext = ".jld2")
  julia_save(.x = "x", .file = file)
  expect_true(file.exists(file))
  julia_load(.file = file, .x = "x")
  expect_true(julia_exists("x"))

  input <- dat_acoustics$timestamp
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

