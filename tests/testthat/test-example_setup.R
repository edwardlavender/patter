test_that("example_setup() methods work", {

  # Test example_setup.default()
  example_setup("unknown_function") |>
    expect_error("An `example_setup()` method is required for unknown_function.",
                 fixed = TRUE)

  # Test example_setup.pf_smoother_two_filter()
  setup <- example_setup("pf_smoother_two_filter")
  map   <- setup$map
  args  <- setup$pf_filter_args
  fwd   <- do.call(pf_filter, args)
  check_inherits(fwd, "list")

})

