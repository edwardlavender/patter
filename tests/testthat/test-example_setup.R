test_that("example_setup() methods work", {

  # Test example_setup.default()
  example_setup("unknown_function") |>
    expect_error("An `example_setup()` method is required for unknown_function.",
                 fixed = TRUE)

  # Test example_setup.pf_smoother_two_filter()
  pff_args <- example_setup("pf_smoother_two_filter")
  out_pff  <- do.call(pf_filter, pff_args)
  check_inherits(out_pff, "list")

})

