test_that(".coa_check_*() functions works", {

  #### Test .coa_check_acoustics()
  # Check the function works
  .coa_check_acoustics(as.data.frame(dat_acoustics), .split = NULL) |>
    check_inherits("data.table")
  .coa_check_acoustics(dat_acoustics, .split = NULL) |>
    check_inherits("data.table")
  .coa_check_acoustics(dat_acoustics, .split = "individual_id") |>
    check_inherits("data.table")

  # Check missing names
  acc <- dat_acoustics
  acc$timestamp <- NULL
  .coa_check_acoustics(acc, .split = NULL) |>
    expect_error("'.acoustics' does not contain all required names. One or more of the following name(s) are missing: 'timestamp'.", fixed = TRUE)
  .coa_check_acoustics(dat_acoustics, .split = "blah") |>
    expect_error("'.acoustics' does not contain all required names. One or more of the following name(s) are missing: 'blah'.", fixed = TRUE)

})
