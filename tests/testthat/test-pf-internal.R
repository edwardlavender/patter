test_that(".pf_history_dt() works", {

  # Define inputs
  pfbk_folder <- dat_pfbk_src()
  out_pfbk    <- dat_pfbk()

  # Implement .pf_history_dt()
  a <- .pf_history_dt(out_pfbk)
  b <- .pf_history_dt(out_pfbk$history)
  c <- .pf_history_dt(pf_files(pfbk_folder))
  d <- .pf_history_dt(pfbk_folder,
                      schema = schema(
                        timestep = int32(),
                        cell_past = int32(),
                        cell_now = int32(),
                        x_now = double(),
                        y_now = double(),
                        lik = double(),
                        weight = double()
                      ))

  # Confirm each implementation option returns identical outputs
  expect_equal(a, b)
  expect_equal(b, c)
  expect_equal(c, d)

})
