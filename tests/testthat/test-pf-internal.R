test_that(".pf_history_dt() works", {

  # Define inputs
  pff_folder <- dat_pff_src()
  out_pff    <- dat_pff()

  # Implement .pf_history_dt()
  a <- .pf_history_dt(out_pff)
  b <- .pf_history_dt(out_pff$history)
  c <- .pf_history_dt(pf_files(pff_folder))
  d <- .pf_history_dt(pff_folder,
                      schema = schema(
                        timestep = int32(),
                        cell_past = int32(),
                        cell_now = int32(),
                        x_now = double(),
                        y_now = double(),
                        loglik = double(),
                        logwt = double()
                      ))

  # Confirm each implementation option returns identical outputs
  expect_equal(a, b)
  expect_equal(b, c)
  expect_equal(c, d)

})
