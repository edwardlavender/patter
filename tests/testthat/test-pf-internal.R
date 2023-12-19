test_that(".pf_check_*() functions work", {

  .pf_check_obs(data.frame(timestep = 1))
  .pf_check_obs(data.table(timestep = 1))
  .pf_check_obs(data.table()) |>
    expect_error("`.obs` should be a data.table with a `timestep` column.",
                 fixed = TRUE)

})

test_that(".pf_history_dt() works", {

  # Define inputs
  pfbk_folder <- system.file("extdata", "acpf", "backward", "killer",
                             package = "patter", mustWork = TRUE)
  out_pfbk    <- dat_pfbk()

  # Implement .pf_history_dt()
  a <- .pf_history_dt(out_pfbk)
  b <- .pf_history_dt(out_pfbk$history)
  c <- .pf_history_dt(pf_files(pfbk_folder))
  d <- .pf_history_dt(pfbk_folder,
                      schema = schema(
                        timestep = int32(),
                        cell_past = int32(),
                        x_past = double(),
                        y_past = double(),
                        cell_now = int32(),
                        x_now = double(),
                        y_now = double(),
                        lik = double(),
                        weight = double(),
                        bathy = double(),
                        dens = double()
                      ))

  # Confirm each implementation option returns identical outputs
  expect_equal(a, b)
  expect_equal(b, c)
  expect_equal(c, d)

})
