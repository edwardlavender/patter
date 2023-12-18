test_that(".pf_check_*() functions work", {

  .pf_check_obs(data.frame(timestep = 1))
  .pf_check_obs(data.table(timestep = 1))
  .pf_check_obs(data.table()) |>
    expect_error("`.obs` should be a data.table with a `timestep` column.",
                 fixed = TRUE)

})

test_that(".pf_history_dt() works", {

  require(arrow)

  # Define particle samples
  # * Use pre-defined samples from pf_forward()
  # * Implement backward pass
  obs        <- dat_obs()
  gebco      <- dat_gebco()
  dat_pff    <- dat_pff()
  con        <- tempdir()
  pfb_folder <- file.path(con, "patter", "pf", "backward")
  dir.create(pfb_folder, recursive = TRUE)
  out_pfb <- pf_backward_killer(dat_pff$history,
                                .save_history = TRUE,
                                .write_history = list(sink = pfb_folder))

  # Implement .pf_history_dt()
  a <- .pf_history_dt(out_pfb)
  b <- .pf_history_dt(out_pfb$history)
  c <- .pf_history_dt(pf_setup_files(pfb_folder))
  d <- .pf_history_dt(pfb_folder,
                      schema = schema(
                        timestep = int32(),
                        cell_past = int32(),
                        x_past = double(),
                        y_past = double(),
                        cell_now = int32(),
                        x_now = double(),
                        y_now = double(),
                        lik = double(),
                        dens = double(),
                        weight = double()
                      ))

  # Confirm each implementation option returns identical outputs
  expect_equal(a, b)
  expect_equal(b, c)
  expect_equal(c, d)

})
