test_that(".pf_check_*() functions work", {

  .pf_check_obs(data.frame(timestep = 1))
  .pf_check_obs(data.table(timestep = 1))
  .pf_check_obs(data.table()) |>
    expect_error("`.obs` should be a data.table with a `timestep` column.",
                 fixed = TRUE)

  folder <- file.path(tempdir(), "pf")
  dir.create(folder)
  .pf_check_write_history(list(sink = folder))
  unlink(folder, recursive = TRUE)

})

test_that(".pf_history_dt() works", {

  # Define particle samples
  # * Use pre-defined samples from pf_forward()
  # * Implement backward pass
  obs        <- dat_obs()
  gebco      <- dat_gebco()
  dat_pff    <- dat_pff()
  con        <- tempdir()
  pfb_folder <- file.path(con, "patter", "pf", "backward")
  dir.create(pfb_folder, recursive = TRUE)
  out_pfb <- pf_backward(dat_pff$history,
                         .save_history = TRUE,
                         .write_history = list(sink = pfb_folder))

  # Implement .pf_history_dt()
  a <- .pf_history_dt(out_pfb)
  b <- .pf_history_dt(out_pfb$history)
  c <- .pf_history_dt(pf_setup_files(pfb_folder))
  d <- .pf_history_dt(pfb_folder)

  # Confirm each implementation option returns identical outputs
  expect_equal(a, b)
  expect_equal(b, c)
  expect_equal(c, d)

})
