test_that("pf_backward_sampler_v() works", {

  # TO DO

})

test_that("pf_backward_sampler_p() works", {

  #### Run backward sampler
  # Define default arguments
  obs     <- dat_obs()
  dlist   <- dat_dlist()
  out_pff <- dat_pff()
  # Run sampler
  ssf()
  out_pfbs <- pf_backward_sampler_p(.history = dat_pff(),
                                    .obs = obs,
                                    .dlist = dlist,
                                    .record = pf_opt_record(.save = TRUE))

  #### Basic validation
  # Check named list (pf_particles-class) object
  check_inherits(out_pfbs, "list")
  check_named_list(out_pfbs)
  expect_true(all(c("history", "path", "time") %in% names(out_pfbs)))
  expect_s3_class(out_pfbs, pf_class)
  # The `path` element is a data.table with T * N rows
  check_inherits(out_pfbs$path, "data.table")
  expect_true(
    nrow(out_pfbs$path) == (length(out_pff$history) * nrow(out_pff$history[[1]]))
  )

  #### Validate location pairs
  # cell_now [t - 1] = cell_past [t]
  path <- out_pfbs$path
  val <-
    path |>
    group_by(path_id) |>
    mutate(cell_match = lag(cell_now) == cell_past,
           x_match = lag(x_now) == x_past,
           y_match = lag(y_now) == y_past)
  expect_true(all(val$cell_match, na.rm = TRUE))
  expect_true(all(val$x_match, na.rm = TRUE))
  expect_true(all(val$y_match, na.rm = TRUE))

  #### Validate densities (pairwise)
  val <-
    path |>
    group_by(path_id) |>
    filter(timestep > 1L) |>
    mutate(len = clen(cbind(x_now, y_now), cbind(x_past, y_past), .lonlat = FALSE),
           dens2 = dtruncgamma(len))
  expect_equal(val$dens, val$dens2)

  # Validate densities (sequentially)
  val <-
    path |>
    group_by(path_id) |>
    mutate(len = dist_along_path(cbind(x_now, y_now)),
           len = lag(len)) |>
    filter(timestep > 1L) |>
    mutate(dens2 = dtruncgamma(len))
  expect_equal(val$dens, val$dens2)

})
