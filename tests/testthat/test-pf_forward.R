test_that("pf_forward() works", {

  require(data.table)
  require(dtplyr)
  require(dplyr, warn.conflicts = FALSE)

  #### Set up forward simulation
  dlist <- dat_dlist()
  dlist$algorithm$detection_overlaps <- acs_setup_detection_overlaps(dlist)
  dlist$algorithm$detection_kernels  <- acs_setup_detection_kernels(dlist)
  obs <- dat_obs()
  pf_lik_acpf <- list(acs_filter_land = acs_filter_land,
                      acs_filter_container = acs_filter_container,
                      pf_lik_ac = pf_lik_ac)

  #### Validate function checks
  # check_dots_used:
  pf_forward(.obs = obs[1:10, ],
             .dlist = dlist,
             .likelihood = pf_lik_acpf,
             .record = pf_opt_record(.save = TRUE),
             .rargs = list(.blah = 100),
             .verbose = FALSE) |>
    expect_message("Arguments in `...` must be used.")

  #### Validate output object
  out_pff <- pf_forward(.obs = obs,
                        .dlist = dlist,
                        .likelihood = pf_lik_acpf,
                        .trial = pf_opt_trial(.trial_sampler = 0L),
                        .record = pf_opt_record(.save = TRUE))
  check_inherits(out_pff, pf_class)
  elements <- c("history", "convergence", "time")
  expect_true(all(names(out_pff) %in% elements & elements %in% names(out_pff)))
  check_inherits(out_pff$history[[1]], "data.table")
  expect_true(out_pff$convergence)
  check_inherits(out_pff$time, "data.table")
  expect_true(all(names(out_pff$time[[1]]) %in% c("start", "end", "duration")))

  #### Validate .rpropose implementation
  # Validate pairwise distances are < mobility (500 m by default)
  out_pff_h <-
    out_pff$history |>
    rbindlist(fill = TRUE)
  dists <-
    out_pff_h |>
    filter(!is.na(x_past)) |>
    select(x_past, y_past, x_now, y_now) |>
    mutate(dist = clen(cbind(x_past, y_past), cbind(x_now, y_now), .lonlat = FALSE)) |>
    pull(dist)
  expect_true(all(dists < obs$mobility[1]))
  # Validate cell_now locations on the grid
  expect_equal(out_pff_h$cell_now,
               terra::cellFromXY(dlist$spatial$bathy, cbind(out_pff_h$x_now, out_pff_h$y_now)))
  expect_true(all(
    clen(
      cbind(out_pff_h$x_now, out_pff_h$y_now),
      terra::xyFromCell(dlist$spatial$bathy, out_pff_h$cell_now),
      .lonlat = FALSE
    ) < terra::res(dlist$spatial$bathy)[1]))
  # Validate cell_past locations on the grid
  out_pff_h_p <- out_pff_h[!is.na(cell_past), ]
  expect_equal(out_pff_h_p$cell_past,
               terra::cellFromXY(dlist$spatial$bathy, cbind(out_pff_h_p$x_past, out_pff_h_p$y_past)))
  expect_true(all(
    clen(
      cbind(out_pff_h_p$x_past, out_pff_h_p$y_past),
      terra::xyFromCell(dlist$spatial$bathy, out_pff_h_p$cell_past),
      .lonlat = FALSE
    ) < terra::res(dlist$spatial$bathy)[1]))

  #### Validate .dpropose implementation
  # Run simulation
  out_pff <- pf_forward(.obs = obs,
                        .dlist = dlist,
                        .likelihood = pf_lik_acpf,
                        .trial = pf_opt_trial(.trial_kick = 0L),
                        .record = pf_opt_record(.save = TRUE))
  # Validate distances
  out_pff_h <-
    out_pff$history |>
    rbindlist(fill = TRUE)
  dists <-
    out_pff_h |>
    filter(!is.na(x_past)) |>
    select(x_past, y_past, x_now, y_now) |>
    mutate(dist = clen(cbind(x_past, y_past), cbind(x_now, y_now), .lonlat = FALSE)) |>
    pull(dist)
  expect_true(all(dists < obs$mobility[1]))

  #### Validate likelihoods, densities and weights
  # TO DO

  #### Validate sampling
  # Validate .n
  out_pff <- pf_forward(.obs = obs,
                        .dlist = dlist,
                        .n = 50L,
                        .likelihood = pf_lik_acpf,
                        .record = pf_opt_record(.save = TRUE))
  expect_true(nrow(out_pff$history[[1]]) == 50L)
  # Validate resampling
  # * TO DO

  #### Validate .rerun
  out_pff <- pf_forward(.obs = obs,
                        .dlist = dlist,
                        .n = 50L,
                        .likelihood = pf_lik_acpf,
                        .rerun = out_pff,
                        .rerun_from = 2L,
                        .record = pf_opt_record(.save = TRUE))
  expect_length(out_pff$history, nrow(obs))
  expect_true(nrow(out_pff$time) == 2L)

  #### Validate .record implementation
  # Validate .save = FALSE
  sink <- file.path(tempdir(), "patter")
  dir.create(sink)
  out_pff <- pf_forward(.obs = obs,
                        .dlist = dlist,
                        .likelihood = pf_lik_acpf,
                        .record = pf_opt_record(.save = FALSE, .sink = sink))
  expect_true(length(out_pff$history) == 0L)
  unlink(sink, recursive = TRUE)
  # Validate .save = TRUE & .sink
  sink <- file.path(tempdir(), "patter")
  dir.create(sink)
  out_pff <- pf_forward(.obs = obs,
                        .dlist = dlist,
                        .likelihood = pf_lik_acpf,
                        .record = pf_opt_record(.save = TRUE, .sink = sink))
  cl_lapply(seq_len(nrow(obs)), function(i) {
    expect_equal(
      out_pff$history[[i]],
      arrow::read_parquet(file.path(sink, paste0(i, ".parquet")))
    )
  })
  unlink(sink, recursive = TRUE)
  # Validate handling of `.cols`
  sink <- file.path(tempdir(), "patter")
  dir.create(sink)
  cols <- c("timestep", "cell_now", "cell_past")
  out_pff <- pf_forward(.obs = obs,
                        .dlist = dlist,
                        .likelihood = pf_lik_acpf,
                        .record = pf_opt_record(.save = TRUE,
                                                .sink = sink,
                                                .cols = cols))
  cl_lapply(seq_len(nrow(obs)), function(i) {
    d1 <-  out_pff$history[[i]]
    d2 <-   arrow::read_parquet(file.path(sink, paste0(i, ".parquet")))
    # Confirm objects are identical
    expect_equal(d1, d2)
    # Confirm column names are defined by cols
    expect_true(all(colnames(d1) %in% cols) & all(cols %in% colnames(d2)))
  })
  unlink(sink, recursive = TRUE)

})
