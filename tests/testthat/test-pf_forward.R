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

  #### Validate .rpropose implementation
  # TO DO
  # Confirm mobility

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
      arrow::read_parquet(file.path(sink, "history", paste0(i, ".parquet")))
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
    d2 <-   arrow::read_parquet(file.path(sink, "history", paste0(i, ".parquet")))
    # Confirm objects are identical
    expect_equal(d1, d2)
    # Confirm column names are defined by cols
    expect_true(all(colnames(d1) %in% cols) & all(cols %in% colnames(d2)))
  })
  unlink(sink, recursive = TRUE)

})
