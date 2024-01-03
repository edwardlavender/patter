
test_that("pf_forward() dot handling", {

  require(data.table)
  require(dtplyr)
  require(dplyr, warn.conflicts = FALSE)

  #### Set up forward simulation
  # Select example acoustic & archival datasets
  acc <- dat_acoustics[individual_id == dat_acoustics$individual_id[1], ]
  arc <- dat_archival[individual_id == acc$individual_id[1], ]
  # Setup data list
  dlist <- pat_setup_data(.acoustics = acc,
                          .archival = arc,
                          .moorings = dat_moorings,
                          .bathy = dat_gebco(),
                          .lonlat = FALSE)
  # Setup AC* algorithm layers
  dlist$algorithm$detection_overlaps <- acs_setup_detection_overlaps(dlist)
  dlist$algorithm$detection_kernels  <- acs_setup_detection_kernels(dlist)
  # Set up observations
  obs <- pf_setup_obs(.dlist = dlist,
                      .trim = TRUE,
                      .step = "2 mins",
                      .mobility = 500,
                      .receiver_range = 750)
  # Subset observations for speed
  obs <- obs[1:100L, ]
  # Defline likelihood
  pf_lik_acpf <- list(acs_filter_land = acs_filter_land,
                      acs_filter_container = acs_filter_container,
                      pf_lik_ac = pf_lik_ac)

  #### check_dots_used:
  pf_forward(.obs = obs[1:10, ],
             .dlist = dlist,
             .likelihood = pf_lik_acpf,
             .record = pf_opt_record(.save = TRUE),
             .rargs = list(.blah = 100),
             .verbose = FALSE) |>
    expect_message("Arguments in `...` must be used.")


})
