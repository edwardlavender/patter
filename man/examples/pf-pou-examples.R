#### Set up examples

# (A) Define input datasets
acoustics <- dat_acoustics[individual_id == 25, ]
archival <- dat_archival[individual_id == 25, ]
obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
obs <- obs[1:200, ]
gebco <- dat_gebco()

# (B) Implement AC* algorithm
dat_moorings$receiver_range <- 500
containers <- acs_setup_detection_containers(gebco, dat_moorings)
overlaps <- acs_setup_detection_overlaps(containers, dat_moorings)
kernels <-
  acs_setup_detection_kernels(dat_moorings,
                              .calc_detection_pr = acs_setup_detection_pr,
                              .bathy = gebco)
ac_folder <- file.path(tempdir(), "ac")
dir.create(ac_folder)
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE)

# (C) Implement forward simulation
forward_folder <- file.path(tempdir(), "pf", "forward")
dir.create(forward_folder, recursive = TRUE)
out_pf <- pf_forward(.obs = obs,
                     .record = out_ac$record,
                     .n = 1e3,
                     .kick = pf_kick,
                     .bathy = gebco,
                     .save_history = TRUE,
                     .write_history = list(sink = forward_folder))

# (D) Implement backward pass
# * TO DO

#### Example (1): Calculate POU from pf object
pou_1 <- pf_pou(.history = out_pf$history, .bathy = gebco)

#### Example (2): Calculate POU from parquet files
pou_2 <- pf_pou(.history = forward_folder, .bathy = gebco)
stopifnot(terra::all.equal(pou_1, pou_2))

#### Example (3): Customise plot via ...
pou <- pf_pou(.history = forward_folder,
              .bathy = gebco,
              col = grDevices::cm.colors(100))
get_hr_full(pou, .add = TRUE, lwd = 0.25)
get_hr_core(pou, .add = TRUE, lwd = 0.75)

# Clean up
unlink(ac_folder, recursive = TRUE)
unlink(forward_folder, recursive = TRUE)
