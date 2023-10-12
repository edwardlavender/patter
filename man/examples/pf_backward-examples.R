#### Set up examples

# (A) Define input datasets
acoustics <- dat_acoustics[individual_id == 25, ]
archival <- dat_archival[individual_id == 25, ]
obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
obs <- obs[1:200, ]
gebco <- dat_gebco()

# (B) Implement AC* algorithm
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
out_pff <- pf_forward(.obs = obs,
                      .record = out_ac$record,
                      .n = 1e3,
                      .kick = pf_kick,
                      .bathy = gebco,
                      .save_history = TRUE,
                      .write_history = list(sink = forward_folder))

#### Example (1): Implement backward simulation from `pf` object
out_pfb <- pf_backward(out_pff$history, .save_history = TRUE)

#### Example (2): Implement backward simulation from parquet files
out_pfb <- pf_backward(pf_setup_files(forward_folder),
                       .save_history = TRUE)

#### Example (3): Write history to file (as in `pf_forward()`)
backward_folder <- file.path(tempdir(), "pf", "backward")
dir.create(backward_folder, recursive = TRUE)
out_pfb <- pf_backward(pf_setup_files(forward_folder),
                       .write_history = list(sink = backward_folder))
utils::head(list.files(backward_folder))

#### Example (4): Control monitoring of function progress
# Suppress progress bar
out_pfb <- pf_backward(pf_setup_files(forward_folder),
                       .save_history = TRUE,
                       .progress = FALSE)
# Save messages
log.txt <- tempfile(fileext = ".txt")
out_pfb <- pf_backward(pf_setup_files(forward_folder),
                       .save_history = TRUE,
                       .con = log.txt)
utils::head(readLines(log.txt), 15)
# Suppress messages
out_pfb <- pf_backward(pf_setup_files(forward_folder),
                       .save_history = TRUE,
                       .verbose = FALSE)
