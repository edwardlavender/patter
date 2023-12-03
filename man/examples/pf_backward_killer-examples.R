#### Set up examples
obs <- dat_obs()
con <- tempdir()
pff_folder <- file.path(tempdir(), "patter", "pf", "forward")
dir.create(pff_folder, recursive = TRUE)
out_pff <- pf_forward(.obs = obs,
                      .bathy = dat_gebco(),
                      .moorings = dat_moorings, .detection_overlaps = dat_overlaps(),
                      .detection_kernels = dat_kernels(),
                      .save_opts = TRUE,
                      .write_opts = list(sink = pff_folder))

#### Example (1): Implement backward simulation from `pf` object
out_pfb <- pf_backward(out_pff$history, .save_history = TRUE)

#### Example (2): Implement backward simulation from parquet files
pff_folder_h <- file.path(pff_folder, "history")
out_pfb <- pf_backward(pf_setup_files(pff_folder_h),
                       .save_history = TRUE)

#### Example (3): Write history to file (as in `pf_forward()`)
pfb_folder <- file.path(con, "patter", "pf", "backward")
dir.create(pfb_folder, recursive = TRUE)
out_pfb <- pf_backward(pf_setup_files(pff_folder_h),
                       .write_history = list(sink = pfb_folder))
utils::head(list.files(pfb_folder))

#### Example (4): Control monitoring of function progress
# Suppress progress bar
out_pfb <- pf_backward(pf_setup_files(pff_folder_h),
                       .save_history = TRUE,
                       .progress = FALSE)
# Save messages
log.txt <- tempfile(fileext = ".txt")
out_pfb <- pf_backward(pf_setup_files(pff_folder_h),
                       .save_history = TRUE,
                       .txt = log.txt)
utils::head(readLines(log.txt), 15)
# Suppress messages
out_pfb <- pf_backward(pf_setup_files(pff_folder_h),
                       .save_history = TRUE,
                       .verbose = FALSE)

# Clean up
unlink(file.path(con, "patter"), recursive = TRUE)
