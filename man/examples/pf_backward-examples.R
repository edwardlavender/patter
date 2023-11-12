#### Set up examples
# A. Define input datasets (see ?`acs_setup_obs()`)
# (Here, we used pre-defined outputs for speed)
obs <- dat_obs()
# B. Implement AC-branch algorithm (see ?`acs()` or ?`dc()`)
out_ac <- dat_ac()
# C. Implement forward simulation (see ?`pf_forward_*()`)
con <- tempdir()
pff_folder <- file.path(tempdir(), "patter", "pf", "forward")
dir.create(pff_folder, recursive = TRUE)
out_pff <- pf_forward_1(.obs = obs,
                        .record = out_ac$record,
                        .n = 1e3,
                        .kick = pf_kick,
                        .save_history = TRUE,
                        .write_history = list(sink = pff_folder))

#### Example (1): Implement backward simulation from `pf` object
out_pfb <- pf_backward(out_pff$history, .save_history = TRUE)

#### Example (2): Implement backward simulation from parquet files
out_pfb <- pf_backward(pf_setup_files(pff_folder),
                       .save_history = TRUE)

#### Example (3): Write history to file (as in `pf_forward_*()`)
pfb_folder <- file.path(con, "patter", "pf", "backward")
dir.create(pfb_folder, recursive = TRUE)
out_pfb <- pf_backward(pf_setup_files(pff_folder),
                       .write_history = list(sink = pfb_folder))
utils::head(list.files(pfb_folder))

#### Example (4): Control monitoring of function progress
# Suppress progress bar
out_pfb <- pf_backward(pf_setup_files(pff_folder),
                       .save_history = TRUE,
                       .progress = FALSE)
# Save messages
log.txt <- tempfile(fileext = ".txt")
out_pfb <- pf_backward(pf_setup_files(pff_folder),
                       .save_history = TRUE,
                       .txt = log.txt)
utils::head(readLines(log.txt), 15)
# Suppress messages
out_pfb <- pf_backward(pf_setup_files(pff_folder),
                       .save_history = TRUE,
                       .verbose = FALSE)

# Clean up
unlink(file.path(con, "patter"), recursive = TRUE)
