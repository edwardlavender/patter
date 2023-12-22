#### Set up examples
# Run the forward simulation (see ?`pf_forward()`)
# We will use example output objects:
out_pff    <- dat_pff()
pff_folder <- dat_pff_src(.folder = "history")

#### Example (1): Implement backward pass from object in memory
out_pfb <- pf_backward_killer(.history = out_pff$history,
                              .record = pf_opt_record(.save = TRUE))

#### Example (2): Implement backward pass from parquet files
out_pfb <- pf_backward_killer(.history = pf_files(pff_folder),
                              .record = pf_opt_record(.save = TRUE))

#### Example (3): Write history to file (as in `pf_forward()`)
con        <- file.path(tempdir(), "patter")
pfb_folder <- file.path(con, "backward", "killer")
dir.create(pfb_folder, recursive = TRUE)
out_pfb <- pf_backward_killer(.history = pf_files(pff_folder),
                              .record = pf_opt_record(.sink = pfb_folder))
head(list.files(pfb_folder))

#### Example (4): Adjust standard `patter-progress` options
# Use a log.txt file
log.txt <- tempfile(fileext = ".txt")
out_pfb <- pf_backward_killer(pf_files(pff_folder),
                              .record = pf_opt_record(.save = TRUE),
                              .verbose = log.txt)
head(readLines(log.txt), 15)
unlink(log.txt)
# Suppress `.verbose`
out_pfb <- pf_backward_killer(pf_files(pff_folder),
                              .record = pf_opt_record(.save = TRUE),
                              .verbose = FALSE)
# Suppress progress bar
pbo <- pbapply::pboptions(type = "n")
out_pfb <- pf_backward_killer(pf_files(pff_folder),
                              .record = pf_opt_record(.save = TRUE))

# Clean up
unlink(con, recursive = TRUE)
