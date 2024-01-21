#### Set up examples
# Run the forward simulation (see ?`pf_forward()`)
# We will use example output objects:
out_pff    <- dat_pff()
pff_folder <- dat_pff_src(.folder = "history")

#### Example (1): Use particle samples in memory or on file
# Particles can be provided in any format accepted by `?.pf_history_list()`
# Here, we use precomputed samples:
out_pfbk_a <- pf_backward_killer(.history = out_pff,
                                 .record = pf_opt_record(.save = TRUE))
out_pfbk_b <- pf_backward_killer(.history = out_pff$history,
                                 .record = pf_opt_record(.save = TRUE))
out_pfbk_c <- pf_backward_killer(.history = pff_folder,
                                 .record = pf_opt_record(.save = TRUE))
out_pfbk_d <- pf_backward_killer(.history = pf_files(pff_folder),
                                 .record = pf_opt_record(.save = TRUE))
stopifnot(isTRUE(all.equal(out_pfbk_a$history, out_pfbk_b$history)))
stopifnot(isTRUE(all.equal(out_pfbk_a$history, out_pfbk_c$history)))
stopifnot(isTRUE(all.equal(out_pfbk_a$history, out_pfbk_d$history)))

#### Example (2): Write history to file (as in `pf_forward()`)
con         <- file.path(tempdir(), "patter")
pfbk_folder <- file.path(con, "backward", "killer")
dir.create(pfbk_folder, recursive = TRUE)
out_pfbk <- pf_backward_killer(.history = pf_files(pff_folder),
                               .record = pf_opt_record(.sink = pfbk_folder))
pf_files(pfbk_folder)

#### Example (3): Adjust standard `patter-progress` options
# Use a log.txt file
log.txt <- tempfile(fileext = ".txt")
out_pfbk <- pf_backward_killer(pf_files(pff_folder),
                               .record = pf_opt_record(.save = TRUE),
                               .verbose = log.txt)
head(readLines(log.txt), 15)
unlink(log.txt)
# Suppress `.verbose`
out_pfbk <- pf_backward_killer(pf_files(pff_folder),
                               .record = pf_opt_record(.save = TRUE),
                               .verbose = FALSE)
# Suppress progress bar
pbo <- pbapply::pboptions(type = "n")
out_pfbk <- pf_backward_killer(pf_files(pff_folder),
                               .record = pf_opt_record(.save = TRUE))

# Clean up
unlink(con, recursive = TRUE)
