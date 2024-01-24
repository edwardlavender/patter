#### Set up
# Use precomputed function inputs & particle samples from `?pf_forward()`:
# * Data list (see `?pat_setup_data()` and `?pf_forward()`)
# * Observations timeline (see `?pf_setup_obs()` and `?pf_forward()`)
# * Particle samples (see `?pf_forward()`)
obs      <- dat_obs()
dlist    <- dat_dlist()
out_pff  <- dat_pff()
# Implement `pf_backward_killer()` (for comparison)
out_pfbk <- pf_backward_killer(.history = out_pff,
                               .record = pf_opt_record(.save = TRUE))
# Define the number of time steps
nt       <- length(out_pff$history)

#### Example (1): Implement function with default arguments
ssv()
out_pfbs <- pf_backward_sampler_v(.history = out_pff,
                                  .dlist = dlist,
                                  .record = pf_opt_record(.save = TRUE))
# The function returns a pf_particles-class object
summary(out_pfbs)
# Particle samples are located in the `history` element
head(out_pfbs$history[[1]])
head(out_pfbs$history[[2]])
head(out_pfbs$history[[nt]])
# The `time` element records timing
out_pfbs$time

#### Example (2): Use particle samples in memory or on file
# Particles can be provided in any format accepted by `?.pf_history_list()`
# Here, we use precomputed samples:
ssv()
out_pfbs_b <- pf_backward_sampler_v(.history = out_pff$history,
                                    .dlist = dlist,
                                    .record = pf_opt_record(.save = TRUE))
ssv()
out_pfbs_c <- pf_backward_sampler_v(.history = dat_pff_src(),
                                    .dlist = dlist,
                                    .record = pf_opt_record(.save = TRUE))
ssv()
out_pfbs_d <- pf_backward_sampler_v(.history = pf_files(dat_pff_src()),
                                    .dlist = dlist,
                                    .record = pf_opt_record(.save = TRUE))
stopifnot(all.equal(out_pfbs$history, out_pfbs_b$history))
stopifnot(all.equal(out_pfbs$history, out_pfbs_c$history))
stopifnot(all.equal(out_pfbs$history, out_pfbs_d$history))

#### Example (3): Customise the movement model
# Use .dpropose and .dargs

#### Example (4): Write history to file (as in `pf_forward()`)
con        <- file.path(tempdir(), "patter")
pfbs_folder <- file.path(con, "backward", "sampler")
dir.create(pfbs_folder, recursive = TRUE)
ssv()
out_pfbs <- pf_backward_sampler_v(.history = out_pff,
                                  .dlist = dlist,
                                  .record = pf_opt_record(.save = TRUE,
                                                          .sink = pfbs_folder))
cl_lapply(seq_len(nt), function(i) {
  a <- out_pfbs$history[[i]]
  b <- arrow::read_parquet(file.path(pfbs_folder, paste0(i, ".parquet")))
  stopifnot(all.equal(a, b))
})

#### Example (4): Adjust standard `patter-progress` options
# See `?pf_backward_killer()` for examples

#### Example (5): Compare particle diagnostics (e.g., degeneracy)
# Extract diagnostics
diag_f <- pf_diag_summary(out_pff)
diag_k <- pf_diag_summary(out_pfbk)
diag_s <- pf_diag_summary(out_pfbs)
# Compare the number of unique samples through time
ylim <- range(c(diag_f$n_u, diag_k$n_u, diag_s$n_u))
plot(diag_f$timestep, diag_f$n_u,
     ylim = ylim,
     xlab = "Time (steps)", ylab = "Number unique samples",
     type = "b", cex = 0.5, col = "red")
lines(diag_k$timestep, diag_k$n_u,
      type = "b", cex = 0.5, col = "orange")
lines(diag_s$timestep, diag_s$n_u,
      type = "b", cex = 0.5, col = "darkgreen")

#### Example (6): Compare maps
pp    <- par(mfrow = c(1, 3))
gebco <- dat_gebco()
map_dens(.map = gebco,
         .coord = pf_coord(.history = out_pff, .bathy = gebco),
         sigma = spatstat.explore::bw.diggle)
map_dens(.map = gebco,
         .coord = pf_coord(.history = out_pfbk, .bathy = gebco),
         sigma = spatstat.explore::bw.diggle)
map_dens(.map = gebco,
         .coord = pf_coord(.history = out_pfbs, .bathy = gebco),
         sigma = spatstat.explore::bw.diggle)
par(pp)


# Clean up
unlink(con, recursive = TRUE)
