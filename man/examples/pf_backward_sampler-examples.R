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
# D. Define inputs for pf_backward_*()
# * Use a subset of samples for speed
history <- out_pff$history[1:10]
gebco <- dat_gebco()

#### Example (1): Implementation with default options
out_pfb <-
  pf_backward_sampler(history,
                      .dens_step = dstep, lonlat = FALSE,
                      .save_history = TRUE)

#### Example (2): Parallelise default implementation
# Use forking
if (.Platform$OS.type == "unix") {
  out_pfb <-
    pf_backward_sampler(history,
                        .dens_step = dstep, lonlat = FALSE,
                        .save_history = TRUE,
                        .cl = 2L)
}
# Use socket cluster
out_pfb <-
  pf_backward_sampler(history,
                      .dens_step = patter::dstep, lonlat = FALSE,
                      .save_history = TRUE,
                      .cl = parallel::makeCluster(2L),
  )
