require(data.table)
require(dtplyr)
require(dplyr, warn.conflicts = FALSE)

#### Set up forward simulation
# Set up data list(see `?pat_setup_data()`)
dlist <- dat_dlist()
# Add AC* algorithm layers
dlist$algorithm$detection_overlaps <- acs_setup_detection_overlaps(dlist)
dlist$algorithm$detection_kernels  <- acs_setup_detection_kernels(dlist)
# Set up observations (see `?pf_setup_obs()`)
obs <- dat_obs()

#### Example (1): Implement ACPF algorithm with default options
## Implement simulation
pf_lik_acpf <- list(acs_filter_land = acs_filter_land,
                    acs_filter_container = acs_filter_container,
                    pf_lik_ac = pf_lik_ac)
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .record = pf_opt_record(.save = TRUE))
## Examine output object
# The function returns a named list:
summary(out_pff)
# The `history` element contains a list of particle samples
head(out_pff$history[[1]])
head(out_pff$history[[2]])
# The `diagnostics` element contains data.table of particle diagnostics
head(out_pff$diagnostics, 20)
# `convergence` records convergence (TRUE/FALSE)
out_pff$convergence
# `time` records timings
out_pff$time

#### Example (2): Implement DCPF algorithm with default options
# Define shallow and depth limits
obs[, depth_shallow := depth - 20]
obs[, depth_deep := depth + 20]
obs[, depth_scale := depth_deep - depth_shallow]
# (optional) Define origin SpatRaster
origin <- dlist$spatial$bathy
origin <- terra::clamp(origin,
                       lower = obs$depth_shallow[1],
                       upper = obs$depth_deep[1])
dlist$spatial$origin <- origin
# Implement DCPF algorithm
pf_lik_dcpf <- list(pf_lik_dc = pf_lik_dc)
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_dcpf,
                      .record = pf_opt_record(.save = TRUE))

#### Example (3): Implement ACDCPF algorithm with default options
pf_lik_acdcpf <- list(pf_lik_dc = pf_lik_dc,
                      acs_filter_container = acs_filter_container,
                      pf_lik_ac = pf_lik_ac)
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acdcpf,
                      .record = pf_opt_record(.save = TRUE))

#### Example (4): Customise movement model via `.rpropose` and `.dpropose`
# Pass arguments to the default models via `.rargs` & `.dargs`
# * For `.rpropose` = `pf_rpropose_kick()`, `.rargs` get passed to
# * ... `.rkick` = `rkick()` > `.rstep` = `rstep()` >
# * ... `.rlen` = rlen(), `.rang` = rang()
# * For `.dpropose` = `pf_dpropose()`, `.dargs` gets passed to
# * ... `.dkick` = `dkick()` > `.dstep` = `dstep()` > `.dlen` = `dtruncgamma()`

# E.g., customise rlen() shape and scale parameters
hist(rlen(.n = 1e5L, .shape = 1, .scale = 100), xlim = c(0, 500))
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .rpropose = pf_rpropose_kick,
                      .rargs = list(.shape = 1, .scale = 100),
                      .dpropose = pf_dpropose,
                      .dargs = list(.shape = 1, .scale = 100),
                      .likelihood = pf_lik_acdcpf,
                      .record = pf_opt_record(.save = TRUE),
                      .control = pf_opt_control(.sampler_batch_size = 100L))

# E.g., customise rlen() .mobility parameter
# > Restricted mobility leads to convergence failure
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .rpropose = pf_rpropose_kick,
                      .rargs = list(.mobility = 100),
                      .dpropose = pf_dpropose,
                      .dargs = list(.mobility = 100),
                      .likelihood = pf_lik_acdcpf,
                      .record = pf_opt_record(.save = TRUE),
                      .control = pf_opt_control(.sampler_batch_size = 100L))
# > Pairwise steps are < .mobility + grid resolution/2
for (i in 2:length(out_pff$history)) {
  print(i)
  dist <- clen(out_pff$history[[i]][, .(x_past, y_past)],
               out_pff$history[[i]][, .(x_now, y_now)], .lonlat = FALSE)
  stopifnot(max(dist) < 100 + terra::res(dlist$spatial$bathy)[1] / 2)
}
# > Sequential steps are < .mobility + grid resolution/2
out_pfbk <- pf_backward_killer(out_pff$history,
                               .record = pf_opt_record(.save = TRUE))
out_path <-
  pf_path(out_pfbk$history, .bathy = dlist$spatial$bathy) |>
  group_by(path_id) |>
  mutate(dist = dist_along_path(cbind(cell_x, cell_y), .lonlat = FALSE)) |>
  ungroup() |>
  as.data.table()
stopifnot(max(out_path$dist, na.rm = TRUE) <
  100 + terra::res(dlist$spatial$bathy)[1] / 2)

## E.g. customise rlen() and dlen() models
# Write a new model with step lengths simulated from a lognormal distribution
hist(rlnorm(1e6L, meanlog = 5, sdlog = 0.25), xlim = c(0, 500))
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .rpropose = pf_rpropose_kick,
                      .dpropose = pf_dpropose,
                      .rargs = list(.rlen = rlnorm, meanlog = 5, sdlog = 0.25),
                      .dargs = list(.dlen = dlnorm, meanlog = 5, sdlog = 0.25),
                      .likelihood = pf_lik_acdcpf,
                      .record = pf_opt_record(.save = TRUE),
                      .control = pf_opt_control(.sampler_batch_size = 100L))

#### Example (5): Customise likelihood
# We can implement different algorithms by modifying the .likelihood list (above)
# We can also write custom likelihood functions
# As an example, we will imagine we have observed temperatures every two minutes
obs[, temp := rnorm(.N, 7)]
obs[, temp_cool := temp - 2]
obs[, temp_warm := temp + 2]
# We will also imagine we have hourly temperature data across the study area
# (e.g., from a hydrodynamic model)
obs[, hour := as.integer(cut(timestamp, "hour"))]
hours <- unique(obs$hour)
grid <- terra::setValues(dlist$spatial$bathy, NA)
nc   <- terra::ncell(grid)
temps <-
  hours |>
  lapply(function(hour) {
    vals <- rnorm(nc, 7)
    hydro <- terra::setValues(grid, vals)
    terra::mask(hydro, dlist$spatial$bathy)
  }) |>
  terra::rast()
names(temps) <- hours
terra::plot(temps)
dlist$spatial$temp <- temps
# Define the likelihood of the temp data given location proposals
pf_lik_temp <- function(.particles, .obs, .t, .dlist, .drop) {
  # Extract temps
  temp <- NULL
  locs <- terra::vect(cbind(.particles$x_now, .particles$y_now))
  .particles[, temp := terra::extract(x = .dlist$spatial$temp,
                                      y = locs,
                                      layer = .obs$hour[.t])$value]
  # Calculate temp likelihood
  # * We use a simple binary model for illustration
  # * Under this model, temperature observations that are not
  # * ... within a cool/warm limit are impossible
  .particles[, lik_temp := (temp >= .obs$temp_cool[.t] &
                              temp <= .obs$temp_warm[.t]) + 0, ]
  # Update likelihood
  lik <- NULL
  .particles[, lik := lik * lik_temp]
  # (optional) Filter impossible locations
  if (.drop) {
    .particles <- .particles[lik > 0, ]
  }
  .particles
}
# Run simulation accounting for temperature data only
dlist$spatial$origin <- NULL
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = list(pf_lik_temp = pf_lik_temp),
                      .record = pf_opt_record(.save = TRUE))
# Run simulation accounting for multiple datasets
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = list(pf_lik_ac = pf_lik_ac,
                                         pf_lik_temp = pf_lik_temp),
                      .record = pf_opt_record(.save = TRUE))
# Note that the temperature likelihood (deliberately) has no influence here:
out_pff$diagnostics

#### Example (6): Customise (re)sampling
# Adjust the number of particles
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .n = 1000L,
                      .record = pf_opt_record(.save = TRUE))
nrow(out_pff$history[[1]])
# Use systematic resampling
# * This triggers directed sampling, so we boost sampler_batch_size for improved speed
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .sample = pf_sample_systematic,
                      .control = pf_opt_control(.sampler_batch_size = 100L),
                      .record = pf_opt_record(.save = TRUE))

#### Example (7): Control convergence via `.trial_` arguments
# See `vignette("c-demos", package = "patter")` for detailed examples

#### Example (8): Rerun the algorithm from an earlier time step
# This is only sensible in the case of a convergence failure,
# but for demonstration purposes:
ssv()
out_pff_1 <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .record = pf_opt_record(.save = TRUE))
ssv()
out_pff_2 <- pf_forward(.obs = obs,
                        .dlist = dlist,
                        .likelihood = pf_lik_acpf,
                        .record = pf_opt_record(.save = TRUE),
                        .rerun = out_pff_1, .rerun_from = 5L)

#### Example (9): Adjust record options
# Use `sink` to write to particles to file (recommended)
pff_folder <- file.path(tempdir(), "forward")
dir.create(pff_folder)
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .record = pf_opt_record(.save = FALSE, .sink = pff_folder))
# > `save = FALSE` suppresses outputs in memory:
out_pff$history
out_pff$diagnostics
# > `sink` directs outputs to file:
list.files(pff_folder)
head(pf_files(file.path(pff_folder, "history")))
# > Check file size (MB)
pf_files_size(file.path(pff_folder, "history"))
# Use `cols` to restrict the output columns
cols <- c("timestep", "cell_now", "x_now", "y_now")
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .record = pf_opt_record(.save = FALSE,
                                              .sink = pff_folder,
                                              .cols = cols))
pf_files_size(file.path(pff_folder, "history"))
unlink(pff_folder, recursive = TRUE)

#### Example (10): Adjust standard `patter-progress` options
# Use a log.txt file
log.txt <- tempfile(fileext = ".txt")
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .record = pf_opt_record(.save = TRUE),
                      .verbose = log.txt)
readLines(log.txt)
unlink(log.txt)
# Suppress `.verbose`
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .record = pf_opt_record(.save = TRUE),
                      .verbose = FALSE)
# Suppress progress bar
pbo <- pbapply::pboptions(type = "n")
ssv()
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = pf_lik_acpf,
                      .record = pf_opt_record(.save = TRUE))
pbapply::pboptions(pbo)
