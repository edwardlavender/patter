#### Define datasets
acoustics <- dat_acoustics[individual_id == 25, ]
archival <- dat_archival[individual_id == 25, ]

#### Process datasets
obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
obs <- obs[1:200, ]
head(obs, 25)

#### Define overlapping receivers
# Define detection containers
gebco <- dat_gebco()
dat_moorings$receiver_range <- 500
containers <- acs_setup_detection_containers(gebco, dat_moorings)
terra::plot(containers[[3]])
# Identify receivers with overlapping containers for each array design
overlaps <- acs_setup_detection_overlaps(containers, dat_moorings)

#### Define detection kernels
kernels <-
  acs_setup_detection_kernels(dat_moorings,
                              .calc_detection_pr = acs_setup_detection_pr,
                              .bathy = gebco)

#### Example (1): AC algorithm minimal implementation
# This implementation assumes there are no overlapping receivers!
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_kernels = kernels,
      .save_record = TRUE)
# The function returns an ac_record-class object
class(out_ac)
summary(out_ac)
# The archive element contains
# * `record` - a time series of maps that define the individual's possible locations
# * `map` - a cumulative map (if requested)
out_ac$archive
terra::plot(out_ac$archive$record[[1]])
terra::plot(out_ac$archive$record[[120]])

#### Example (2): Create a cumulative map on the fly
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .save_cumulative = TRUE)
terra::plot(out_ac$archive$map)

#### Example (3): Account for receiver overlaps
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .save_cumulative = TRUE)
terra::plot(out_ac$archive$map)

#### Example (4): Write record to file
# This is useful for longer time series
path <- file.path(tempdir(), "patter")
dir.create(path)
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .write_record = list(filename = path))

#### Customise verbose options
# Suppress progress bar
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .progress = FALSE,
      .save_record = TRUE)
# Use prompt = TRUE for debugging
if (interactive()) {
  out_ac <-
    acs(obs,
        .bathy = gebco,
        .detection_overlaps = overlaps,
        .detection_kernels = kernels,
        .progress = FALSE,
        .save_record = TRUE,
        .prompt = TRUE)
}
# Use con to write messages to file
# * This is highly recommended
log.txt <- tempfile(fileext = ".txt")
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .con = log.txt)
readLines(log.txt) |> utils::head()
# Suppress messages
# * This is not recommended
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .verbose = FALSE)

#### Example (5): Implement ACDC algorithm via depth error function
# Define depth error function
# * We imagine the individual's depth is known ± 25 m with equal probability
calc_depth_error   <- function(...) matrix(c(-25, 25), nrow = 2)
# Define shallow/deep depth limits
obs$depth_shallow <- obs$depth + calc_depth_error(obs$depth)[1, ]
obs$depth_deep    <- obs$depth + calc_depth_error(obs$depth)[2, ]
# Define function to update AC layer, accepting four arguments
# * The SpatRaster that defines the possible locations of the individual given the AC algorithm
# * The bathymety Raster
# * The observations data.table
# & The time step (used to index obs if necessary)
update_ac <- function(.spat, .bathy, .obs, .t, ...) {
  .spat * (.bathy >= .obs$depth_shallow[.t] & .bathy <= .obs$depth_deep[.t])
}
# Test function works as expected
blank <- terra::setValues(gebco, 1)
update_ac(blank, gebco, obs, 1) |> terra::plot()
# Implement ACDC algorithm
out_acdc <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .update_ac = update_ac,
      .save_record = TRUE,
      .verbose = FALSE)

# Compare AC and ACDC algorithms
require(graphics)
pp <- par(mfrow = c(3, 2))
lapply(1:3, \(i) {
  out_ac$archive$record[[i]] |>
    terra::plot(main = paste("AC", i))
  out_acdc$archive$record[[i]] |>
    terra::plot(main = paste("ACDC", i))
}) |> invisible()
par(pp)

#### Example (6): Implement ACDC algorithm via probabilistic function
# We assume the individual's true depth is normally distributed around the observed depth
calc_pr_given_depth <- function(.depth_seabed, .depth_obs) {
  dnorm(.depth_seabed, mean = .depth_obs, sd = 1)
}
# Test function works as expected
# * If the observed depth is 30 m, this is the implies set of possible locations
pr_given_depth <- terra::app(gebco, fun = \(x) calc_pr_given_depth(x, .depth_obs = 30))
terra::plot(pr_given_depth)
# Update update_ac() function
update_ac <- function(.spat, .bathy, .obs, .t, ...) {
  # (optional) You could add other surfaces in here too
  .spat * terra::app(.bathy, fun = \(x) calc_pr_given_depth(x, .depth_obs = .obs$depth[.t]))
}
# Implement ACDC algorithm
out_acdc <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .update_ac = update_ac,
      .save_record = TRUE,
      .verbose = FALSE)

