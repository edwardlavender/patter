#### Workflow
# A. Define input datasets & objects
# * ?`acs_setup_obs()`
# * ?`acs_setup_detection_containers()`
# * ?`acs_setup_detection_overlaps()`
# * ?`acs_setup_detection_kernels()`
# B. Implement AC* algorithm(s)
# C. Proceed with particle filtering (?`pf_forward()`)

#### Set up examples using pre-defined datasets for speed
obs <- dat_obs()
gebco      <- dat_gebco()
containers <- dat_containers()
overlaps   <- dat_overlaps()
kernels    <- dat_kernels()

#### Example (1): AC algorithm minimal implementation
# This implementation assumes there are no overlapping receivers!
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_kernels = kernels,
      .save_record = TRUE)
# The function returns an `acb-class` object
class(out_ac)
summary(out_ac)
# This contains:
# * `record` - a time series of maps that define the individual's possible locations
# * `map` - a cumulative map (if requested)
terra::plot(out_ac$record[[1]])
terra::plot(out_ac$record[[2]])

#### Example (2): Create a cumulative map on the fly
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .save_cumulative = TRUE)
terra::plot(out_ac$map)

#### Example (3): Account for receiver overlaps
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .save_cumulative = TRUE)
terra::plot(out_ac$map)

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
  # This produces a map of possible locations at each time step
  # * red: possible location boundaries given data
  # * orange: possible location boundaries given past
  # * green: possible location boundaries given future
  # * background: possible locations at the present time given data, past and future
  out_ac <-
    acs(obs,
        .bathy = gebco,
        .detection_overlaps = overlaps,
        .detection_kernels = kernels,
        .progress = FALSE,
        .save_record = TRUE,
        .prompt = TRUE)
}
# Use `.con` to write messages to file
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
  out_ac$record[[i]] |>
    terra::plot(main = paste("AC", i))
  out_acdc$record[[i]] |>
    terra::plot(main = paste("ACDC", i))
}) |> invisible()
par(pp)

#### Example (6): Implement ACDC algorithm via probabilistic function
# We assume the individual's true depth is normally distributed around the observed depth
calc_pr_given_depth <- function(.depth_seabed, .depth_obs) {
  dnorm(.depth_seabed, mean = .depth_obs, sd = 1)
}
# Test function works as expected
# * If the observed depth is 30 m, this is the implied set of possible locations
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

#### Example (7): AC* algorithms are typically followed by particle filtering
# See ?`pf_forward()` and associated pf_*() functions
# ... to produce movement paths & refined maps of space use
