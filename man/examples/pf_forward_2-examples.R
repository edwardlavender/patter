#### Set up examples
# Use pre-prepared datasets (see also `?acs()`)
moorings  <- dat_moorings
obs       <- dat_obs()
gebco     <- dat_gebco()
overlaps  <- dat_overlaps()
kernels   <- dat_kernels()

#### Example (1): Implement the ACPF algorithm using default options
# * AC algorithm is implemented on the fly using overlaps & kernels
# * These pre-prepared datasets assume a particular detection probability model
# * PF is implemented using the default movement model
out_pff <- pf_forward_2(obs,
                        .bathy = gebco,
                        .moorings = moorings,
                        .detection_overlaps = overlaps,
                        .detection_kernels = kernels,
                        .kick = pf_kick,
                        .save_history = TRUE)
# The function returns a list as `pf_forward_1()`:
summary(out_pff)

#### Example (2): Implement the DCPF algorithm
# Define depth-error model (see also `?acs()` and ?dc()`)
# * We imagine the individual's depth is known ± 25 m with equal probability
calc_depth_error   <- function(...) matrix(c(-25, 25), nrow = 2)
# Define shallow/deep depth limits
obs$depth_shallow <- obs$depth + calc_depth_error(obs$depth)[1, ]
obs$depth_deep    <- obs$depth + calc_depth_error(obs$depth)[2, ]
# Define function that calculates DC weights
update_ac <- function(.particles, .bathy, .obs, .t, ...) {
  # Extract depth of seabed at particle positions
  .particles$bathy <- terra::extract(.bathy, as.matrix(.particles[, c("x_now", "y_now")]))
  # Weight = 1 in locations where bathymetric depth is within possible limits, otherwise 0
  (.particles$bathy  >= .obs$depth_shallow[.t] & .particles$bathy <= .obs$depth_deep[.t]) + 0
}
# Implement DCPF
out_pff <- pf_forward_2(obs,
                        .bathy = gebco,
                        .kick = pf_kick,
                        .update_ac = update_ac,
                        .save_history = TRUE)
# Extract coordinates & validate implementation of depth-error model
#
# TO DO
#

#### Example (3): Implement the ACDCPF algorithm
#
# TO DO
#

#### Example (4): Write history to file
# See `pf_forward_1()`

#### Example (4): Customise verbose options
# See `pf_forward_1()`

#### Example (5): Customise movement model (as in `pf_forward_1()`)
# See `pf_forward_1()`

#### Example (6): Implement the backward pass
# `pf_forward_*()` should be followed by `pf_backward()`
