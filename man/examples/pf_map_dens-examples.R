#### Set up examples
# Load and attach selected packages
require(data.table)
require(dtplyr)
require(dplyr, warn.conflicts = FALSE)
require(spatstat.explore)
# Define grid
gebco   <- dat_gebco()

#### Example (1): Smooth a POU SpatRaster
# Estimate POU on a grid based on pre-prepared particle samples
out_pfb <- dat_pfb()
pou     <- pf_map_pou(.history = out_pfb$history, .bathy = gebco)
# Smooth POU
dens_1    <- pf_map_dens(pou)

#### Example (2): Smooth particle coordinates
# This approach is equivalent to Example (1) & the outputs are identical
dens_2 <- pf_map_dens(gebco, .coord = pf_coords(out_pfb$history, gebco))
stopifnot(all.equal(dens_1, dens_2))

#### Example (3): Smooth coordinates from other algorithms
# Define coordinates to smooth (e.g., based on COA algorithm)
out_coa <-
  # Define acoustic data for an example individual
  # (over the same time period as `out_pfb`)
  dat_acoustics |>
  filter(individual_id == 25) |>
  slice(seq_len(length(out_pfb$history))) |>
  # Calculate centres of activity over some (arbitrary) time interval
  merge(dat_moorings, by = "receiver_id") |>
  as.data.table() |>
  coa(.delta_t = "12 hours", .plot_weights = FALSE) |>
  select(x = "coa_x", y = "coa_y") |>
  as.data.frame()
# Smooth centres of activity
dens_3 <- pf_map_dens(gebco, .coord = out_coa)
graphics::points(out_coa)

#### Example (4): Control smoothing parameters via `spatstat.explore::density.ppp()`
# E.g. use fixed bandwidth:
pf_map_dens(pou, sigma = 5)
pf_map_dens(pou, sigma = 100)
# E.g., perform automatic bandwidth selection using cross validation:
pf_map_dens(pou, sigma = bw.diggle)
pf_map_dens(pou, sigma = bw.scott)
# pf_map_dens(pou, sigma = bw.CvL)  # ~12 s (slow)
# pf_map_dens(pou, sigma = bw.ppl)  # ~14 s (slow)
