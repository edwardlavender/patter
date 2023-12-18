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
out_pfb <- dat_pfbk()
pou     <- pf_map_pou(.history = out_pfb$history, .bathy = gebco)
# Smooth POU
dens_1    <- pf_map_dens(pou)

#### Example (2): Smooth particle coordinates
# This approach is equivalent to Example (1) & the outputs are identical
# But this implementation can be considerably faster
dens_2 <- pf_map_dens(gebco, .coord = pf_coords(out_pfb$history, gebco))
stopifnot(all.equal(dens_1, dens_2))

#### Example (3): Smooth COAs
# Define data list
dlist <- pat_setup_data(.acoustics = dat_acoustics[individual_id == 25, ],
                        .moorings = dat_moorings,
                        .bathy = dat_gebco(),
                        .lonlat = FALSE)
# Calculate COAs
out_coa <- coa(dlist, .delta_t = "12 hours", .plot_weights = FALSE)
out_coa <- out_coa[, .(x = coa_x, y = coa_y)]
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
