#### Example (1): Use particle samples in memory or on file
# Particles can be provided in any format accepted by `?.pf_history_dt()`
# Here, we use precomputed samples:
gebco <- dat_gebco()
pf_coord(dat_pff(), .bathy = gebco)
pf_coord(dat_pff()$history, .bathy = gebco)
pf_coord(dat_pff_src(), .bathy = gebco)
pf_coord(pf_files(dat_pff_src()), .bathy = gebco)

#### Example (2): Use particle samples from the forward run or the backward pass
# (TO DO)

#### Example (3): Include additional columns in the output
obs <- dat_obs()
pxy <- pf_coord(dat_pff(), .bathy = gebco,
                .obs = obs, .cols = c("detection", "depth"))

#### Example (4): Use particle coordinates for mapping
# See `map_*()` functions e.g. `?map_dens()`
