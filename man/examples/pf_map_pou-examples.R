#### Workflow
# A. Define input datasets (see ?`acs_setup_obs()`)
# B. Implement forward simulation (see ?`acs()`, ?`dc()` and ?`pf_forward_*()`)
# C. Implement backwards pass (see ?`pf_backward()`)
# D. Implement `pf_path()`, `pf_map_pou()` etc.

#### Set up examples using pre-defined datasets
# Define required datasets (e.g., observations, bathymetry grid)
obs        <- dat_obs()
gebco      <- dat_gebco()
# Collect pre-defined particle samples from forward simulation
dat_pff    <- dat_pff()
# Implement backwards pass
con        <- tempdir()
pfb_folder <- file.path(con, "patter", "pf", "backward")
dir.create(pfb_folder, recursive = TRUE)
out_pfb <- pf_backward(dat_pff$history,
                       .save_history = TRUE,
                       .write_history = list(sink = pfb_folder))

#### Example (1): Calculate POU from pf object
pou_1 <- pf_map_pou(.history = out_pfb$history, .bathy = gebco)

#### Example (2): Calculate POU from parquet files
pou_2 <- pf_map_pou(.history = pfb_folder, .bathy = gebco)
stopifnot(terra::all.equal(pou_1, pou_2))

#### Example (3): Customise plot via ...
pou <- pf_map_pou(.history = pfb_folder,
                  .bathy = gebco,
                  col = grDevices::cm.colors(100))
get_hr_full(pou, .add = TRUE, lwd = 0.25)
get_hr_core(pou, .add = TRUE, lwd = 0.75)

# Clean up
unlink(pfb_folder, recursive = TRUE)
