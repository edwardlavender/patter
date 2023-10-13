#### Workflow
# A. Define input datasets (see ?`acs_setup_obs()`)
# B. Implement AC-branch algorithm (see ?`acs()` or ?`dc()`)
# C. Implement forward simulation (see ?`pf_forward()`)
# D. Implement backwards pass (see ?`pf_backward()`)
# E. Implement `pf_path()`, `pf_pou()` etc.

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
pou_1 <- pf_pou(.history = out_pfb$history, .bathy = gebco)

#### Example (2): Calculate POU from parquet files
pou_2 <- pf_pou(.history = pfb_folder, .bathy = gebco)
stopifnot(terra::all.equal(pou_1, pou_2))

#### Example (3): Customise plot via ...
pou <- pf_pou(.history = pfb_folder,
              .bathy = gebco,
              col = grDevices::cm.colors(100))
get_hr_full(pou, .add = TRUE, lwd = 0.25)
get_hr_core(pou, .add = TRUE, lwd = 0.75)

# Clean up
unlink(pfb_folder, recursive = TRUE)
