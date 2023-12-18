#### Workflow
# A. Define input datasets (see ?`acs_setup_obs()`)
# B. Implement forward simulation (see ?`pf_forward()`)
# C. Implement backwards pass (see ?`pf_backward_*()`)
# D. Implement `pf_path()`, `pf_map_pou()` etc.

#### Set up examples using pre-defined datasets
gebco       <- dat_gebco()
out_pfbk    <- dat_pfbk()
pfbk_folder <- system.file("extdata", "acpf", "backward", "killer",
                           package = "patter", mustWork = TRUE)

#### Example (1): Calculate POU from pf object
pou_1 <- pf_map_pou(.history = out_pfbk$history, .bathy = gebco)

#### Example (2): Calculate POU from parquet files
pou_2 <- pf_map_pou(.history = pfbk_folder, .bathy = gebco)
stopifnot(terra::all.equal(pou_1, pou_2))

#### Example (3): Customise plot via ...
pou <- pf_map_pou(.history = pfbk_folder,
                  .bathy = gebco,
                  col = grDevices::cm.colors(100))
get_hr_full(pou, .add = TRUE, lwd = 0.25)
get_hr_core(pou, .add = TRUE, lwd = 0.75)
