#### Set up examples
# Use pre-prepared datasets
obs       <- dat_obs()
overlaps  <- dat_overlaps()
kernels   <- dat_kernels()
gebco     <- dat_gebco()
con       <- tempdir()
# Implement AC-branch (e.g., AC) algorithm with default settings
ac_folder <- file.path(con, "patter", "ac")
dir.create(ac_folder, recursive = TRUE)
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .write_record = list(filename = ac_folder, overwrite = TRUE))

#### Example (1): Implement pf_forward() with default options
out_pff <- pf_forward(.obs = obs,
                      .record = out_ac$record,
                      .n = 1e3,
                      .kick = pf_kick,
                      .bathy = gebco,
                      .save_history = TRUE)
# The function returns a named list:
summary(out_pff)

#### Example (2): Pass a list of SpatRasters to `.record`
files <- pf_setup_files(ac_folder)
out_pff <- pf_forward(.obs = obs,
                      .record = files,
                      .n = 1e3,
                      .kick = pf_kick,
                      .bathy = gebco,
                      .save_history = TRUE)

#### Example (3): Write history to file (similar to `acs()`)
pff_folder <- file.path(con, "patter", "pf", "forward")
dir.create(pff_folder, recursive = TRUE)
pf_forward(.obs = obs,
           .record = out_ac$record,
           .n = 1e3,
           .kick = pf_kick,
           .bathy = gebco,
           .save_history = TRUE,
           .write_history = list(sink = pff_folder))
utils::head(pf_setup_files(pff_folder))

#### Example (4): Customise verbose options (as in acs())
# Suppress progress bar
out_pff <- pf_forward(.obs = obs,
                      .record = out_ac$record,
                      .n = 1e3,
                      .kick = pf_kick,
                      .bathy = gebco,
                      .save_history = TRUE,
                      .progress = FALSE)
# Use prompt = TRUE for debugging
if (interactive()) {
  out_pff <- pf_forward(.obs = obs,
                        .record = out_ac$record,
                        .n = 1e3,
                        .kick = pf_kick,
                        .bathy = gebco,
                        .save_history = TRUE,
                        .prompt = TRUE)
}
# Use con to write messages to file
log.txt <- tempfile(fileext = ".txt")
out_pff <-
  pf_forward(.obs = obs,
             .record = out_ac$record,
             .n = 1e3,
             .kick = pf_kick,
             .bathy = gebco,
             .txt = log.txt,
             .save_history = TRUE)
readLines(log.txt) |> utils::head()

#### Example (5): Customise movement model
# Modify `.kick()` to use alternative movement models, e.g.:
# * Use random walk
# * Use correlated random walk
# * Use temporally varying movement model
# * Use spatio-temporal movement model

#### Example (6): Implement the backward pass
# `pf_forward()` should be followed by `pf_backward()`

# Clean up
unlink(file.path(con, "patter"), recursive = TRUE)
