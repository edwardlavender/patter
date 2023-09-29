#### Set up examples

# Define input datasets
acoustics <- dat_acoustics[individual_id == 25, ]
archival <- dat_archival[individual_id == 25, ]
obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
obs <- obs[1:200, ]
gebco <- dat_gebco()

# Implement AC* algorithm
dat_moorings$receiver_range <- 500
containers <- acs_setup_detection_containers(gebco, dat_moorings)
overlaps <- acs_setup_detection_overlaps(containers, dat_moorings)
kernels <-
  acs_setup_detection_kernels(dat_moorings,
                              .calc_detection_pr = acs_setup_detection_pr,
                              .bathy = gebco)
ac_folder <- file.path(tempdir(), "ac")
dir.create(ac_folder)
out_ac <-
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .write_record = list(filename = ac_folder, overwrite = TRUE))

#### Example (1): Implement pf_forward() with default options
out_pf <- pf_forward(.obs = obs,
                     .record = out_ac$record,
                     .n = 1e3,
                     .kick = pf_kick,
                     .bathy = gebco,
                     .save_history = TRUE)
# The function returns a named list:
summary(out_pf)

#### Example (2): Pass a list of SpatRasters to `.record`
spats <- pf_setup_record(ac_folder)
out_pf <- pf_forward(.obs = obs,
                     .record = spats,
                     .n = 1e3,
                     .kick = pf_kick,
                     .bathy = gebco,
                     .save_history = TRUE)

#### Example (3): Write history to file (similar to acs())
pf_folder <- file.path(tempdir(), "pf")
dir.create(pf_folder)
pf_forward(.obs = obs,
           .record = out_ac$record,
           .n = 1e3,
           .kick = pf_kick,
           .bathy = gebco,
           .save_history = TRUE,
           .write_history = list(sink = pf_folder))
utils::head(list.files(pf_folder))

#### Example (4): Customise verbose options (as in acs())
# Suppress progress bar
out_pf <- pf_forward(.obs = obs,
                     .record = out_ac$record,
                     .n = 1e3,
                     .kick = pf_kick,
                     .bathy = gebco,
                     .save_history = TRUE,
                     .progress = FALSE)
# Use prompt = TRUE for debugging
if (interactive()) {
  out_pf <- pf_forward(.obs = obs,
                       .record = out_ac$record,
                       .n = 1e3,
                       .kick = pf_kick,
                       .bathy = gebco,
                       .save_history = TRUE,
                       .prompt = TRUE)
}
# Use con to write messages to file
log.txt <- tempfile(fileext = ".txt")
out_pf <-
  pf_forward(.obs = obs,
             .record = out_ac$record,
             .n = 1e3,
             .kick = pf_kick,
             .bathy = gebco,
             .con = log.txt,
             .save_history = TRUE)
readLines(log.txt) |> utils::head()

#### Example (5): Customise movement model
# Modify `.kick()` to use alternative movement models, e.g.:
# * Use random walk
# * Use correlated random walk
# * Use temporally varying movement model
# * Use spatio-temporal movement model

# Clean up
unlink(ac_folder, recursive = TRUE)
unlink(pf_folder, recursive = TRUE)
