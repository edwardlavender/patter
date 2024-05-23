#########################
#########################
#### add-data-algorithms.R

#### Aims
# 1) Prepare algorithm datasets (used to streamline examples)

#### Prerequisites
# 1) NA


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())
try(pacman::p_unload("all"), silent = TRUE)
dv::clear()

#### Essential packages
devtools::load_all()
library(lubridate)

#### Global variables
overwrite <- TRUE


#########################
#########################
#### Set up

#### Connect to Julia
julia_connect()
set_seed()

#### Define map
map <- dat_gebco()
set_map(map)

#### Define study period
timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                length.out = 720L, by = "2 mins")


#########################
#########################
#### Simulate datasets

#### Simulate path(s)
paths <- sim_path_walk(.map = map,
                       .timeline = timeline,
                       .state = "StateXY",
                       .model_move = move_xy())

#### Simulate array(s)
arrays <- sim_array(.map = map,
                    .timeline = timeline,
                    .n_receiver = 500L)

#### Simulate observation(s)
models <- c("ModelObsAcousticLogisTrunc", "ModelObsDepthUniform")
obs <- sim_observations(.timeline = timeline,
                        .model_obs = models,
                        .model_obs_pars =
                          list(
                            arrays |>
                              select(sensor_id = "receiver_id",
                                     "receiver_x", "receiver_y",
                                     "receiver_alpha", "receiver_beta", "receiver_gamma") |>
                              as.data.table(),
                            data.table(sensor_id = 1L,
                                       depth_shallow_eps = 20,
                                       depth_deep_eps = 20)
                          ))

#### Run the COA algorithm
# TO DO
# Improve alignment between coa() & pf_*() function output
detections <-
  obs$ModelObsAcousticLogisTrunc[[1]] |>
  filter(obs == 1L) |>
  as.data.table()
out_coa <- coa(.acoustics = detections,
               .delta_t = "2 hours")

#### Run the particle filter
# Run the filter forwards
out_pff <-
  pf_filter(.map = map,
            .timeline = timeline,
            .state = "StateXY",
            .xinit_pars = list(mobility = 750),
            .model_move = move_xy(),
            .yobs = list(obs$ModelObsAcousticLogisTrunc[[1]], obs$ModelObsDepthUniform[[1]]),
            .model_obs = c("ModelObsAcousticLogisTrunc", "ModelObsDepthUniform"),
            .n_record = 250L)
# Run the filter backwards
# TO DO

#### Run the smoother
# TO DO


#########################
#########################
#### Update package

#### Collate datasets
dat_coa <- out_coa
dat_pff <- out_pff
datasets <-
  list(dat_coa = dat_coa,
       dat_pff = dat_pff)

#### Check dataset sizes (MB)
# ./data/
datasets |>
  sapply(\(dataset) {
    # Save file
    con <- tempfile(fileext = ".rda")
    saveRDS(dataset, con)
    # Define file size in MB
    file.size(con) / 1e6L
  }) |>
  sum()
# ./inst/
list.files("inst", recursive = TRUE, full.names = TRUE) |>
  file.size() |>
  sum() / 1e6L

#### Write datasets to file
# We will save all datasets in inst/ for consistency
# and use functions to read (and, if necessary, unpack) datasets
if (overwrite) {
  lapply(seq_len(length(datasets)), function(i) {
    saveRDS(datasets[[i]], here::here("inst", "extdata", paste0(names(datasets)[i], ".rds")))
  }) |> invisible()
}


#### End of code.
#########################
#########################
