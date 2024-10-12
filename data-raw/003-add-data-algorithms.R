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
                length.out = 360, by = "2 mins")


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
obs <- sim_observations(.timeline = timeline,
                        .model_obs =
                          list(
                            ModelObsAcousticLogisTrunc = arrays |>
                              select(sensor_id = "receiver_id",
                                     "receiver_x", "receiver_y",
                                     "receiver_alpha", "receiver_beta", "receiver_gamma") |>
                              as.data.table(),
                            ModelObsDepthUniform = data.table(sensor_id = 1L,
                                       depth_shallow_eps = 30,
                                       depth_deep_eps = 30)
                          ))

#### Run the COA algorithm
detections <-
  obs$ModelObsAcousticLogisTrunc[[1]] |>
  filter(obs == 1L) |>
  as.data.table()
out_coa <- coa(.map = map,
               .detections = detections,
               .delta_t = "2 hours")

#### Run the particle filter
# Define filter args
args <- list(.timeline = timeline,
             .state = "StateXY",
             .model_move = move_xy(),
             .yobs = list(ModelObsAcousticLogisTrunc = obs$ModelObsAcousticLogisTrunc[[1]],
                          ModelObsDepthUniform = obs$ModelObsDepthUniform[[1]]),
             .n_particle = 1e5L,
             .n_record = 100L)
# Run the filter forwards
args$.direction = "forward"
out_fwd <- do.call(pf_filter, args)
# Run the filter backwards
args$.direction = "backward"
out_bwd <- do.call(pf_filter, args)

#### Run the smoother
out_smo <- pf_smoother_two_filter()


#########################
#########################
#### Update package

#### Collate datasets
dat_path <- paths
dat_coa  <- out_coa
dat_pff  <- out_fwd
dat_pfb  <- out_bwd
dat_tff  <- out_smo
datasets <-
  list(dat_path = dat_path,
       dat_coa = dat_coa,
       dat_pff = dat_pff,
       dat_pfb = dat_pfb,
       dat_tff = dat_tff)

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
} else {
  warn("Datasets not written to file as `overwrite = FALSE`!")
}


#### End of code.
#########################
#########################
