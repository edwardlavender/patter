##########################
##########################
#### benchmark.R

#### Aims
# (1) Benchmark key routines through time

#### Prerequisites
# (1) NA

#### (TO DO) Extensions
# (1) Define 'iteration' data.frame with different function arguments
# ... & benchmark different options


##########################
##########################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Load essential packages
devtools::load_all()
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(dv)
library(glue)
library(JuliaCall)

#### Set up Julia
julia_connect()
set_seed()

#### Set up local parameters
overwrite <- FALSE
threads   <- julia_eval('Threads.nthreads()')
timestamp <- Sys.time()

#### Set up helpers
secs <- function(t1, t2) {
  as.numeric(difftime(t1, t2, units = "secs"))
}


##########################
##########################
#### Simulate data

#### Define study period
timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                as.POSIXct("2016-01-15 00:00:00", tz = "UTC"),
                by = "2 mins")

#### Define study area
map <- dat_gebco()
terra::plot(map)
set_map(map)

#### Simulate an acoustic array
moorings <- run(file = here_data_raw("benchmark", "moorings.rds"),
                overwrite = overwrite,
                expr = {
                  sim_array(.map = map,
                            .timeline = timeline,
                            .n_receiver = 100L)
                })

#### Simulate a movement path
# Define movement model
state      <- "StateXY"
mobility   <- 750
model_move <-
  move_xy(.mobility     = mobility,
          .dbn_length   = glue("truncated(Gamma(15.0, 250.0), upper = {mobility})"),
          .dbn_heading  = "Uniform(-pi, pi)")
# Simulate path
path <- run(here_data_raw("benchmark", "paths.rds"),
            overwrite = overwrite,
            expr = {
              sim_path_walk(.map = map,
                            .timeline = timeline,
                            .state = state,
                            .model_move = model_move)
            })

#### Simulate observations
# Define parameters
model_obs <- list(ModelObsAcousticLogisTrunc =
                    moorings |>
                    select(sensor_id = "receiver_id",
                           "receiver_x", "receiver_y",
                           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
                    as.data.table(),
                  ModelObsDepthUniformSeabed =
                    data.table(sensor_id = 1L,
                               depth_shallow_eps = 20,
                               depth_deep_eps = 20))
#### Simulate observations
# Simulate observations
obs <- run(file = here_data_raw("benchmark", "obs.rds"),
           overwrite = overwrite,
           expr = {
             sim_observations(.timeline = timeline,
                              .model_obs = model_obs)
           })
# Extract datasets
acc <- obs$ModelObsAcousticLogisTrunc[[1]]
arc <- obs$ModelObsDepthUniformSeabed[[1]]
# Define acoustic containers
containers <- assemble_acoustics_containers(.timeline = timeline,
                                            .acoustics = acc,
                                            .mobility = mobility,
                                            .map = map)
# Collate datasets
yobs_fwd <- list(ModelObsAcousticLogisTrunc = acc,
             ModelObsDepthUniformSeabed           = arc,
             ModelObsAcousticContainer      = containers$forward)
yobs_bwd <- list(ModelObsAcousticLogisTrunc = acc,
                 ModelObsDepthUniformSeabed       = arc,
                 ModelObsAcousticContainer  = containers$backward)

#### Define filter args
args <- list(.timeline   = timeline,
             .state      = state,
             .xinit      = NULL,
             .model_move = model_move,
             .yobs       = yobs_fwd,
             .n_particle = 2.5e4L,
             .direction  = "forward")

#### Particle filter (forward): ~2.53 min (loop: 2:24)
t1_fwd   <- Sys.time()
fwd      <- do.call(pf_filter, args, quote = TRUE)
t2_fwd   <- Sys.time()
secs_fwd <- secs(t2_fwd, t1_fwd)
if (!fwd$convergence) {
  stop("Forward filter failed to converge.")
}

#### Particle filter (backward): ~2.45 min (loop: 2:24)
# Update args
args$.yobs      <- yobs_bwd
args$.direction <- "backward"
# Run backward filter
t1_bwd   <- Sys.time()
bwd      <- do.call(pf_filter, args, quote = TRUE)
t2_bwd   <- Sys.time()
secs_bwd <- secs(t2_bwd, t1_bwd)
if (!fwd$convergence) {
  stop("Backward filter failed to converge.")
}

#### Particle smoother:
#
# Baseline:
# ~3.80 min (loop: 02:14)
#
# Replace LRUcache with Dict() in two_filter_smoother():
# ~3.35 min (loop: 01:47)
# - Speed of normalisation precomputation is maintained
# - Loop is 27 s faster
# - Smoother is 27 s faster
#
# Tweak Dict() cache generation:
# ~3.40 min (loop: 01:48)
# - Similar speed achieved
#
# Try multithreading smoother
# ~2.8 min (loop: 1:16)
# - Smoothing is ~32 s faster

t1_smo   <- Sys.time()
smo      <- pf_smoother_two_filter(.n_particle = 500L, .n_sim = 100L, .cache = TRUE)
t2_smo   <- Sys.time()
secs_smo <- secs(t2_smo, t1_smo)


##########################
##########################
#### Record timings

#### Collate timings
benchmarks <- data.table(timestamp = timestamp,
                         threads   = threads,
                         routine   = c("fwd", "bwd", "smo"),
                         time      = c(secs_fwd, secs_bwd, secs_smo),
                         note      = "Try multithreading smoother"
                         )

#### Write to file
write.table(benchmarks,
            here_data_raw("benchmark", "benchmarks.txt"),
            append = TRUE,
            sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)


#### End of code.
##########################
##########################
