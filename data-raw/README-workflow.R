library(patter)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

#### Julia set up
julia_connect()
set_seed()

#### Define map
map <- dat_gebco()
set_map(map)

#### Identify datasets

# Define acoustic detections
acc <-
  dat_acoustics |>
  filter(individual_id == 25L) |>
  mutate(individual_id = NULL) |>
  as.data.table()

# Define archival (depth) observations
arc <-
  dat_archival |>
  filter(individual_id == 25L) |>
  mutate(individual_id = NULL,
         depth_sigma = 50,
         depth_deep_eps = 20) |>
  rename(obs = depth) |>
  as.data.table()

# Define a timeline for the simulation
timeline <-
  list(acc, arc) |>
  assemble_timeline(.step = "2 mins", .trim = TRUE)
timeline <- timeline[1:720]

#### Observation models

model_1   <- "ModelObsAcousticLogisTrunc"
acoustics <- assemble_acoustics(.timeline = timeline,
                                .acoustics = acc,
                                .moorings = dat_moorings)

model_2  <- "ModelObsDepthNormalTrunc"
archival <- assemble_archival(.timeline = timeline,
                              .archival = arc)


#### Movement models

state      <- "StateXY"
mobility   <- 750.0
model_move <- move_xy(dbn_length = glue::glue("truncated(Gamma(1, 250.0), upper = {mobility})"),
                      dbn_angle = "Uniform(-pi, pi)")

# Visualise the movement model
sim_path_walk(.map = map,
              .timeline = timeline,
              .state = state,
              .model_move = model_move)

#### Particle filter
# Define arguments
args <- list(.map = map,
             .timeline = timeline,
             .state = state,
             .xinit_pars = list(mobility = mobility),
             .yobs = list(acoustics, archival),
             .model_obs = c(model_1, model_2),
             .model_move = model_move,
             .n_record = 500L,
             .n_particle = 1e4L)
# Forward run
fwd <- do.call(pf_filter, args)
# Backward run
args$.direction <- "backward"
bwd <- do.call(pf_filter, args)

#### Two filter smoother
# smo <- pf_smoother_two_filter(.map = map, .nMC = 1)

#### Map
map_dens(.map = map,
         .coord = fwd$states,
         .discretise = TRUE,
         sigma = spatstat.explore::bw.diggle)
