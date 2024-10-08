library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

#### Define example dataset(s) for a selected individual
# Acoustic time series
# * Observation model parameters are defined in `.moorings`
acc <-
  dat_acoustics |>
  filter(individual_id == 25L) |>
  select("timestamp", "receiver_id") |>
  as.data.table()
# Archival time series
# * Observation model parameters must be included
# * Here, we define parameters for `?ModelObsDepthNormalTrunc`
arc <-
  dat_archival |>
  filter(individual_id == 25L) |>
  select("timestamp", obs = "depth") |>
  mutate(depth_sigma = 50, depth_deep_eps = 20) |>
  as.data.table()

#### Example (1): Define a timeline
# Define a timeline manually
timeline <- seq(as.POSIXct("2016-03-01 00:00:00", tz = "UTC"),
                as.POSIXct("2016-04-01 00:00:00"),
                by = "2 mins")
# Use `assemble_timeline()` with `.trim = FALSE`
timeline <- assemble_timeline(list(acc, arc), .step = "2 mins")
range(timeline)
# Use `assemble_timeline()` with `.trim = TRUE`
timeline <- assemble_timeline(list(acc, arc), .step = "2 mins", .trim = TRUE)
timeline <- timeline[1:1440]
range(timeline)

#### Example (2): Assemble an acoustic timeline
# Assemble a timeline of acoustic observations (0, 1) and model parameters
# * The default acoustic observation model parameters are taken from `.moorings`
# * But can be modified or added afterwards for custom observation models
acoustics <- assemble_acoustics(.timeline = timeline,
                                .acoustics = acc,
                                .moorings = dat_moorings)
head(acoustics)

#### Example (3): Assemble an archival timeline
# Assemble a timeline of archival observations and model parameters
archival <- assemble_archival(.timeline = timeline,
                              .archival = arc)
head(archival)

#### Example (4): Implement particle filter
# Use `pf_filter()` to implement the particle filter
# A list of assembled datasets is passed to the `yobs` argument
# The corresponding `ModelObs` sub-types must also be specified
