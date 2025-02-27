if (patter_run(.julia = FALSE, .geospatial = TRUE)) {

  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)

  #### Define example dataset(s) for a selected individual
  # Study area map
  map <- dat_gebco()
  # Acoustic detection time series
  # * Observation model parameters are defined in `.moorings`
  det <-
    dat_detections |>
    filter(individual_id == 25L) |>
    select("timestamp", "receiver_id") |>
    as.data.table()
  # Archival time series
  # * Observation model parameters must be included
  # * Here, we define parameters for `?ModelObsDepthNormalTruncSeabed`
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
  timeline <- assemble_timeline(list(det, arc), .step = "2 mins")
  range(timeline)
  # Use `assemble_timeline()` with `.trim = TRUE`
  timeline <- assemble_timeline(list(det, arc), .step = "2 mins", .trim = TRUE)
  timeline <- timeline[1:1440]
  range(timeline)

  #### Example (2): Assemble an acoustic timeline
  # Assemble a timeline of acoustic observations (0, 1) and model parameters
  # * The default acoustic observation model parameters are taken from `.moorings`
  # * But can be modified or added afterwards for custom observation models
  acoustics <- assemble_acoustics(.timeline = timeline,
                                  .detections = det,
                                  .moorings = dat_moorings)
  head(acoustics)

  #### Example (3): Assemble an archival timeline
  # Assemble a timeline of archival observations and model parameters
  archival <- assemble_archival(.timeline = timeline,
                                .archival = arc)
  head(archival)

  #### Example (4): Assemble custom datasets
  temperature <-
    data.table(timestamp = c(as.POSIXct("2016-03-17 01:50:30", tz = "UTC"),
                             as.POSIXct("2016-03-17 02:00:30 UTC", tz = "UTC")),
               obs = c(7.6, 7.7))
  temperature <- assemble_custom(.timeline = timeline,
                                 .dataset = temperature)
  head(temperature)

  #### Example (5): Assemble xinit (capture/recapture) containers
  # (A) If we know the capture location, containers are defined for the backward filter run
  capture_xy       <- data.table(x = 708913.6, y = 6256280)
  xinit_containers <-
    assemble_xinit_containers(.timeline = timeline,
                              .xinit = list(forward = capture_xy,
                                            backward = NULL),
                              .radius = 750,
                              .mobility = 750,
                              .map = map)
  # (B) If we know the recapture location, containers are defined for the forward run
  recapture_xy     <- data.table(x = 707816.5, y = 6265746)
  xinit_containers <-
    assemble_xinit_containers(.timeline = timeline,
                              .xinit = list(forward = NULL,
                                            backward = recapture_xy),
                              .radius = 750,
                              .mobility = 750,
                              .map = map)
  # (C) If we know capture & recapture locations, containers are defined for both filter runs
  xinit_containers <-
    assemble_xinit_containers(.timeline = timeline,
                              .xinit = list(forward = capture_xy,
                                            backward = recapture_xy),
                              .radius = 750,
                              .mobility = 750,
                              .map = map)
  # (D) A set of possible capture/recapture locations is permitted:
  # (i) Define possible starting locations
  capture_xy <-
    cbind(capture_xy$x, capture_xy$y) |>
    terra::vect(crs = terra::crs(map)) |>
    terra::buffer(width = 500) |>
    terra::spatSample(size = 10L) |>
    terra::crds(df = TRUE) |>
    setDT()
  # (ii) Define possible recapture locations
  recapture_xy <-
    cbind(recapture_xy$x, recapture_xy$y) |>
    terra::vect(crs = terra::crs(map)) |>
    terra::buffer(width = 500) |>
    terra::spatSample(size = 10L) |>
    terra::crds(df = TRUE) |>
    setDT()
  # (iii) Define containers
  xinit_containers <-
    assemble_xinit_containers(.timeline = timeline,
                              .xinit = list(forward = capture_xy,
                                            backward = recapture_xy),
                              .radius = 750,
                              .mobility = 750,
                              .map = map)

  #### Example (6): Assemble acoustic containers
  # Assemble acoustic containers for the `acoustics` dataset above
  acoustics_containers <- assemble_acoustics_containers(.timeline = timeline,
                                              .acoustics = acoustics,
                                              .mobility = 750,
                                              .map = map)
  # As for assemble_xinit_containers(), this function returns a list:
  summary(acoustics_containers)
  # Use the `forward` element for `pf_filter()` with `.direction = "forward"`
  head(acoustics_containers$forward)
  # Use the `backward` for `pf_filter()` with `.direction = "backward"`
  head(acoustics_containers$backward)

  #### Example (7): Collate containers for multiple datasets
  containers <- assemble_containers(xinit_containers, acoustics_containers)

  #### Example (8): Implement particle filter
  # Use `pf_filter()` to implement the particle filter
  # A list of assembled datasets is passed to the `yobs` argument
  # The corresponding `ModelObs` sub-types must also be specified, e.g.:
  # * `ModelObsAcousticLogisTrunc`
  # * `ModelObsDepthUniformSeabed`
  # * `ModelObsContainer`

}
