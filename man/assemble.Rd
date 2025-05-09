% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assemble-data.R
\name{assemble}
\alias{assemble}
\alias{assemble_timeline}
\alias{assemble_acoustics}
\alias{assemble_archival}
\alias{assemble_custom}
\alias{assemble_xinit_containers}
\alias{assemble_acoustics_containers}
\alias{assemble_containers}
\title{Assemble observations}
\usage{
assemble_timeline(.datasets = list(), .step, .trim = FALSE)

assemble_acoustics(.timeline, .detections, .moorings, .services = NULL)

assemble_archival(.timeline, .archival)

assemble_custom(.timeline, .dataset)

assemble_xinit_containers(
  .timeline,
  .xinit = list(),
  .radius,
  .mobility,
  .map = NULL,
  .threshold = NULL
)

assemble_acoustics_containers(
  .timeline,
  .acoustics,
  .mobility,
  .map = NULL,
  .threshold = NULL
)

assemble_containers(...)
}
\arguments{
\item{.datasets, .step, .trim}{Arguments for \code{\link[=assemble_timeline]{assemble_timeline()}}.
\itemize{
\item \code{.datasets}---A \code{list} of \code{\link[data.table:data.table]{data.table::data.table}}s, one for each data type, each containing a \code{timestamp} column;
\item \code{.step}---A \code{character} (such as \code{"2 mins"}), passed to \code{\link[lubridate:round_date]{lubridate::round_date()}} and \code{\link[=seq.POSIXt]{seq.POSIXt()}}, that defines the resolution of the timeline;
\item \code{.trim}---A \code{logical} variable that defines whether or not to trim the timeline to the overlapping period between datasets;
}}

\item{.timeline}{A \code{POSIXct} vector of regularly spaced time stamps that defines the timeline for the simulation (optionally from \code{\link[=assemble_timeline]{assemble_timeline()}}). Here, \code{timeline} is used to:
\itemize{
\item Define the resolution of observations;
}}

\item{.detections, .moorings, .services}{The \code{\link[data.table:data.table]{data.table::data.table}}s for \code{\link[=assemble_acoustics]{assemble_acoustics()}} (see \code{\link[=pat_setup_data]{pat_setup_data()}}).
\itemize{
\item \code{.detections} is a \code{\link[data.table:data.table]{data.table::data.table}} of acoustic detections \strong{for a single individual}. This must contain the \code{receiver_id} and \code{timestamp} columns.
\item \code{.moorings} is a \code{\link[data.table:data.table]{data.table::data.table}} of acoustic receiver deployments. This must contain the \code{receiver_id}, \code{receiver_start}, and \code{receiver_end} columns, plus  (optional) additional parameter columns.
\item (optional) \code{.services} is a \code{\link[data.table:data.table]{data.table::data.table}} of servicing events. This must contain the \code{receiver_id}, \code{service_start} and \code{service_end} columns.
}}

\item{.archival}{For \code{\link[=assemble_archival]{assemble_archival()}}, \code{.archival} is a \code{\link[data.table:data.table]{data.table::data.table}} of depth observations \strong{for a single individual} with \code{timestamp} and \code{obs} columns (see \code{.dataset}, below).}

\item{.dataset}{For \code{\link[=assemble_custom]{assemble_custom()}}, \code{.dataset} is a \code{\link[data.table:data.table]{data.table::data.table}} of observations (such as depth measurements) \strong{for a single individual}. This must contain \code{timestamp} and \code{obs} columns plus (optional) additional parameter columns.}

\item{.xinit, .radius, }{Dataset arguments for \code{\link[=assemble_xinit_containers]{assemble_xinit_containers()}}.
\itemize{
\item \code{.xinit} is a named \code{list}, with elements \code{"forward"} and \code{"backward"}. Elements should be \code{NULL} or a \code{\link[data.table:data.table]{data.table::data.table}} of initial state(s) (i.e., capture or recapture locations) for the corresponding filter run. If capture/recapture locations are known exactly, use a one-row \code{\link[data.table:data.table]{data.table::data.table}}. If starting/ending locations are not known exactly, multi-row \code{\link[data.table:data.table]{data.table::data.table}}s (with all possible starting/ending locations) are permitted.
\item \code{.radius} is a double that defines the radius of the container around the capture/recapture location that particles must reach (in \code{\link[=assemble_acoustics_containers]{assemble_acoustics_containers()}}, \code{.radius = .acoustics$detection_gamma}).
}}

\item{.mobility, .map, .threshold}{Shared container threshold arguments (for \code{\link[=assemble_xinit_containers]{assemble_xinit_containers()}} and \code{\link[=assemble_acoustics_containers]{assemble_acoustics_containers()}}).
\itemize{
\item \code{.mobility} is the maximum movement distance (m) between two time steps (and sets the rate of container contraction).
\item \code{.map}, \code{.threshold} are distance threshold options. Specify \code{.map} or \code{.threshold}:
\itemize{
\item \code{.map} is a two-column \code{matrix} of the four coordinates of the study area or a \code{\link[terra:SpatRaster-class]{terra::SpatRaster}} or \code{\link[terra:SpatVector-class]{terra::SpatVector}} from which such a \code{matrix} can be obtained. On Linux, the latter two options are only possible if \code{JUILA_SESSION = "FALSE"}. \code{.threshold} is set automatically based on the distances between container centroids and the boundaries of the study area.
\item Otherwise, \code{.threshold} is a \code{double} that defines the distance threshold.
}
}}

\item{.acoustics}{Dataset arguments for \code{\link[=assemble_acoustics_containers]{assemble_acoustics_containers()}}.
\itemize{
\item \code{.acoustics} is a \code{\link[data.table:data.table]{data.table::data.table}} of acoustic observations, from \code{\link[=assemble_acoustics]{assemble_acoustics()}}.
}}

\item{...}{For \code{\link[=assemble_containers]{assemble_containers()}}, \code{...} represents container \code{list}s for multiple data types, such as capture events (\code{\link[=assemble_xinit_containers]{assemble_xinit_containers()}}) and acoustic observations (\code{\link[=assemble_acoustics_containers]{assemble_acoustics_containers()}}).}
}
\value{
\itemize{
\item \code{\link[=assemble_timeline]{assemble_timeline()}} returns a POSIXct vector;
\item \code{\link[=assemble_acoustics]{assemble_acoustics()}}, \code{\link[=assemble_archival]{assemble_archival()}} and \code{\link[=assemble_custom]{assemble_custom()}} return a \code{\link[data.table:data.table]{data.table::data.table}} for \code{\link[=pf_filter]{pf_filter()}};
\item \code{\link[=assemble_xinit_containers]{assemble_xinit_containers()}}, \code{\link[=assemble_acoustics_containers]{assemble_acoustics_containers()}} and \code{\link[=assemble_containers]{assemble_containers()}} return a named \code{list}, with one element (\code{\link[data.table:data.table]{data.table::data.table}}) for (a) the forward and (b) the backward runs of \code{\link[=pf_filter]{pf_filter()}};
}
}
\description{
These functions assemble a timeline and observations for the particle filter (\code{\link[=pf_filter]{pf_filter()}}).
}
\section{Assemble timeline}{
\code{\link[=assemble_timeline]{assemble_timeline()}} is a simple function that defines a regular timeline, of resolution \code{.step}, from a \code{list} of input datasets.
\itemize{
\item If \code{.trim = FALSE}, this defines a sequence of regular time stamps across the full range of time stamps in the input datasets.
\item If \code{.trim = TRUE}, the timeline is trimmed to the overlapping period between datasets.
}
}

\section{Assemble datasets}{
\verb{assemble_\{dataset\}()} functions are helper routines that prepare timelines observations for different data types as required for the particle filter (\code{\link[=pf_filter]{pf_filter()}}). The filter expects a named \code{list} of datasets (one for each data type). Each dataset must contain the following columns:
\itemize{
\item \code{timestamp}
\item \code{sensor_id}
\item \code{obs}
\item Additional columns with the parameters of the observation model (see \code{\link{glossary}}).
}

\code{\link[=assemble_acoustics]{assemble_acoustics()}}, \code{\link[=assemble_archival]{assemble_archival()}} and \code{\link[=assemble_custom]{assemble_custom()}} assemble 'standard' observational time series:
\enumerate{
\item \code{\link[=assemble_acoustics]{assemble_acoustics()}} prepares a timeline of acoustic observations as required by the filter \strong{for a single individual}. This function expects a 'standard' detection dataset (that is, a \code{\link[data.table:data.table]{data.table::data.table}} like \code{\link{dat_detections}} but for a single individual) that defines detections at receivers alongside a moorings dataset (like \code{\link{dat_moorings}}) that defines receiver deployment periods and, optionally, a \code{\link[data.table:data.table]{data.table::data.table}} of servicing events (when receiver(s) were non-operational). \code{\link[=assemble_acoustics]{assemble_acoustics()}} uses these datasets to assemble a complete time series of acoustic observations; that is, a \code{\link[data.table:data.table]{data.table::data.table}} of time stamps and receivers that defines, for each time step and each operational receiver whether (\code{1L}) or not (\code{0L}) a detection was recorded at that time step. Duplicate observations (that is, detections at the same receiver in the same time step) are dropped. If available in \code{.moorings}, additional columns (\code{receiver_alpha}, \code{receiver_beta} and \code{receiver_gamma}) are included as required for the default acoustic observation model (that is, \code{\link{ModelObsAcousticLogisTrunc}}). If observation model parameters vary both by receiver and through time, simply amend these columns as required.
\item \code{\link[=assemble_archival]{assemble_archival()}} prepares a timeline of archival functions \strong{for a single individual}. This simply wraps \code{\link[=assemble_custom]{assemble_custom()}} and is informally deprecated.
\item \code{\link[=assemble_custom]{assemble_custom()}} prepares a timeline of observations for other data types, as required by the filter. This function expects a \code{\link[data.table:data.table]{data.table::data.table}} that includes, at a minimum, the \code{timestamp} and \code{obs} columns. The latter defines the observations. The \code{sensor_id} column (if unspecified) is simply set to \code{1L}. The function re-expresses time stamps at the resolution specified by \code{timeline}. Duplicate observations (that is, multiple measurements in the same time step) throw a \code{\link{warning}}.
}
}

\section{Assemble containers}{
\code{\link[=assemble_containers]{assemble_containers()}}, \code{\link[=assemble_xinit_containers]{assemble_xinit_containers()}} and \code{\link[=assemble_acoustics_containers]{assemble_acoustics_containers()}} assemble 'container' datasets. A container is a circular region within which an individual must be located, given:
\itemize{
\item A 'future' observation in a particular place, such as a capture/recapture event or an acoustic detection;
\item The time until that observation;
\item An individual's maximum movement speed (\code{.mobility});
}

As we approach a 'future' observation, the container within which an individual must be located, according to that observation, shrinks towards the location in which the observation was recorded. Encoding this knowledge in the particle filter (by killing particles outside of a container that are incompatible with a future observation) facilitates convergence with fewer particles, assuming regular resampling (see \code{\link[=pf_filter]{pf_filter()}}). (Otherwise, we rely on some particles ending up in the right region by chance, which can be unlikely if the region in which particles must end up is small (e.g., a receiver's detection container).)

Consider acoustic containers as an example. Suppose an individual can move up to \code{.mobility} = 500 m per time step, and two time steps elapse between the first and second detections, then at the moment of the first detection the individual must be within 1000 m of the detection range (say, \code{receiver_gamma} = 750 m) of the second receiver; that is, at the moment of first detection, the maximum possible distance of the individual from the receiver that recorded the next detection is 1750 m. As time passes, the container shrinks towards the receiver(s) that recorded the next detection(s), in line with the individual's \code{.mobility}. Pro-actively killing particles that move outside these containers (and are incompatible with the next detection) facilitates convergence.

The following functions assemble containers:
\enumerate{
\item \code{\link[=assemble_xinit_containers]{assemble_xinit_containers()}} assembles containers for initial capture or recapture events. This is designed for situations in which you know the starting and/or ending location for an individual. Suppose you know the starting location of an individual. We can specify this in the forward filter run via \code{.xinit} in \code{\link[=pf_filter]{pf_filter()}}. But what about the backward filter run? For particle smoothing (via \code{\link[=pf_smoother_two_filter]{pf_smoother_two_filter()}}), we need a forward and a backward filter run and these must align sufficiently. Containers provide a mechanism that encourages the backward filter run to end up in the known starting location. Similarly, if we know a recapture location, containers in the forward filter run provide a mechanism that encourages the filter to end up in that location. In \code{\link[=assemble_xinit_containers]{assemble_xinit_containers()}}, \code{.xinit} defines the initial locations for the forward/backward filter runs and \code{.radius} defines the radius of the starting/ending container. If you know capture/recapture locations exactly, strictly speaking \code{.radius = 0}. But it may help to permit some flexibility (e.g., \code{.radius = .mobility}) to facilitate convergence (it is unlikely that particles will finish in exactly the right location). If capture/recapture locations are not known exactly, the centroid of the input coordinates is taken as the container's centroid. The container radius at the time of capture/recapture event is given by the maximum distance from the centroid to any of the coordinates plus \code{.radius}. As time passes, containers shrink towards this region in line with \code{.mobility}.
\item \code{\link[=assemble_acoustics_containers]{assemble_acoustics_containers()}} prepares a dataset of acoustic containers, given the acoustic time series from \code{\link[=assemble_acoustics]{assemble_acoustics()}}. Acoustic containers define the region within which an individual must be located at a given time step according to the receiver(s) at which it was next detected. The radius depends on the time until the next detection, the maximum movement speed and the detection range around the receiver at the time of the detection. This function requires the \code{\link[tidyr:nest]{tidyr::nest()}}, \code{\link[tidyr:unnest]{tidyr::unnest()}} and \code{\link[zoo:na.locf]{zoo::na.locf()}} functions (suggested dependencies).
\item \code{\link[=assemble_containers]{assemble_containers()}} is a post-processing function. Use this function to collate the container \code{\link[data.table:data.table]{data.table::data.table}}s from multiple datasets, i.e., if you have capture/recapture and acoustic containers.
}

All \verb{assemble_*_containers()} functions assemble a \code{list} of \code{\link[data.table:data.table]{data.table::data.table}}s (with one element for the forward filter run and one element for the backward filter run). Each row defines the maximum distance (\code{radius}) of the individual from the location in which a future observation was recorded. For computational efficiency,  \code{\link[data.table:data.table]{data.table::data.table}}s only include containers with a \code{radius} < \code{.threshold}. If \code{.map} is supplied, the \code{.threshold} is set to the maximum distance between each location (e.g., receiver) and the furthest corner of the study area. Otherwise, set the .\code{threshold} to the desired value. The \code{\link[data.table:data.table]{data.table::data.table}}s are used to instantiate a Vector of \code{\link{ModelObsContainer}} instances in \code{Julia}. (Only one sub-type is used for all container structures for speed.) In the particle filter (\code{\link[=pf_filter]{pf_filter()}}), for each particle, we compute the log-probability of the particle from the distance of the particle from the relevant location (0.0 or -Inf). By re-sampling particles with replacement, particles that move in a way that is incompatible with the future data (e.g., a detection) are killed.
}

\section{\code{Julia} implementation}{
In \code{Julia}, datasets are translated into a hash-table (\code{Dict}) of observations (via \href{https://edwardlavender.github.io/Patter.jl/}{\code{Patter.assemble_yobs()}}). For each time stamp with an observation, this includes a \code{Vector} of \code{Tuple}s, each containing the observation and the associated \code{\link{ModelObs}} instance that defines the parameters of the observation model. The particle filter (\href{https://edwardlavender.github.io/Patter.jl/}{\code{Patter.particle_filter()}}) iterates over each time step in the timeline, uses a movement model to simulate animal movement and, for the time stamps with observations, evaluates the likelihood of those observations for the simulated locations (particles).

\verb{assemble_*()} routines are only required for real-world analyses.
}

\examples{
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
}
\seealso{
Particle filters and smoothers sample states (particles) that represent the possible locations of an individual through time, accounting for all data and the individual's movement.
\itemize{
\item To simulate artificial datasets, see \verb{sim_*()} functions (especially \code{\link[=sim_path_walk]{sim_path_walk()}}, \code{\link[=sim_array]{sim_array()}} and \code{\link[=sim_observations]{sim_observations()}}).
\item To assemble real-world datasets for the filter, see \code{\link{assemble}}\verb{_*()} functions.
\item \code{\link[=pf_filter]{pf_filter()}} runs the filter:
\itemize{
\item For state types, see \code{\link{State}};
\item For observation models, see \code{\link{ModelObs}};
\item For movement models, see \code{\link{ModelMove}};
}
\item To run particle smoothing, use \code{\link[=pf_smoother_two_filter]{pf_smoother_two_filter()}}.
\item To map emergent patterns of space use, use a \verb{map_*()} function (such as \code{\link[=map_pou]{map_pou()}}, \code{\link[=map_dens]{map_dens()}} and \code{\link[=map_hr]{map_hr()}}).
}
}
\author{
Edward Lavender
}
