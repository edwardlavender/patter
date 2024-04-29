#' @title PF: set up movement datasets
#' @description This function builds a timeline of observations for particle filtering ([`pf_forward()`]). The filter  iterates over this timeline. At each time step, the filter proposes candidate locations for an individual, evaluates the likelihood of each contemporary observation (e.g., acoustic and/or archival data) in the timeline and optionally (re)samples valid locations using a set of weights that incorporate the likelihood.
#'
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * (optional) `.dlist$data$acoustics`, with the following columns: `timestamp` and `receiver_id`.
#' * (optional) `.dlist$data$archival`, with the following columns: `timestamp` and `depth`.
#' * (optional) If `.dlist$data$acoustics` is provided, `.dlist$algorithm$detection_overlaps` is required if there are receivers with overlapping detection containers;
#'
#' At least one dataset (`.dlist$data$acoustics` and/or `.dlist$data$archival`) must be provided. Data should be given **for a single individual**. Other datasets are not currently integrated within this function but can be included afterwards (see Details).
#'
#' @param .trim If both acoustic and archival data are supplied, `.trim` is `logical` variable that defines whether or not to trim the time series to the time period for which they overlap.
#' @param .step An character, passed to [`lubridate::period()`], [`lubridate::round_date()`] and [`seq()`] that defines the duration between sequential time steps (e.g., `"2 mins"`).
#' @param .period (optional) A length-two `POSIXct` vector that defines the start and end time for the timeline. If unprovided, this is defined internally according to inputted movement datasets and `.trim`:
#' * If only `acoustics = .dlist$data$acoustics` is provided, `.period` is taken as `range(acoustics$timestamp)`.
#' * If only `archival = .dlist$data$archival` is provided, `.period` is taken as `range(archival$timestamp)`.
#' * If both datasets are provided and `.trim = FALSE`, `.period` is defined as `range(c(acoustics$timestamp, archival$timestamp))`.
#' * If both datasets are provided and `.trim = TRUE`, `.period` is defined as the range in timestamps for the overlapping region.
#' @param .mobility A constant that defines the maximum (Euclidean) distance the individual could move in `.step`. See the [`flapper::get_mvt_mobility_*()`](https://edwardlavender.github.io/flapper/reference/get_mvt_mobility.html) functions to estimate `.mobility` from acoustic and/or archival data.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' This function defines the timeline of observations over which [`pf_forward()`] is implemented. The function implements the following routines:
#'
#' * Acoustic progressing (if applicable):
#'    - Acoustic time series are rounded to the nearest `.step`;
#'    - Duplicate detections (at the same receiver in the same time interval) are dropped;
#'
#' * Archival processing (if applicable):
#'    - Archival time series are rounded to the nearest `.step`;
#'    - Multiple records (in the same time interval) are not permitted (in this instance, you need to aggregate observations manually or reduce `.step`);
#'
#' * Alignment (if applicable):
#'    - Acoustic and archival time series are optionally trimmed to the time period for which they overlap;
#'
#' * Time series:
#'    - Acoustic and/or archival time series are mapped onto a regular time series;
#'    - Information required by [`pf_forward()`] (and built-in likelihood functions) is added;
#'
#' At the time of writing, this function only supports acoustic and archival data. However, ancillary observations can easily be included in [`pf_forward()`] as extra columns in this [`data.table`].
#'
#' @return The function returns a [`data.table`] with the following columns:
#' * `timestep`---an `integer` vector that defines the time step;
#' * `timestamp`---a regular sequences of `POSIXct` time stamps;
#' * `mobility`---a `numeric` vector that defines the mobility;
#'
#' If acoustic data is provided, the following column(s) are also included:
#' * `array_id`---an `integer` vector that uniquely distinguishes each array design;
#' * `detection`---an `integer` vector that distinguishes time steps at which detections were (1) or were not (0) recorded;
#' * `acoustics`---a `list` of one-row acoustic observation matrices. In [`pf_forward()`], this information is used to calculate the likelihood of acoustic observations at proposal locations (see [`pf_lik_ac()`]).
#'    * If `detection = 1`, the `matrix` contains one named column for each receiver that recorded a detection and each overlapping receiver. (It is not necessary to include all receivers in this `matrix`.) The entries 1 and 0 distinguish the receivers at which detections were recorded/not recorded. This information is used to calculate the likelihood of acoustic observations at time steps with detection(s).
#'    * If `detection = 0`, the `list` element is `NULL`. We do not need to store empty detection matrices because the likelihood of non detection at all operational receivers in a given array design is extracted from pre-computed layers (see [`acs_setup_detection_kernels()`]).
#' * `container`---a `list` of acoustic container information. In [`pf_forward()`], this information is used to eliminate proposal locations that are too far away from the location(s) of the next detection(s) (see [`acs_filter_container()`]). Each `list` contains:
#'    * `coord`---a two-column `matrix` of coordinates for the receiver(s) that recorded the next detection;
#'    * `buffer`---a `numeric` vector of the (Euclidean) distance from each pair of receiver coordinates within which the individual must have been located at each time step. The distance depends on the time between detections, the individual's `.mobility` and the receiver-specific detection range;
#'
#' If archival data is provided, the following column(s) are also included:
#' * `depth`---a number that defines the individual's depth (m) at each time step;
#'
#' To calculate the likelihood of archival observations in [`pf_forward()`] by the default [`pf_lik_dc()`] routine, a depth envelope (controlled by `depth_deep_eps` and `depth_shallow_eps` columns) is also required (see [`pf_lik_dc()`]).
#'
#' @example man/examples/pf_setup_obs-examples.R
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @export

pf_setup_obs <- function(.dlist,
                         .trim = TRUE,
                         .step,
                         .period = NULL,
                         .mobility,
                         .verbose = getOption("patter.verbose")) {

  #### Set up messages
  t_onset <- Sys.time()
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_setup_obs", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_setup_obs", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Check user inputs
  cat_log("... Checking user inputs...")
  check_dlist(.dlist = .dlist)
  acoustics <- .dlist$data$acoustics
  archival  <- .dlist$data$archival
  if (is.null(acoustics) && is.null(archival)) {
    abort("`acoustics` and/or `archival` should be supplied.")
  }
  check_inherits(.step, "character")

  #### Process acoustic and/or archival time series
  # Process time series
  cat_log("... Processing time series...")
  if (!is.null(acoustics)) {
    acoustics <- .setup_acoustics(.acoustics = acoustics, .step = .step)
  }
  if (!is.null(archival)) {
    archival <- .setup_archival(.archival = archival, .step = .step)
  }
  # Align time series
  if (!is.null(acoustics) && !is.null(archival) && .trim) {
    cat_log("... Aligning time series...")
    aligners <- .setup_alignment(.acoustics = acoustics, .archival = archival)
    acoustics <- aligners$acoustics
    archival  <- aligners$archival
  }

  #### Define template observations timeline
  cat_log("... Defining observations timeline...")
  obs <- .setup_series(.acoustics = acoustics,
                       .archival = archival,
                       .period = .period,
                       .step = .step,
                       .mobility = .mobility)

  #### Add movement parameters
  mobility <- NULL
  obs[, mobility := .mobility]

  #### Add acoustic data
  if (!is.null(acoustics)) {
    # Add detection flag(s) (detection, detection_id, receiver_id)
    cat_log("... Adding acoustic observations: detection IDs...")
    obs <- .add_acoustics_flags(.obs = obs, .acoustics = acoustics)
    # Add array ID(s)
    cat_log("... Adding acoustic observations: array IDs...")
    obs <- .add_acoustics_arrays(.obs = obs, .dlist = .dlist, .step = .step)
    # Add acoustic observations matrices
    cat_log("... Adding acoustic observations: matrices...")
    obs <- .add_acoustics_obs(.obs = obs, .dlist = .dlist, .acoustics = acoustics)
    # Add acoustic containers information
    cat_log("... Adding acoustic observations: containers...")
    obs <- .add_acoustics_containers(.obs = obs, .dlist = .dlist, .mobility = .mobility)
  }

  #### Add archival data
  if (!is.null(archival)) {
    cat_log("... Adding archival observations...")
    obs <- .add_archival(.obs = obs, .archival = archival)
  }

  #### Tidy outputs & return
  cat_log("... Completing call...")
  obs |>
    select("timestep", "timestamp", "mobility",
           any_of(c("array_id", "detection",
                    "acoustics", "container", "depth"))
           ) |>
    as.data.table()

}
