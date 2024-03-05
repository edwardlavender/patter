#' @title PF: set up movement datasets
#' @description This function builds a timeline of observations for particle filtering. The forward filter ([`pf_forward()`]) iterates over this timeline. At each time step, the algorithm proposes candidate locations for an individual, evaluates the likelihood of each observation (e.g., acoustic and/or archival data) in the timeline at the current time step  and (re)samples valid locations using likelihood-based weights.
#'
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * (optional) `.dlist$data$acoustics`, with the following columns: `timestamp` and `receiver_id`.
#' * (optional) `.dlist$data$archival`, with the following columns: `timestamp` and `depth`.
#'
#' At least one dataset must be provided. Data should be given **for a single individual**. Other datasets are not currently integrated within this function but can be included afterwards (see Details).
#'
#' @param .trim If both acoustic and archival data are supplied, `.trim` is `logical` variable that defines whether or not to trim the time series to the time period for which they overlap.
#' @param .step An character, passed to [`lubridate::period()`], [`lubridate::round_date()`] and [`seq()`] that defines the duration between sequential time steps (e.g., `"2 mins"`).
#' @param .period (optional) A length-two `POSIXct` vector that defines the start and end time for the timeline. If unprovided, this is defined internally according to inputted movement datasets and `.trim`:
#' * If only `acoustics = .dlist$data$acoustics` is provided, `.period` is taken as `range(acoustics$timestamp)`.
#' * If only `archival = .dlist$data$archival` is provided, `.period` is taken as `range(archival$timestamp)`.
#' * If both datasets are provided and `.trim = FALSE`, `.period` is defined as `range(c(acoustics$timestamp, archival$timestamp))`.
#' * If both datasets are provided and `.trim = TRUE`, `.period` is defined as the range in timestamps for the overlapping region.
#' @param .mobility A constant that defines the maximum (Euclidean) distance the individual could move in `.step`. See the [`flapper::get_mvt_mobility_*()`](https://edwardlavender.github.io/flapper/reference/get_mvt_mobility.html) functions to estimate `.mobility` from acoustic and/or archival data.
#' @param .receiver_range A constant that defines the receiver detection range. At the time of writing, a constant value across all receivers and time steps is currently assumed (although receiver-specific detection kernels within this range are supported by [`acs_setup_detection_kernels()`]).
#'
#' @details
#' This function defines the timeline of observations over which [`pf_forward()`] is implemented. The function implements the following routines:
#'
#' * Acoustic progressing (if applicable);
#'    - Acoustic time series are rounded to the nearest `.step`;
#'    - All receivers that recorded detection(s) in each time interval are listed;
#'    - Duplicate detections (at the same receiver in the same time interval) are dropped;
#'
#' * Archival processing (if applicable);
#'    - Archival time series are rounded to the nearest `.step`;
#'    - Multiple records (in the same time interval) are not permitted (in this instance, you need to aggregate observations manually or reduce `.step`);
#'
#' * Alignment (if applicable)
#'    - Acoustic and archival time series are optionally trimmed to the time period for which they overlap;
#'
#' * Time series
#'    - Acoustic and/or archival time series are mapped onto a regular time series;
#'    - Information required by [`pf_forward()`] is added;
#'
#' At the time of writing, this function only supports acoustic and archival data. However, ancillary observations can easily be included in [`pf_forward()`] as extra columns in this [`data.table`].
#'
#' @return The function returns a [`data.table`] with the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * `timestamp`---a regular sequences of `POSIXct` time stamps;
#' * `date`---a `character` that defines the date;
#' * `mobility`---a `double` that defines `.mobility`;
#'
#' If acoustic data is provided, the following column(s) are also included:
#' * `detection_id`---an `integer` vector that uniquely defines each detection;
#' * `detection`---an `integer` that distinguishes the time steps at which detections were (1) or were not (0) recorded;
#' * `receiver_id`---a `list` that defines the receiver(s) that recorded detection(s) at each time step;
#' * `receiver_id_next`---a `list` that defines the receiver(s) that recorded the next detection(s);
#' * `buffer_past`---a `double` that controls container growth from the past to the present;
#' * `buffer_future`---a `double` that controls container shrinkage from the future to the present;
#' * `buffer_future_incl_gamma`---a `double` (`buffer_future` + `.receiver_range`) that controls container shrinkage for [`pf_forward()`];
#'
#' These columns are required to calculate the likelihood of acoustic observations (the detection or lack thereof at each operational receiver) in [`pf_forward()`] by the default [`pf_lik`] routines.
#'
#' If archival data is provided, the following column(s) are also included:
#' * `depth`---a number that defines the individual's depth (m) at each time step;
#'
#' To calculate the likelihood of archival observations in [`pf_forward()`] by the default [`pf_lik_dc()`] routine, a depth envelope (controlled by `depth_deep_eps`, `depth_shallow_eps` columns) is also required (see [`pf_lik_dc()`]).
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
                         .mobility) {

  #### Check user inputs
  check_dlist(.dlist = .dlist)
  acoustics <- .dlist$data$acoustics
  archival  <- .dlist$data$archival
  if (is.null(acoustics) && is.null(archival)) {
    abort("`acoustics` and/or `archival` should be supplied.")
  }
  check_inherits(.step, "character")

  #### Process acoustic and/or archival time series
  # Process time series
  if (!is.null(acoustics)) {
    acoustics <- .setup_acoustics(.acoustics = acoustics, .step = .step)
  }
  if (!is.null(archival)) {
    archival <- .setup_archival(.archival = archival, .step = .step)
  }
  # Align time series
  if (!is.null(acoustics) && !is.null(archival) && .trim) {
    aligners <- .setup_alignment(.acoustics = acoustics, .archival = archival)
    acoustics <- aligners$acoustics
    archival  <- aligners$archival
  }

  #### Define template observations timeline
  obs <- .setup_series(.acoustics = acoustics,
                       .archival = archival,
                       .period = .period,
                       .step = .step,
                       .mobility = .mobility)

  #### Add acoustic data
  if (!is.null(acoustics)) {
    # Add detection flag(s) (detection, detection_id, receiver_id)
    obs <- .add_acoustics_flags(.obs = obs, .acoustics = acoustics)
    # Add array ID(s)
    obs <- .add_acoustics_arrays(.obs = obs, .dlist = .dlist, .step = .step)
    # Add acoustic observations matrices
    obs <- .add_acoustics_obs(.obs = obs, .dlist = .dlist, .acoustics = acoustics)
    # Add acoustic containers information
    obs <- .add_acoustics_containers(.obs = obs, .dlist = .dlist, .mobility = .mobility)
  }

  #### Add archival data
  if (!is.null(archival)) {
    obs <- .add_archival(.obs = obs, .archival = archival)
  }

  #### Tidy outputs & return
  obs |>
    select("timestep", "timestamp",
           any_of(c("array_id", "detection",
                    "acoustics", "container", "depth"))
           ) |>
    as.data.table()

}
