#' @title PF: set up movement datasets
#' @description This function builds a timeline of observations for particle filtering. The forward filter ([`pf_forward()`]) iterates over this timeline. At each time step, the algorithm proposes candidate locations for an individual, evaluates the likelihood of each observation in the timeline (e.g., acoustic and/or archival observations) and (re)samples valid locations using likelihood-based weights.
#'
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * (optional) `.dlist$data$acoustics`, with the following columns: `timestamp` and `receiver_id`.
#' * (optional) `.dlist$data$archival`, with the following columns: `timestamp` and `depth`.
#'
#' At least one dataset must be provided. Data should be given **for a single individual**. Other datasets are not currently integrated within this function but can be included afterwards (see Details).
#'
#' @param .trim If both acoustic and archival data are supplied, `.trim` is `logical` variable that defines whether or not to trim the time series to the time period for which they overlap.
#' @param .step An character, passed to [`lubridate::period()`], [`lubridate::round_date()`] and [`seq()`] that defines the duration between sequential time steps (e.g., `"2 mins"`).
#' @param .period (optional) A length-two `POSIXct` vector that defines the start and end time of the observations. If unprovided, this is defined internally according to inputted movement datasets and `.trim`.
#' @param .mobility A constant that defines the maximum (Euclidean) distance the individual could move in `.step`.
#' @param .detection_range A constant that defines the detection range. A constant value across all receivers and time steps is assumed.
#'
#' @details
#' This function defines the timeline of observations over which [`pf_forward()`] is implemented.The function implements the following routines:
#'
#' * Acoustic progressing (if applicable);
#'    - Acoustic time series are rounded to the nearest `.step`;
#'    - All receivers that recorded detection(s) in each time interval are listed;
#'    - Duplicate detections (at the same receiver in the same time interval) are dropped;
#'
#' * Archival processing (if applicable);
#'    - Archival time series are rounded to the nearest `.step`;
#'    - Duplicate records (in the same time interval) are not permitted;
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
#' * `buffer_future_incl_gamma`---a `double` (`buffer_future` + `.detection_range`) that controls container shrinkage for [`pf_forward()`];
#'
#' These columns are required to calculate the likelihood of acoustic observations (the detection or lack thereof at each operational receiver) in [`pf_forward()`] by the default [`pf_lik`] routines.
#'
#' If archival data is provided, the following column(s) are also included:
#' * `depth`---a number that defines the individual's depth (m) at each time step;
#'
#' To calculate the likelihood of archival observations in [`pf_forward()`] by the default [`pf_lik_dc()`] routine, a depth envelope (as defined by `depth_deep` and `depth_shallow` columns) is also required (see [`pf_lik_dc()`]).
#'
#' @example man/examples/pf_setup_obs-examples.R
#'
#' @seealso
#' For [`pf_setup_obs()`] specifically:
#'  * [`process_receiver_ids`](https://edwardlavender.github.io/flapper/reference/process_receiver_id.html) in the [`flapper`](https://edwardlavender.github.io/flapper/reference/process_receiver_id.html) package can be used to define receiver deployments using an integer vector.
#'
#' @author Edward Lavender
#' @export

pf_setup_obs <- function(.dlist,
                         .trim = TRUE,
                         .step,
                         .period = NULL,
                         .mobility,
                         .detection_range) {

  #### Check user inputs
  check_dlist(.dlist = .dlist)
  acoustics <- .dlist$data$acoustics
  archival  <- .dlist$data$archival
  if (is.null(acoustics) && is.null(archival)) {
    abort("`acoustics` and/or `archival` should be supplied.")
  }
  check_inherits(.step, "character")

  #### Process acoustics
  # * Round time series & drop duplicates
  # * List receivers with detections
  if (!is.null(acoustics)) {
    acoustics <-
      acoustics |>
      lazy_dt(immutable = TRUE) |>
      # Round time series & drop duplicates
      mutate(timestamp = lubridate::round_date(.data$timestamp, .step)) |>
      group_by(.data$receiver_id, .data$timestamp) |>
      slice(1L) |>
      ungroup() |>
      # List receivers with detections at each time step
      group_by(.data$timestamp) |>
      summarise(receiver_id = list(unique(.data$receiver_id))) |>
      ungroup() |>
      arrange(timestamp) |>
      # Add additional columns
      mutate(detection_id = as.integer(row_number())) |>
      as.data.table()
  }

  #### Process archival time series
  if (!is.null(archival)) {
    timestamp <- NULL
    archival[, timestamp := lubridate::round_date(timestamp, .step)]
  }

  #### Align time series
  if (!is.null(acoustics) && !is.null(archival) && .trim) {
    start <- max(c(min(acoustics$timestamp), min(archival$timestamp)))
    end   <- min(c(max(acoustics$timestamp), max(archival$timestamp)))
    acoustics <-
      acoustics |>
      filter(.data$timestamp >= start & .data$timestamp <= end) |>
      as.data.table()
    archival <-
      archival |>
      filter(.data$timestamp >= start & .data$timestamp <= end) |>
      as.data.table()
    if (nrow(acoustics) == 0L | nrow(archival) == 0L) {
      abort("There are no remaining observations after aligning the acoustic and archival time series.")
    }
  }

  #### Define output time series
  # Define regular time series
  if (!is.null(.period)) {
    start <- min(.period)
    end   <- max(.period)
  } else {
    timestamps <- list(acoustics[["timestamp"]], archival[["timestamp"]])
    timestamps <- list_compact(timestamps)
    if (length(timestamps) == 1L) {
      timestamps <- timestamps[[1]]
    } else {
      timestamps <- c(timestamps[[1]], timestamps[[2]])
    }
    start <- min(timestamps)
    end   <- max(timestamps)
  }
  tss <- seq(start, end, by = .step)
  out <- data.table(timestep = seq_len(length(tss)),
                    timestamp = tss,
                    date = as.character(as.Date(tss)),
                    mobility = .mobility)
  # Add acoustic data
  if (!is.null(acoustics)) {
    out <-
      out |>
      mutate(detection = as.integer((timestamp %in% acoustics$timestamp) + 0)) |>
      merge(acoustics, all.x = TRUE, by = "timestamp") |>
      mutate(detection_id = as.integer(data.table::nafill(.data$detection_id, type = "locf"))) |>
      group_by(.data$detection_id) |>
      # Define buffers
      mutate(step_forwards = row_number(),
             step_backwards = rev(.data$step_forwards),
             # We buffer the past by mobility
             buffer_past = .mobility,
             # We shrink the future
             buffer_future = .mobility * .data$step_backwards,
             buffer_future_incl_gamma = .data$buffer_future + .detection_range) |>
      ungroup() |>
      arrange(.data$timestamp) |>
      mutate(timestep = as.integer(row_number()),
             receiver_id_next = .pf_setup_obs_receiver_id_next(.data$receiver_id)
      ) |>
      as.data.table()
  }
  # Add archival data
  if (!is.null(archival)) {
    depth <- timestamp <- NULL
    out[, depth := archival$depth[match(timestamp, archival$timestamp)]]
    bool <- is.na(out$depth)
    if (any(bool)) {
      nrw <- fnrow(out)
      nob <- nrw - length(which(bool))
      warn("There are {nob}/{nrw} ({round(nob / nrw * 100, digits = 1)} %) archival observations within the time series.",
           .envir = environment())
      if (all(bool)) {
        warn("The depth column has been dropped.")
        out[, depth := NULL]
      }
    }
  }

  #### Tidy outputs & return
  out |>
    select("timestep",
           "timestamp", "date",
           any_of(c("detection_id", "detection", "receiver_id", "receiver_id_next",
                    "mobility", "buffer_past", "buffer_future",
                    "buffer_future_incl_gamma", "depth"))
    ) |>
    as.data.table()

}
