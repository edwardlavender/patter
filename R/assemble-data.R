#' @title Assemble observations
#' @description These functions assembles a timeline and observations for the particle filter.
#' @param .datasets,.step,.trim Arguments for [`assemble_timeline()`].
#' * `.datasets`---A `list` of [`data.table`]s, one for each data type, each containing a `timestamp` column;
#' * `.step`---A `character` (such as `"2 mins"`), passed to [`lubridate::round_date()`] and [`seq.POSIXt()`], that defines the resolution of the timeline;
#' * `.trim`---A `logical` variable that defines whether or not to trim the timeline to the overlapping period between datasets;
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation. Here, `timeline` is used to:
#' * Define the resolution of observations;
#' @param .acoustics For [`assemble_acoustics()`], `.acoustics` is a [data.table] of acoustic detections **for a single individual**.
#' @param .moorings For [`assemble_acoustics()`], `.moorings` is a [`data.table`] of acoustic moorings.
#' @param .archival For [`assemble_archival()`], `.archival` is a [`data.table`] of archival observations (such as depth measurements) **for a single individual**.
#'
#' @details
#' [`assemble_timeline()`] is a simple function that defines a regular timeline, of resolution `.step`, from a `list` of input datasets. If `.trim = FALSE`, this defines a sequence of regular time stamps across the full range of time stamps in the input datasets. If `.trim = TRUE`, the timeline is trimmed to the overlapping period between datasets.
#'
#'  [`assemble_acoustics()`] and [`assemble_archival()`] prepare timelines of acoustic and archival observations as required for the particle filter ([`pf_filter()`]). The filter expects a `list` of datasets (one for each data type). Each dataset must contain the following columns:
#' * `timestamp`---a `POSIXct` vector of time stamps;
#' * `sensor_id`---a vector of sensor IDs, such as receivers;
#' * `obs`---a vector of observations;
#' * Additional columns that define the parameters of the observation model, **in the same order as required by the `ModelObs` structure**;
#'
#' No other columns should be included.
#'
#' * [`assemble_acoustics()`] prepares a timeline of acoustic observations, as required by the filter. This function expects a 'standard' acoustic dataset (that is, a [`data.table`] like [`dat_acoustics`]) that defines detections at receivers alongside a moorings dataset (like [`dat_moorings`]) that defines receiver deployment periods. [`assemble_acoustics()`] uses these datasets to assemble a complete time series of acoustic observations; that is, a [`data.table`] of time stamps and receivers that defines, for each time step and each operational receiver whether (`1L`) or not (`0L`) a detection was recorded at that time step. Duplicate observations (that is, detections at the same receiver in the same time step, are dropped.) If available in `.moorings`, additional columns (`receiver_alpha`, `receiver_beta` and `receiver_gamma`) are included as required for the default acoustic observation model (that is, `ModelObsAcousticLogisTrunc`). If observation model parameters vary both by receiver and through time, simply amend these columns as required.
#'
#' * [`assemble_archival()`] prepares a timeline of archival observations (such as depth measurements), as required by the filter. This function expects a [`data.table`] that includes, at a minimum, the `timestamp` and `obs` columns. For archival data, the `sensor_id` column (if unspecified) is simply set to `1L`. The function re-expresses time stamps at the resolution specified by `timeline`. Duplicate observations (that is, multiple measurements in the same time step) throw a [`warning`].
#'
#' Additional datasets are easy to incorporate into the particle filter but require manual preparation in the format described above. Observations can be recorded irregularly or regularly through time but must be expressed at the temporal resolution defined by the timeline.
#'
#' In `Julia`, datasets are translated into a hash-table (`Dict`) of observations. For each time stamp with an observation, this includes a `Vector` of `Tuple`s, each containing the observation and associated `ModelObs` instance that defines the parameters of the observation model. The particle filter (`Patter.particle_filter()`) iterates over each time step in the timeline, uses a movement model to simulate animal movement and, for the time stamps with observations, evaluates the likelihood of those observations for the simulated locations (particles).
#'
#' These routines are only required for real-world analyses. (In _de novo_ simulations, the hash-table of observations is defined in `Julia` by `simulate_obs()`.)
#'
#' @example
#' @seealso TO DO
#' @author Edward Lavender
#' @name assemble

#' @rdname assemble
#' @export

assemble_timeline <- function(.datasets = list(), .step, .trim = FALSE) {

  # Get time range for each dataset
  check_inherits(.datasets, "list")
  periods <-
    lapply(.datasets, function(dataset) {
      # Check input
      check_names(dataset, "timestamp")
      if (any(is.na(dataset$timestamp))) {
        abort("`timestamp` column(s) should not contain NA(s).")
      }
      # Extract time range
      data.table(start = min(dataset$timestamp), end  = max(dataset$timestamp))
    }) |> rbindlist()

  # Define start and end times
  if ((length(.datasets) == 1L) | isFALSE(.trim)) {
    # Define time extent
    start <- min(periods$start)
    end   <- max(periods$end)
  } else {
    # Define minimum overlapping window
    ints    <- interval(periods$start, periods$end)
    overlap <- ints[1]
    for (i in 2:length(ints)) {
      overlap <- lubridate::intersect(overlap, ints[i])
      if (is.na(overlap)) {
        abort("Dataset timelines do not overlap.")
      }
    }
    start <- int_start(overlap)
    end   <- int_end(overlap)
  }

  # Define timeline
  start <- round_date(start, .step)
  end   <- round_date(end, .step)
  seq(start, end, by = .step)

}

#' @rdname assemble
#' @export

assemble_acoustics <- function(.timeline, .acoustics, .moorings) {

  # Define study time interval
  step     <- diffstep(.timeline)
  time_int <- interval(min(.timeline), max(.timeline))

  # Define moorings
  # * `receiver_start` and `receiver_end` must be POSIXct vectors
  .moorings <-
    .moorings |>
    as.data.frame() |>
    # Focus on receivers within the study interval
    mutate(int = interval(.data$receiver_start, .data$receiver_end)) |>
    filter(int_overlaps(.data$int, time_int)) |>
    select(-"int") |>
    rename(sensor_id = "receiver_id") |>
    as.data.frame()

  if (fnrow(.moorings) == 0L) {
    abort("There are no receiver deployments in `timeline.")
  }

  # Define acoustics
  .acoustics <-
    .acoustics |>
    lazy_dt(immutable = TRUE) |>
    select("timestamp", sensor_id = "receiver_id") |>
    as.data.table()

  # Define detection time series
  detections <-
    .acoustics |>
    filter(.data$timestamp %within% time_int) |>
    mutate(timestamp = lubridate::round_date(.data$timestamp, step)) |>
    group_by(.data$sensor_id, .data$timestamp) |>
    slice(1L) |>
    ungroup() |>
    mutate(obs = 1L) |>
    as.data.table()

  # Define full acoustic time series
  dataset <-
    # Define a sequence of time steps along receiver deployment periods
    .moorings |>
    group_by(.data$sensor_id) |>
    reframe(timestamp = seq(.data$receiver_start, .data$receiver_end, step)) |>
    ungroup() |>
    # Define detection/non-detection by joining with detections
    collapse::join(detections,
                   on = c("sensor_id", "timestamp"),
                   verbose = 0L) |>
    mutate(obs = if_else(is.na(.data$obs), 0L, 1L)) |>
    # Focus on the sections of receiver deployment periods within `time_int`
    filter(.data$timestamp %within% time_int) |>
    arrange(.data$timestamp, .data$sensor_id) |>
    as.data.table()

  # Add acoustic model parameters
  dataset <-
    dataset |>
    join(.moorings |>
           select("sensor_id",
                  "receiver_x", "receiver_y",
                  any_of(c("receiver_alpha", "receiver_beta", "receiver_gamma"))),
         on = "sensor_id",
         verbose = 0L) |>
    select("timestamp", everything()) |>
    as.data.table()

  # Return dataset
  dataset

}

#' @rdname assemble
#' @export

assemble_archival <- function(.timeline, .archival) {

  # Define study time interval
  step     <- diffstep(.timeline)
  time_int <- interval(min(.timeline), max(.timeline))

  # Define sensor ID
  check_names(.archival, c("timestamp", "obs"))
  dataset <- copy(.archival)
  if (!rlang::has_name(dataset, "sensor_id")) {
    dataset <-
      dataset |>
      lazy_dt(immutable = FALSE) |>
      mutate(sensor_id = 1L) |>
      select("timestamp", "sensor_id", everything()) |>
      as.data.table()
  }

  # Define observation timeline
  # * Focus on observations within `time_int`
  # * Round observations to the nearest step
  dataset <-
    dataset |>
    lazy_dt(immutable = FALSE) |>
    filter(.data$timestamp %within% time_int) |>
    mutate(timestamp = lubridate::round_date(.data$timestamp, step)) |>
    as.data.table()

  if (fnrow(dataset) == 0L) {
    abort("There are no archival observations in `timeline`.")
  }

  # Validate observation timeline
  # * There should only be one observation per time step
  count <-
    dataset |>
    group_by(.data$sensor_id, .data$timestamp) |>
    summarise(n = n()) |>
    as.data.table()

  if (any(count$n != 1L)) {
    warn("There are multiple archival observations in one or more time steps.")
  }

  # Return dataset
  dataset

}
