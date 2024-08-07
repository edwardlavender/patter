#' @title Assemble observations
#' @description These functions assemble a timeline and observations for the particle filter ([`pf_filter()`]).
#' @param .datasets,.step,.trim Arguments for [`assemble_timeline()`].
#' * `.datasets`---A `list` of [`data.table`]s, one for each data type, each containing a `timestamp` column;
#' * `.step`---A `character` (such as `"2 mins"`), passed to [`lubridate::round_date()`] and [`seq.POSIXt()`], that defines the resolution of the timeline;
#' * `.trim`---A `logical` variable that defines whether or not to trim the timeline to the overlapping period between datasets;
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation (optionally from [`assemble_timeline()`]). Here, `timeline` is used to:
#' * Define the resolution of observations;
#' @param .acoustics,.moorings,.services The [data.table]s for [`assemble_acoustics()`] (see [`pat_setup_data()`]).
#' * `.acoustics` is a [data.table] of acoustic detections **for a single individual**. This must contain the `receiver_id` and `timestamp` columns.
#' * `.moorings` is a [`data.table`] of acoustic receiver deployments. This must contain the `receiver_id`, `receiver_start`, and `receiver_end` columns, plus additional parameter columns.
#' * `.services` is a [`data.table`] of servicing events. This must contain the `receiver_id`, `service_start` and `service_end` columns.
#'
#' @param .archival For [`assemble_archival()`], `.archival` is a [`data.table`] of archival observations (such as depth measurements) **for a single individual**. This must contain `timestamp` and `obs` columns plus additional parameter columns.
#'
#' @details
#' [`assemble_timeline()`] is a simple function that defines a regular timeline, of resolution `.step`, from a `list` of input datasets.
#' * If `.trim = FALSE`, this defines a sequence of regular time stamps across the full range of time stamps in the input datasets.
#' * If `.trim = TRUE`, the timeline is trimmed to the overlapping period between datasets.
#'
#'  [`assemble_acoustics()`] and [`assemble_archival()`] prepare timelines of acoustic and archival observations as required for the particle filter ([`pf_filter()`]). The filter expects a `list` of datasets (one for each data type). Each dataset must contain the following columns: `timestamp`, `sensor_id`, `obs` and additional columns with the parameters of the observation model (see [`glossary`]).
#'
#' * [`assemble_acoustics()`] prepares a timeline of acoustic observations, as required by the filter. This function expects a 'standard' acoustic dataset (that is, a [`data.table`] like [`dat_acoustics`]) that defines detections at receivers alongside a moorings dataset (like [`dat_moorings`]) that defines receiver deployment periods and optionally  a [`data.table`] of servicing events (when receiver(s) were non-operational). [`assemble_acoustics()`] uses these datasets to assemble a complete time series of acoustic observations; that is, a [`data.table`] of time stamps and receivers that defines, for each time step and each operational receiver whether (`1L`) or not (`0L`) a detection was recorded at that time step. Duplicate observations (that is, detections at the same receiver in the same time step, are dropped.) If available in `.moorings`, additional columns (`receiver_alpha`, `receiver_beta` and `receiver_gamma`) are included as required for the default acoustic observation model (that is, [`ModelObsAcousticLogisTrunc`]). If observation model parameters vary both by receiver and through time, simply amend these columns as required.
#'
#' * [`assemble_archival()`] prepares a timeline of archival observations (such as depth measurements), as required by the filter. This function expects a [`data.table`] that includes, at a minimum, the `timestamp` and `obs` columns. The latter defines the observations. For archival data, the `sensor_id` column (if unspecified) is simply set to `1L`. The function re-expresses time stamps at the resolution specified by `timeline`. Duplicate observations (that is, multiple measurements in the same time step) throw a [`warning`].
#'
#' Additional datasets are easy to incorporate into the particle filter but require manual preparation in the format described above. Observations can be recorded irregularly or regularly through time but must be expressed at the temporal resolution defined by the timeline.
#'
#' In `Julia`, datasets are translated into a hash-table (`Dict`) of observations (via [`Patter.assemble_yobs()`](https://edwardlavender.github.io/Patter.jl/)). For each time stamp with an observation, this includes a `Vector` of `Tuple`s, each containing the observation and the associated [`ModelObs`] instance that defines the parameters of the observation model. The particle filter ([`Patter.particle_filter()`](https://edwardlavender.github.io/Patter.jl/)) iterates over each time step in the timeline, uses a movement model to simulate animal movement and, for the time stamps with observations, evaluates the likelihood of those observations for the simulated locations (particles).
#'
#' `assemble_*()` routines are only required for real-world analyses.
#'
#' @example man/examples/example-assemble-data.R
#' @seealso Particle filters and smoothers sample states (particles) that represent the possible locations of an individual through time, accounting for all data and the individual's movement.
#' * To simulate artificial datasets, see `sim_*()` functions (especially [`sim_path_walk()`], [`sim_array()`] and [`sim_observations()`]).
#' * To assemble real-world datasets for the filter, see [`assemble`]`_*()` functions.
#' * [`pf_filter()`] runs the filter:
#'    * For state types, see [`State`];
#'    * For observation models, see [`ModelObs`];
#'    * For movement models, see [`ModelMove`];
#' * To run particle smoothing, use [`pf_smoother_two_filter()`].
#' * To map emergent patterns of space use, use a `map_*()` function (such as [`map_pou()`], [`map_dens()`] and [`map_hr()`]).
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

assemble_acoustics <- function(.timeline, .acoustics, .moorings, .services = NULL) {

  # Define study time interval
  step     <- diffstep(.timeline)
  time_int <- interval(min(.timeline), max(.timeline))

  # Define moorings
  .moorings <-
    .moorings |>
    as.data.frame() |>
    # Focus on receivers within the study interval
    rename(sensor_id = "receiver_id") |>
    mutate(receiver_start = lubridate::round_date(.data$receiver_start, step),
           receiver_end = lubridate::round_date(.data$receiver_end, step),
           int = interval(.data$receiver_start, .data$receiver_end)) |>
    filter(int_overlaps(.data$int, time_int)) |>
    select(-"int") |>
    as.data.frame()

  if (fnrow(.moorings) == 0L) {
    abort("There are no receiver deployments in `timeline.")
  }

  # Define services
  if (!is.null(.services)) {
    .services <-
      .services |>
      # (Use data.frame() for reframe(), below)
      as.data.frame() |>
      # Define servicing events
      select(sensor_id = "receiver_id", "service_start", "service_end") |>
      mutate(service_start = lubridate::round_date(.data$service_start, step),
             service_end = lubridate::round_date(.data$service_end, step)) |>
      # Define a sequence of time steps along each servicing event
      group_by(.data$sensor_id) |>
      reframe(timestamp = seq(.data$service_start, .data$service_end, step)) |>
      ungroup() |>
      select("sensor_id", "timestamp") |>
      mutate(service = 1L) |>
      as.data.table()
  }

  # Define acoustics
  .acoustics <-
    .acoustics |>
    lazy_dt(immutable = TRUE) |>
    select("timestamp", sensor_id = "receiver_id") |>
    as.data.table()

  # Define detection time series
  # * TO DO
  # * Include detections/before/after first/last observation for map_init()
  # * (as they can restrict starting locations)
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
    .moorings |>
    # Define a sequence of time steps along receiver deployment periods
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

  # Filter by servicing events
  # (Drop time steps during receiver deployment periods)
  if (!is.null(.services)) {
    dataset <-
      dataset |>
      collapse::join(.services,
                     on = c("sensor_id", "timestamp"),
                     verbose = 0L) |>
      lazy_dt(immutable = FALSE) |>
      filter(is.na(.data$service)) |>
      select(!"service") |>
      as.data.table()
  }

  # Add acoustic model parameters
  dataset <-
    dataset |>
    join(.moorings |>
           select("sensor_id",
                  any_of(c("receiver_x", "receiver_y",
                           "receiver_alpha", "receiver_beta", "receiver_gamma"))),
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
