#' @title Assemble observations
#' @description These functions assemble a timeline and observations for the particle filter ([`pf_filter()`]).
#' @param .datasets,.step,.trim Arguments for [`assemble_timeline()`].
#' * `.datasets`---A `list` of [`data.table`]s, one for each data type, each containing a `timestamp` column;
#' * `.step`---A `character` (such as `"2 mins"`), passed to [`lubridate::round_date()`] and [`seq.POSIXt()`], that defines the resolution of the timeline;
#' * `.trim`---A `logical` variable that defines whether or not to trim the timeline to the overlapping period between datasets;
#' @param .timeline A `POSIXct` vector of regularly spaced time stamps that defines the timeline for the simulation (optionally from [`assemble_timeline()`]). Here, `timeline` is used to:
#' * Define the resolution of observations;
#'
#' @param .detections,.moorings,.services The [`data.table`]s for [`assemble_acoustics()`] (see [`pat_setup_data()`]).
#' * `.detections` is a [data.table] of acoustic detections **for a single individual**. This must contain the `receiver_id` and `timestamp` columns.
#' * `.moorings` is a [`data.table`] of acoustic receiver deployments. This must contain the `receiver_id`, `receiver_start`, and `receiver_end` columns, plus  (optional) additional parameter columns.
#' * (optional) `.services` is a [`data.table`] of servicing events. This must contain the `receiver_id`, `service_start` and `service_end` columns.
#'
#' @param .acoustics,.mobility,.map,.threshold Arguments for [`assemble_acoustics_containers()`].
#'  * `.acoustics` is a [`data.table`] of acoustic observations, from [`assemble_acoustics()`].
#'  * `.mobility` is the maximum movement distance (m) between two time steps (and sets the rate of container contraction).
#'  * `.map`, `.threshold` are distance threshold options (see Details). Specify `.map` or `.threshold`.
#'      * `.map` is a two-column `matrix` of the four coordinates of the study area or a [`SpatRaster`] or [`SpatVector`] from which such a `matrix` can be obtained. If provided, `.threshold` is set automatically based on the distances between receivers and the boundaries of the study area.
#'      * Otherwise, `.threshold` is a `double` that defines the distance threshold.
#'
#' @param .archival  For [`assemble_archival()`], `.archival` is a [`data.table`] of depth observations **for a single individual** with `timestamp` and `obs` columns (see `.dataset`, below).
#'
#' @param .dataset For [`assemble_custom()`], `.dataset` is a [`data.table`] of observations (such as depth measurements) **for a single individual**. This must contain `timestamp` and `obs` columns plus (optional) additional parameter columns.
#'
#' @details
#' [`assemble_timeline()`] is a simple function that defines a regular timeline, of resolution `.step`, from a `list` of input datasets.
#' * If `.trim = FALSE`, this defines a sequence of regular time stamps across the full range of time stamps in the input datasets.
#' * If `.trim = TRUE`, the timeline is trimmed to the overlapping period between datasets.
#'
#'  `assemble_{dataset}()` functions are helper routines that prepare timelines observations for different data types as required for the particle filter ([`pf_filter()`]). The filter expects a named `list` of datasets (one for each data type). Each dataset must contain the following columns: `timestamp`, `sensor_id`, `obs` and additional columns with the parameters of the observation model (see [`glossary`]).
#'
#' [`assemble_acoustics()`] prepares a timeline of acoustic observations as required by the filter **for a single individual**. This function expects a 'standard' detection dataset (that is, a [`data.table`] like [`dat_detections`] but for a single individual) that defines detections at receivers alongside a moorings dataset (like [`dat_moorings`]) that defines receiver deployment periods and, optionally, a [`data.table`] of servicing events (when receiver(s) were non-operational). [`assemble_acoustics()`] uses these datasets to assemble a complete time series of acoustic observations; that is, a [`data.table`] of time stamps and receivers that defines, for each time step and each operational receiver whether (`1L`) or not (`0L`) a detection was recorded at that time step. Duplicate observations (that is, detections at the same receiver in the same time step) are dropped. If available in `.moorings`, additional columns (`receiver_alpha`, `receiver_beta` and `receiver_gamma`) are included as required for the default acoustic observation model (that is, [`ModelObsAcousticLogisTrunc`]). If observation model parameters vary both by receiver and through time, simply amend these columns as required.
#'
#'[`assemble_acoustics_containers()`] prepares a dataset of acoustic containers, given the acoustic time series from [`assemble_acoustics()`]. Acoustic containers define the region within which an individual must be located at a given time step according to the receiver(s) at which it was next detected. Each container is a circular region, of radius \eqn{r}, around a receiver that recorded a detection at the next time step. The radius depends on the time until the next detection, the maximum movement speed and the detection range around the receiver at the time of the detection. For example, if an individual can move up to `.mobility` = 500 m per time step, and two time steps elapse between the first and second detections, then at the moment of the first detection the individual must be within 1000 m of the detection range (say, `receiver_gamma` = 750 m) of the second receiver; that is, at the moment of first detection, the maximum possible distance of the individual from the receiver that recorded the next detection is 1750 m. As time passes, the container shrinks towards the receiver(s) that recorded the next detection(s), in line with the individual's `.mobility`. Acoustic containers, coupled with regular re-sampling, facilitate convergence in the particle filter: at each time step, only particles within an acoustic container are duplicated (other particles are killed), which encourages particles to move towards the next receiver(s) by the time of the next detection(s). In practice, this works as follows. [`assemble_acoustics_containers()`] assembles a [`data.table`] that defines the maximum distance (radius) of the individual from the receiver(s) that recorded the next detection. For computational efficiency, this [`data.table`] only includes containers with a `radius` < `.threshold`. If `.map` is supplied, the `.threshold` is set to the maximum distance between each receiver and the furthest corner of the study area. Otherwise, set the .`threshold` to the desired value. The [`data.table`] from [`assemble_acoustics_containers()`] is used to instantiate a Vector of [`ModelObsAcousticContainer`] sub-types in `Julia`. In the particle filter, for each particle, we compute the log-probability of the particle from the distance of the particle from the relevant receiver (0.0 or -Inf). By re-sampling particles with replacement, particles that move in a way that is incompatible with the location of the next detection(s) are killed. The bottom line is that if have acoustic observations, you should also include acoustic containers in the list of 'observations' for the particle filter ([`pf_filter()`]). This function requires the [`tidyr::nest()`], [`tidyr::unnest()`] and [`zoo::na.locf()`] functions (suggested dependencies).
#'
#' [`assemble_archival()`] prepares a timeline of archival functions **for a single individual**. This simply wraps [`assemble_custom()`] and is informally deprecated.
#'
#' [`assemble_custom()`] prepares a timeline of observations for other data types, as required by the filter. This function expects a [`data.table`] that includes, at a minimum, the `timestamp` and `obs` columns. The latter defines the observations. The `sensor_id` column (if unspecified) is simply set to `1L`. The function re-expresses time stamps at the resolution specified by `timeline`. Duplicate observations (that is, multiple measurements in the same time step) throw a [`warning`].
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
#'
#' @return
#' * [`assemble_timeline()`] returns a POSIXct vector;
#' * [`assemble_acoustics()`], [`assemble_archival()`] and [`assemble_custom()`] return a [`data.table`] for [`pf_filter()`];
#' * [`assemble_acoustics_containers()`] returns a named `list`, with one element ([`data.table`]) for (a) the forward and (b) the backward runs of [`pf_filter()`];
#'
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
      data.table(start = min(dataset$timestamp), end = max(dataset$timestamp))
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

assemble_acoustics <- function(.timeline, .detections, .moorings, .services = NULL) {

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

  # Define detection time series
  # * TO DO
  # * Include detections/before/after first/last observation for map_init()
  # * (as they can restrict starting locations)
  detections <-
    # Clean up input
    .detections |>
    lazy_dt(immutable = TRUE) |>
    select("timestamp", sensor_id = "receiver_id") |>
    # Process input
    filter(.data$timestamp %within% time_int) |>
    mutate(timestamp = lubridate::round_date(.data$timestamp, step)) |>
    group_by(.data$sensor_id, .data$timestamp) |>
    slice(1L) |>
    ungroup() |>
    mutate(obs = 1L) |>
    as.data.table()

  # Define full acoustic time series
  acoustics <-
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
    acoustics <-
      acoustics |>
      collapse::join(.services,
                     on = c("sensor_id", "timestamp"),
                     verbose = 0L) |>
      lazy_dt(immutable = FALSE) |>
      filter(is.na(.data$service)) |>
      select(!"service") |>
      as.data.table()
  }

  # Add acoustic model parameters
  acoustics <-
    acoustics |>
    join(.moorings |>
           select("sensor_id",
                  any_of(c("receiver_x", "receiver_y",
                           "receiver_alpha", "receiver_beta", "receiver_gamma"))),
         on = "sensor_id",
         verbose = 0L) |>
    select("timestamp", everything()) |>
    as.data.table()

  # Return dataset
  acoustics

}

#' @rdname assemble
#' @export

assemble_acoustics_containers <- function(.timeline,
                                          .acoustics,
                                          .mobility,
                                          .map = NULL,
                                          .threshold = NULL) {

  # Check user inputs
  check_timeline(.timeline)
  check_inherits(.acoustics, "data.table")
  directions <- c("forward", "backward")

  # Define boundary box, if supplied
  bb <- .map
  if (!is.null(.map)) {
    if (inherits(.map, c( "SpatRaster", "SpatVector"))) {
      bb <- map_bbox(.map)
    } else if (!(inherits(.map, "matrix") && nrow(.map) == 4L && ncol(.map) == 2L)) {
      abort("`.map` should be a SpatRaster, SpatVector or 4-row matrix of boundary coordinates.")
    }
    if (!is.null(.threshold)) {
      .threshold <- NULL
      warn("`.map` is used in place of `.threshold`.")
    }
  }

  # Assemble containers
  containers <- lapply(directions, function(.direction) {
    .assemble_acoustics_containers(.timeline  = .timeline,
                                   .acoustics = .acoustics,
                                   .mobility  = .mobility,
                                   .bbox      = bb,
                                   .threshold = .threshold,
                                   .direction = .direction)
  })

  names(containers) <- directions
  containers
}

#' @rdname assemble
#' @export

assemble_archival <- function(.timeline, .archival) {
  # .Deprecated(new = "assemble_custom()", old = "assemble_archival()")
  assemble_custom(.timeline = .timeline, .dataset = .archival)
}

#' @rdname assemble
#' @export

assemble_custom <- function(.timeline, .dataset) {

  # Define study time interval
  step     <- diffstep(.timeline)
  time_int <- interval(min(.timeline), max(.timeline))

  # Define sensor ID
  check_names(.dataset, c("timestamp", "obs"))
  dataset <- copy(.dataset)
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
