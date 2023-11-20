#' @title AC* set up: set up movement datasets
#' @description This function processes passive acoustic telemetry detections and (optionally) archival time series for use in AC-branch algorithms.
#' @param .acoustics A [`data.table`] that defines passive acoustic telemetry detections (see [`dat_acoustics`] for an example) for a single individual. At a minimum, this must contain a `timestamp` column (an ordered, `POSIXct` vector that defines the times of detections) and `receiver_id` (an `integer` vector that defines the receiver(s) that recorded detections).
#' @param .archival (optional) A [`data.table`] that defines depth (m) observations (see [`dat_archival`] for an example) for the same individual. At a minimum, this must contain a `timestamp` column (as in `.acoustics`) and a `depth` column (a positive-valued `numeric` vector that defines the individual's depth (m) below the surface at each time step).
#' @param .step An character, passed to [`lubridate::period()`], [`lubridate::round_date()`] and [`seq()`] that defines the duration between sequential time steps (e.g., `"2 mins"`). If `.archival` is supplied, `.step` should be the duration between sequential depth observations.
#' @param .mobility A constant that defines the maximum (Euclidean) distance the individual could move in `.step`.
#' @param .detection_range A constant that defines the detection range (required for [`pf_forward_2()`] implementations). A constant value across all receivers and time steps is assumed. If this is unsuitable, a manual definition of the `buffer_future_incl_gamma` column (see Value) is currently required.
#'
#' @details This function implements the following routines:
#' * Acoustic time series are rounded to the nearest `.step`;
#' * All receivers that recorded detection(s) in each time interval are listed;
#' * Duplicate detections (at the same receiver in the same time interval) are dropped;
#' * Archival time series, if provided, are defined over the same time interval and both sets of time series are aligned;
#' * Acoustic and archival time series are merged and ordered by time stamp;
#' * Information required by the AC* algorithms (ultimately [`acs()`]) is added;
#'
#' @return The function returns a [`data.table`] with the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * `timestamp`---a regular sequences of `POSIXct` time stamps;
#' * `date`---a `character` that defines the date;
#' * `detection_id`---an `integer` vector that uniquely defines each detection;
#' * `detection`---an `integer` that distinguishes the time steps at which detections were (1) or were not (0) recorded;
#' * `receiver_id`---a `list` that defines the receiver(s) that recorded detection(s) at each time step;
#' * `receiver_id_next`---a `list` that defines the receiver(s) that recorded the next detection(s);
#' * `buffer_past`---a `double` that controls container growth from the past to the present;
#' * `buffer_future`---a `double` that controls container shrinkage from the future to the present for [`acs()`];
#' * `buffer_future_incl_gamma`---if `.detection_range` is provided, `buffer_future_incl_gamma` is a `double` (`buffer_future` + `.detection_range`) that controls container shrinkage for [`pf_forward_2()`];
#' * `depth`---if `.archival` is provided, `depth` is a number that defines the individual's depth (m) at each time step;
#'
#' @examples
#' #### Define example datasets
#' acoustics <- dat_acoustics[dat_acoustics$individual_id == 25, ]
#' archival  <- dat_archival[dat_archival$individual_id == 25, ]
#'
#' #### Example (1): Implement the function for acoustic time series only
#' obs <- acs_setup_obs(acoustics, .step = "2 mins", .mobility = 500)
#' utils::head(obs)
#'
#' #### Example (2): Use alternative step lengths & mobilities
#' obs <- acs_setup_obs(acoustics, .step = "4 mins", .mobility = 1000)
#' utils::head(obs)
#'
#' #### Example (3): Implement the function for acoustic & archival time series
#' obs <- acs_setup_obs(acoustics, archival, .step = "2 mins", .mobility = 500)
#' utils::head(obs)
#'
#' @seealso
#'
#' See the [`process_receiver_ids`](https://edwardlavender.github.io/flapper/reference/process_receiver_id.html) function in the in the [`flapper`](https://edwardlavender.github.io/flapper/reference/process_receiver_id.html) package to define receiver deployments using an integer vector.
#'
#' @author Edward Lavender
#' @export

acs_setup_obs <- function(.acoustics, .archival = NULL, .step, .mobility, .detection_range = NULL) {

  #### Check user inputs
  .acoustics <- check_acoustics(.acoustics)
  .archival  <- check_archival(.archival)
  check_inherits(.step, "character")
  if (!is.null(.archival)) {
    # Check archival time series are spaced `.step` apart
    if (nrow(.archival) == 1L) {
      abort("There is only one archival observation.")
    }
    .step_req <- as.integer(lubridate::seconds(lubridate::period(.step)))
    .step_obs <- as.numeric(difftime(.archival$timestamp[2], .archival$timestamp[1], .step, units = "secs"))
    if (!isTRUE(all.equal(.step_req, .step_obs))) {
      abort("Archival time series are not spaced `.step` ('{.step}') units apart (observed step: {.step_obs} s).",
            .envir = environment())
    }
  }

  #### Process acoustics
  # * Round time series & drop duplicates
  # * List receivers with detections
  .acoustics <-
    .acoustics |>
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

  #### Align time series
  if (!is.null(.archival)) {
    # Filter acoustics by archival time series
    .acoustics <-
      .acoustics |>
      filter(.data$timestamp >= min(.archival$timestamp) &
               .data$timestamp <= max(.archival$timestamp))
    # Filter archival time series by acoustic time series
    if (nrow(.acoustics) > 0L) {
      .archival <-
        .archival |>
        filter(.data$timestamp >= min(.acoustics$timestamp) &
                 .data$timestamp <= max(.acoustics$timestamp))
    }
    if (nrow(.acoustics) == 0L | nrow(.archival) == 0L) {
      abort("There are no remaining observations after aligning the acoustic and archival time series.")
    }
  }

  #### Define output time series with acoustic & archival data
  # Define regular time series
  out <- data.table(timestamp = seq(min(.acoustics$timestamp), max(.acoustics$timestamp), by = .step))
  # Add acoustic data
  detection <- NULL
  out[, detection := as.integer((timestamp %in% .acoustics$timestamp) + 0)]
  out <- merge(out, .acoustics, all.x = TRUE, by = "timestamp")
  # Add archival data
  if (!is.null(.archival)) {
    depth <- timestamp <- NULL
    out[, depth := .archival$depth[match(timestamp, .archival$timestamp)]]
  }

  #### Tidy outputs & return
  out <-
    out |>
    lazy_dt(immutable = TRUE) |>
    mutate(date = as.character(as.Date(.data$timestamp)),
           detection_id = as.integer(data.table::nafill(.data$detection_id, type = "locf"))) |>
    group_by(.data$detection_id) |>
    # Define buffers
    mutate(step_forwards = row_number(),
           step_backwards = rev(.data$step_forwards),
           # We buffer the past by mobility
           buffer_past = .mobility,
           # We shrink the future
           buffer_future = .mobility * .data$step_backwards) |>
    ungroup() |>
    arrange(.data$timestamp) |>
    mutate(timestep = as.integer(row_number()),
           receiver_id_next = .acs_setup_obs_receiver_id_next(.data$receiver_id)
    ) |>
    as.data.table()
  # Add buffer_future_incl_gamma column, if applicable
  # * In acs(), kernels are buffered, so we do not require the gamma parameter
  # * In pf_forward_2(), we calculate distances between particles & receivers so the gamma parameter is required
  if (!is.null(.detection_range)) {
    buffer_future <- NULL
    buffer_future_incl_gamma <- NULL
    out[, buffer_future_incl_gamma := buffer_future + .detection_range]
  }
  # Tidy
  out |>
    select("timestep",
           "timestamp", "date",
           "detection_id", "detection", "receiver_id", "receiver_id_next",
           "buffer_past", "buffer_future",
           any_of(c("buffer_future_incl_gamma", "depth"))
           ) |>
    as.data.table()

}

#' @title AC* set up: define detection container overlaps
#' @description This function identifies receivers with overlapping detection containers in space and time for the AC* algorithms.
#'
#' @param .moorings A [`data.table`] that defines receiver deployments and associated information (see [`dat_moorings`] for an example). At a minimum, this must contain the following columns:
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `receiver_easting` and `receiver_northing` or `receiver_lon` and `receiver_lat`---doubles that define receiver coordinates on a planar coordinate reference system or in longitude/latitude;
#' * `receiver_start`---a `Date` vector that defines receiver deployment dates;
#' * `receiver_end`---a `Date` vector that defines receiver retrieval dates;
#' * `receiver_range`---a double that defines the detection range (m) for each receiver;
#' @param .services (optional) A [`data.table`] that defines receiver IDs and servicing `Date`s (times during the deployment period of a receiver when it was not active due to servicing) (see [`make_matrix_receivers()`]). If provided, this must contain the following columns:
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `service_start`---a `Date` vector that defines receiver servicing start dates;
#' * `service_end`---a `Date` vector that defines receiver servicing completion dates;
#'
#' @details In the AC* algorithms, at the moment of detection, the set of possible locations depends on the receiver(s) at which an individual is, and is not, detected. The outputs of this function are used to restrict the probability calculations to the set of receivers that overlap with the receiver(s) at which an individual is detected for improved efficiency.
#'
#' At the time of writing (November 2023), the function permits receiver ranges to differ between receivers, but assumes they are constant in time.
#'
#' @return The function returns a nested `list`, with one element for all integers from `1:max(.moorings$receiver_id)`. Any elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a `NULL` or a `list` that defines, for each deployment date with overlapping receiver(s), a vector of overlapping receiver(s).
#'
#' @examples
#' #### Example (1): Basic implementation
#' overlaps <- acs_setup_detection_overlaps(dat_moorings)
#' summary(overlaps)
#'
#' @source This function supersedes the [`get_detection_containers_overlaps`](https://edwardlavender.github.io/flapper/reference/get_detection_containers_overlap.html) function in the [`flapper`](https://github.com/edwardlavender/flapper) package.
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_overlaps <- function(.moorings, .services = NULL) {

  #### Check user inputs
  lonlat    <- .is_lonlat(.moorings)
  .moorings <- check_moorings(.moorings, .lonlat = lonlat)
  check_inherits(.moorings$receiver_start, "Date")
  check_inherits(.moorings$receiver_end, "Date")
  check_names(.moorings, "receiver_range")
  if (!is.null(.services)) {
    .services <- check_services(.services, .moorings)
    check_inherits(.services$service_start, "Date")
    check_inherits(.services$service_end, "Date")
  }

  #### Define receiver pairs
  receivers <- unique(.moorings$receiver_id)
  pairs <-
    expand.grid(r1 = receivers, r2 = receivers) |>
    filter(.data$r1 != .data$r2) |>
    as.data.frame()

  #### Define receivers overlapping in deployment period & time
  # Note that lubridate::interval() only works with data.frame() (not data.table())
  .moorings     <- as.data.frame(.moorings)
  .moorings$int <- lubridate::interval(.moorings$receiver_start, .moorings$receiver_end)
  ind_1 <- match(pairs$r1, .moorings$receiver_id)
  ind_2 <- match(pairs$r2, .moorings$receiver_id)
  pairs <-
    pairs |>
    mutate(
      # Identify deployment periods
      receiver_start = .moorings$receiver_start[ind_1],
      receiver_end = .moorings$receiver_end[ind_1],
      int_1 = lubridate::interval(.data$receiver_start,
                                  .data$receiver_end),
      int_2 = lubridate::interval(.moorings$receiver_start[ind_2],
                                  .moorings$receiver_end[ind_2]),
      # Calculate distances between receivers
      rng_1 = .moorings$receiver_range[ind_1],
      rng_2 = .moorings$receiver_range[ind_2],
      dist = terra::distance(cbind(.moorings$receiver_x[ind_1],
                                   .moorings$receiver_y[ind_1]),
                             cbind(.moorings$receiver_x[ind_2],
                                   .moorings$receiver_y[ind_2]),
                             lonlat = lonlat, pairwise = TRUE)
    ) |>
    # Identify receivers that overlap (at least partially) in time & space
    filter(lubridate::int_overlaps(.data$int_1, .data$int_2)) |>
    filter(.data$dist <= (.data$rng_1 + .data$rng_2)) |>
    arrange(.data$r1, .data$r2) |>
    as.data.frame()

  #### Build overlaps list
  # Define data lists for quick access
  moorings_ls <- split(.moorings, .moorings$receiver_id)
  pairs_ls    <- split(pairs, pairs$r1)
  if (!is.null(.services)) {
    .services     <- as.data.frame(.services)
    .services$int <- lubridate::interval(.services$service_start, .services$service_end)
    services_ls   <- split(.services, .services$receiver_id)
  }
  # Build list
  out <-
    pbapply::pblapply(seq_len(max(.moorings$receiver_id)), function(i) {

      # Define data for relevant receiver
      # print(i)
      r          <- i
      rc         <- as.character(r)
      r_pairs    <- pairs_ls[[rc]]
      if (is.null(r_pairs)) {
        return(NULL)
      }
      r_moorings <- moorings_ls[[rc]]

      # Define active dates
      active <- seq(min(r_moorings$receiver_start), max(r_moorings$receiver_end), by = "days")
      if (!is.null(.services)) {
        r_services <- services_ls[[rc]]
        if (!is.null(r_services)) {
          active <- active[!(active %within% r_services$int)]
        }
      }

      # For each date, identify overlapping receivers
      # * Dates must be _within_ active deployment intervals of other receivers
      overlaps <-
        expand.grid(r1 = r, date = active, r2 = r_pairs$r2) |>
        mutate(r2_active = .moorings$int[match(.data$r2, .moorings$receiver_id)]) |>
        filter(date %within% .data$r2_active) |>
        select("r1", "date", "r2") |>
        as.data.frame()

      # Account for servicing dates of overlapping receivers
      # * Dates must _not_ be within servicing intervals of other receivers
      if (!is.null(.services)) {
        # Add service intervals
        overlaps <-
          overlaps |>
          mutate(r2_service = .services$int[match(.data$r2, .services$receiver_id)],
                 r2_service_na = .data$r2_service@start) |>
          as.data.frame()
        # Identify positions with service intervals (pos_1) that overlap with service dates (to drop)
        pos_1 <- which(!is.na(overlaps$r2_service_na))
        pos_2 <- which(overlaps$date[pos_1] %within% overlaps$r2_service[pos_1])
        pos <- pos_1[pos_2]
        # Filter out dates that are within servicing intervals
        if (length(pos) > 0L) {
          overlaps <- overlaps[-pos, ]
        }
        overlaps |>
          select("r1", "date", "r2") |>
          as.data.table()
      }

      # Return a list of overlaps
      # * This has one element for each date when there was >= 1 overlapping receiver
      # * Each element contains a vector of the overlapping receiver(s) on that date
      split(overlaps$r2, overlaps$date)

    })

  #### Return outputs
  out

}

#' @title AC* set up: calculate detection probability around a receiver
#' @description This function is an example detection probability function, of the kind required by [`acs_setup_detection_kernels()`].
#' @param .data A one-row [`data.table`] that defines the location of the receiver and associated information used by the model of detection probability.
#' @param .bathy A [`SpatRaster`] that defines the grid over which detection probability is calculated.
#' @param .calc_detection_pr A function that calculates detection probability. In this implementation, the function is used to translate a [`SpatRaster`] of distances (m) (from each grid cell to the receiver in `.data`) via [`terra::app()`].
#' @param ... Additional arguments passed to `.calc_detection_pr` ([`calc_detection_pr_logistic()`]  by default.)
#'
#' @details In the AC* algorithms, a model of the detection process informs the set of possible locations for an individual. The information provided by this model is represented in the form of kernels, which are created via [`acs_setup_detection_kernels()`]. For any one receiver, the form of the kernel depends on the input to `.calc_detection_pr`. This function exemplifies one possible input to this argument, which is a model in which detection probability declines logistically with distance from a receiver.
#'
#' # Warning
#'
#' * This function is used to streamline examples and does not represent a generically suitable detection probability model.
#' * The function does not check user inputs.
#'
#' @return The function returns a [`SpatRaster`] that defines the probability of detection in each cell, according to a pre-defined model.
#'
#' @examples
#' m <- dat_moorings[1, , drop = FALSE]
#' p <- acs_setup_detection_pr(m, dat_gebco())
#' terra::plot(p)
#' points(m$receiver_easting, m$receiver_northing, pch = ".")
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_pr <- function(.data,
                                   .bathy,
                                   .calc_detection_pr = calc_detection_pr_logistic, ...) {
  # Calculate Euclidean distance around receiver
  rxy  <- matrix(c(.data$receiver_easting, .data$receiver_northing), ncol = 2)
  cell <- terra::cellFromXY(.bathy, rxy)
  grid <- terra::setValues(.bathy, NA)
  grid[cell] <- 1
  dist <- terra::distance(grid, unit = "m")
  dist <- terra::mask(dist, .bathy)
  # Convert distances to detection pr
  terra::app(dist, calc_detection_pr_logistic, ...)
}


#' @title AC* set up: define detection kernels
#' @description This function defines the detection kernels for the AC* algorithms.
#' @param .moorings A [`data.table`] that defines receiver deployments and associated information (see [`dat_moorings`] for an example). At a minimum, this must contain the following columns:
#' * `receiver_id`---an `integer` vector that defines unique receiver deployments;
#' * `receiver_start` and `receiver_end`---`Date` vectors that defines receiver operational periods (see [`make_matrix_receivers()`]);
#' * `receiver_easting` and `receiver_northing`---`numeric` vectors that define receiver locations on `.bathy` (used to validate `.calc_detection_pr()`)
#' * Plus any columns used internally by `.calc_detection_pr` (see below).
#' @param .services (optional) A [`data.table`] that defines receiver IDs and servicing `Date`s (times during the deployment period of a receiver when it was not active due to servicing) (see [`make_matrix_receivers()`]). If provided, this must contain the following columns:
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `service_start`---a `Date` vector that defines receiver servicing start dates;
#' * `service_end`---a `Date` vector that defines receiver servicing completion dates;
#' @param .calc_detection_pr,... A function that defines a receiver-specific detection kernel (see [`acs_setup_detection_pr()`] for an example). This must accept three arguments (even if they are ignored):
#' * `.data`---A one-row [`data.table`] that contains the information in `.moorings` for a specific receiver;
#' * `.bathy`---A [`SpatRaster`] that defines the grid over which detection probability is calculated (see below);
#' * `...` Additional arguments passed via [`acs_setup_detection_kernels()`].
#' Using these inputs, the function must return a [`SpatRaster`] that defines the detection kernel around a specific receiver (see Examples).
#' @param .bathy A [`SpatRaster`] that defines the grid over which detection kernels are defined;
#' @param .verbose A `logical` variable that defines whether or not to print messages to the console to relay function progress.
#'
#' @details This function permits receiver-specific detection kernels.
#'
#' @return The function returns a named `list`, with the following elements:
#' * **`receiver_specific_kernels`**. A `list`, with one element for all integers from 1 to the maximum receiver number. Any elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a [`SpatRaster`] of the detection probability kernel around the relevant receiver. Cells values define the detection probability around a receiver, given `.calc_detection_pr` In the AC* algorithm(s), these kernels are used to up-weight location probabilities near to a receiver when it is detected (following modification to account for overlapping areas, if necessary).
#' * **`receiver_specific_inv_kernels`**. A `list`, as for `receiver_specific_kernels`, but in which elements contain the inverse detection probability kernels (i.e., 1 - detection probability). In the AC* algorithm(s), these is used to down-weight-weight location probabilities in the overlapping regions between a receiver that recorded detections and others that did not at the same time.
#' * **`array_design`**. A `data.frame` that defines the number and deployment times of each unique array design, resulting from receiver deployment, servicing and removal. In the times between detections, this is used to select the appropriate 'background' detection probability surface (see below). This contains the following columns:
#'   * `array_id`. An integer vector that defines each unique array design.
#'   * `array_start_date`. A Date that defines the start date of each array design.
#'   * `array_end_date`. A Date that defines the end date of each array design.
#'   * `array_interval`. A [`lubridate::Interval-class`] vector that defines the deployment period of each array design.
#' * **`array_design_by_date`**. A named `list` that defines, for each date (list element), the array design on that date (based on `array_id` in `array_design`).
#' * **`bkg_surface_by_design`**. A `list`, with one element for each array design, that defines the detection probability surface across all receivers deployed in that phase of the study. In areas that are covered by the detection probability kernel of a single receiver, the detection probability depends only on distance to that receiver (via `.calc_detection_pr`). In areas covered by multiple, overlapping kernels, detection probability represents the combined detection probability across all overlapping kernels (see Details).
#' * **`bkg_inv_surface_by_design`**. A `list`, as above for `bkg_surface_by_design`, but which contains the inverse detection probability surface (i.e., 1 - `bkg_surface_by_design`). In the AC* algorithm(s), this is used to up-weight areas away from receivers (or, equivalently, down-weight areas near to receivers) in the time steps between detections.
#'
#' @examples
#' #### Define example 'moorings' & 'services' dataset
#' # receivers 3 and 4 overlap in space but receiver 5 is further afield
#' require(graphics)
#' require(data.table)
#' m <- data.table(receiver_id = c(3, 4, 5),
#'                 receiver_start = as.Date(c("2016-01-01", "2016-01-01", "2016-01-01")),
#'                 receiver_end = as.Date(c("2016-01-05", "2016-01-05", "2016-01-05")),
#'                 receiver_easting = c(706124.9, 706012.7, 709379.0),
#'                 receiver_northing = c(6265030, 6264993, 6260093),
#'                 receiver_range = 750)
#' s <- data.table(receiver_id = c(3, 5),
#'                 service_start = as.Date(c("2016-01-01", "2016-01-01")),
#'                 service_end = as.Date(c("2016-01-01", "2016-01-01")))
#'
#' #### Define function to calculate detection probability
#' # This must accept a .data & .bathy argument:
#' # * .data is the .moorings data for a specific receiver (e.g., containing receiver coordinates)
#' # * .bathy is as described above
#' # Using these arguments, the function must calculate detection probability around the receiver
#' # * This implementation supports receiver-specific detection ranges
#' acs_setup_detection_pr <- function(.data, .bathy, ...) {
#'
#'   # Define helper function to calculate detection probability give distance
#'   calc_dpr <- function(distance) {
#'     pr <- stats::plogis(2.5 + -0.02 * distance)
#'     pr[distance > .data$receiver_range] <- 0
#'     pr
#'   }
#'   # Calculate Euclidean distance around receiver
#'   rxy <- matrix(c(.data$receiver_easting, .data$receiver_northing), ncol = 2)
#'   cell <- terra::cellFromXY(.bathy, rxy)
#'   grid <- terra::setValues(.bathy, NA)
#'   grid[cell] <- 1
#'   dist <- terra::distance(grid, unit = "m")
#'   dist <- terra::mask(dist, .bathy)
#'   # Convert distances to detection pr
#'   terra::app(dist, calc_dpr)
#' }
#' # Examine output of function for example receiver
#' pr <- lapply(seq_len(max(m$receiver_id)), function(id) {
#'   if (!(id %in% m$receiver_id)) return(NULL)
#'   acs_setup_detection_pr(m[m$receiver_id == id, , drop = FALSE], dat_gebco())
#' })
#'
#' #### Example (1): Implement function using specified inputs
#' k <- acs_setup_detection_kernels(m, s,
#'                                  .calc_detection_pr = acs_setup_detection_pr,
#'                                  .bathy = dat_gebco())
#'
#' # Examine list elements
#' summary(k)
#'
#' # Examine example receiver-specific kernels
#' pp <- par(mfrow = c(1, 2))
#' lapply(c(3, 4), \(id) {
#'   terra::plot(k$receiver_specific_kernels[[id]])
#'   points(m[m$receiver_id == id, .(receiver_easting, receiver_northing)], cex = 2)
#' }) |> invisible()
#' par(pp)
#'
#' # Examine example receiver-specific inverse kernels
#' pp <- par(mfrow = c(1, 2))
#' lapply(c(3, 4), \(id) {
#'   terra::plot(k$receiver_specific_kernels[[id]])
#'   points(m[m$receiver_id == id, .(receiver_easting, receiver_northing)], cex = 2)
#' }) |> invisible()
#' par(pp)
#'
#' # Examine background detection Pr surfaces
#' # (for each unique combination of receivers that were deployed)
#' pp <- par(mfrow = c(1, 2))
#' lapply(k$bkg_surface_by_design, \(bkg) {
#'   terra::plot(bkg, axes = FALSE)
#'   box()
#' }) |> invisible()
#' par(pp)
#'
#' # Examine background inverse detection Pr surfaces
#' pp <- par(mfrow = c(1, 2))
#' lapply(k$bkg_inv_surface_by_design, \(bkg) {
#'   terra::plot(bkg, axes = FALSE)
#'   box()
#' }) |> invisible()
#' par(pp)
#'
#' @source This function is based on the [`acs_setup_detection_kernels`](https://edwardlavender.github.io/flapper/reference/acs_setup_detection_kernels.html) function in the [`flapper`](https://github.com/edwardlavender/flapper) package, where the role of detection kernels in the AC* algorithms is described extensively (see Details).
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_kernels <-
  function(.moorings,
           .services = NULL,
           .calc_detection_pr,
           .bathy,
           .verbose = TRUE, ...) {


    #########################
    #### Initiation, set up and checks

    #### Initiation
    cat_to_console <- function(..., show = .verbose) {
      if (show) cat(paste(..., "\n"))
    }
    t_onset <- Sys.time()
    cat_to_console(paste0("patter::acs_setup_detection_kernels() called (@ ", t_onset, ")..."))
    cat_to_console("... Setting up function...")

    #### Check user inputs
    .moorings <- check_moorings(.moorings)
    if (!is.null(.services)) {
      .services <- check_services(.services, .moorings)
    }


    #########################
    ####  Receiver-specific kernels (for detection)

    #### Calculate detection Pr around each receiver
    # (used to up-weight areas around a receiver with a detection)
    cat_to_console("... Getting receiver-specific kernels (for detection)...")
    receiver_specific_kernels <-
      pbapply::pblapply(split(.moorings, .moorings$receiver_id), function(m) {
        # Define kernel using user-provided function
        k <- .calc_detection_pr(m, .bathy, ...)
        # Calculate Pr at receiver and check it is not NA or 0
        pr_at_receiver <- terra::extract(k, data.frame(m$receiver_easting, m$receiver_northing))[1, 2]
        if (is.na(pr_at_receiver)) {
          warn("Detection probability is NA at receiver {m$receiver_id}.",
               .envir = environment())
        } else if (pr_at_receiver == 0) {
          warn("Detection probability is 0 at receiver {m$receiver_id}.",
               .envir = environment())
        }
        # Return kernel
        k
      })
    names(receiver_specific_kernels) <- as.character(.moorings$receiver_id)

    #### Calculate inverse detection Pr around each receiver
    # (used in calculations to down-weight areas, around a receiver that recorded detections,
    # ... that overlap with receivers that didn't record a detection)
    cat_to_console("... Getting receiver-specific inverse kernels...")
    receiver_specific_inv_kernels <- lapply(receiver_specific_kernels, function(k) 1 - k)
    names(receiver_specific_inv_kernels) <- names(receiver_specific_kernels)


    #########################
    #### Area-wide kernel (for non detection)

    #### Get dates of changes in array design
    cat_to_console("... Getting area-wide kernels (for non-detection)...")
    cat_to_console("... ... Get unique array designs...")
    # Get receiver status matrix from moorings and services (in units of days, time unit is Date)
    rs_mat <- make_matrix_receivers(
      .moorings = .moorings,
      .services = .services,
      .delta_t = "days",
      .as_POSIXct = NULL
    )
    # Get receiver status change points (dates when the array design changed)
    rs_mat_cp <- unique(rs_mat)
    # Define the time interval for each array design
    array_design <- data.frame(
      array_id = 1:nrow(rs_mat_cp),
      array_start_date = as.Date(rownames(rs_mat_cp))
    )
    array_design$array_end_date <-
      lead(array_design$array_start_date) - 1
    array_design$array_end_date[nrow(array_design)] <-
      max(.moorings$receiver_end)
    array_design$array_interval <-
      lubridate::interval(
        array_design$array_start_date,
        array_design$array_end_date
      )
    # Define array designs by date
    array_design_by_date <-
      lapply(split(array_design, array_design$array_id), function(d) {
        data.frame(array_id = d$array_id[1],
                   date = seq(d$array_start_date, d$array_end_date, "days"))
      }) |> rbindlist()
    cdates <- as.character(array_design_by_date$date)
    array_design_by_date        <- lapply(array_design_by_date$array_id, \(x) x)
    names(array_design_by_date) <- as.character(cdates)

    #### For each unique array design, create the area-wide kernel surface that represents detection Pr/inverse detection Pr
    cat_to_console("... ... Get area wide kernels for each array design...")
    bkgs_by_design <-
      pbapply::pblapply(1:nrow(rs_mat_cp), function(icp) {

        #### Identify active receivers on that date
        # icp <- 1
        cat_to_console(paste0("\n... ... ... For design ", icp, "/", nrow(rs_mat_cp), "..."))
        cp <- rs_mat_cp[icp, , drop = FALSE]
        rs_active <- colnames(cp)[which(cp == 1)]

        #### Pull out necessary kernels for active receivers from detection_kernels_by_xy into a list
        cat_to_console("... ... ... ... Extract detection probability kernels for active receivers...")
        detection_kernels_inv_by_rs_active <-
          lapply(rs_active, function(ra) receiver_specific_inv_kernels[[ra]])

        #### Calculate the probability of not being detected in each cell
        cat_to_console("... ... ... ... Combining detection kernels to calculate the background detection probability surfaces (this is a slow step)...")
        if (length(rs_active) == 1) {
          bkg_inv <- detection_kernels_inv_by_rs_active[[1]]
        } else {
          detection_kernels_inv_for_rs_active <- do.call(c, detection_kernels_inv_by_rs_active)
          bkg_inv <- terra::app(detection_kernels_inv_for_rs_active, prod)
        }
        # Get the probability of at least one detection in each grid cell:
        bkg <- 1 - bkg_inv

        #### Return surfaces
        list(bkg = bkg, bkg_inv = bkg_inv)
      })
    bkg_by_design     <- lapply(bkgs_by_design, function(elm) elm$bkg)
    bkg_inv_by_design <- lapply(bkgs_by_design, function(elm) elm$bkg_inv)

    #### Process outputs
    cat_to_console("... Process detection probability kernels ...")
    # For receiver-specific detection probability kernel lists,
    # add NULL elements to the list for any receivers
    # in the range 1:max(rs) that are not in rs.
    # This means we can use receiver numbers to go straight
    # ... to the correct element in the list from the integer receiver ID.
    receiver_specific_kernels <- lapply(seq_len(max(.moorings$receiver_id)), function(i) {
      receiver_specific_kernels[[as.character(i)]]
    })
    receiver_specific_inv_kernels <- lapply(seq_len(max(.moorings$receiver_id)), function(i) {
      receiver_specific_inv_kernels[[as.character(i)]]
    })

    #### Return outputs
    # Define outputs
    out <- list()
    out$receiver_specific_kernels     <- receiver_specific_kernels
    out$receiver_specific_inv_kernels <- receiver_specific_inv_kernels
    out$array_design                  <- array_design
    out$array_design_by_date          <- array_design_by_date
    out$bkg_surface_by_design         <- bkg_by_design
    out$bkg_inv_surface_by_design     <- bkg_inv_by_design
    # Check function duration
    t_end <- Sys.time()
    total_duration <- difftime(t_end, t_onset, units = "mins")
    cat_to_console(paste0("... patter::acs_setup_detection_kernels() call completed (@ ", t_end, ") after ~", round(total_duration, digits = 2), " minutes."))
    # Return outputs
    out
  }
