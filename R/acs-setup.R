#' @title AC* set up: set up movement datasets
#' @description This function proccess passive acoustic telemetry detections and (optionally) archival time series for use in AC-branch algorithms.
#' @param .acoustics A [`data.table`] that defines passive acoustic telemetry detections (see [`dat_acoustics`] for an example) for a single individual. At a minimum, this must contain a `timestamp` column (an ordered, `POSIXct` vector that defines the times of detections) and `receiver_id` (an `integer` vector that defines the receiver(s) that recorded detections).
#' @param .archival (optional) A [`data.table`] that defines depth (m) observations (see [`dat_archival`] for an example) for the same individual. At a minimum, this must contain a `timestamp` column (as in `.acoustics`) and a `depth` column (a positive-valued `numeric` vector that defines the individual's depth (m) below the surface at each time step).
#' @param .step An character, passed to [`lubridate::period()`], [`lubridate::round_date()`] and [`seq()`] that defines the duration between sequential time steps (e.g., `"2 mins"`). If `.archival` is supplied, `.step` should be the duration between sequential depth observations.
#' @param .mobility A number that defines the maximum (Euclidean) distance the individual could move in `.step`.
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
#' * `receiver_id`---a `list` that defines the receiver(s) that recorded detections at each time step;
#' * `buffer_past`---a `double` that controls container growth from the past to the present;
#' * `buffer_future`---a `double` that controls container shrinkage from the future to the present;
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

acs_setup_obs <- function(.acoustics, .archival = NULL, .step, .mobility) {

  #### Check user inputs
  check_acoustics(.acoustics)
  check_archival(.archival)
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
    mutate(detection_id = as.integer(dplyr::row_number())) |>
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
  out |>
    lazy_dt(immutable = TRUE) |>
    mutate(date = as.character(as.Date(.data$timestamp)),
           detection_id = as.integer(data.table::nafill(.data$detection_id, type = "locf"))) |>
    group_by(.data$detection_id) |>
    mutate(step_forwards = dplyr::row_number(),
           step_backwards = rev(.data$step_forwards),
           # We buffer the past by mobility
           buffer_past = .mobility,
           # We shrink the future
           buffer_future = .mobility * .data$step_backwards) |>
    ungroup() |>
    arrange(.data$timestamp) |>
    mutate(timestep = as.integer(dplyr::row_number())) |>
    select("timestep",
           "timestamp", "date",
           "detection_id", "detection", "receiver_id",
           "buffer_past", "buffer_future",
           dplyr::any_of("depth")) |>
    as.data.table()

}


#' @title AC* set up: define detection containers
#' @description This function defines receiver detection containers.
#' @param .bathy A [`SpatRaster`] that defines the grid over which the AC algorithms are implemented. `NA`s in this layer are used to mask detection containers.
#' @param .moorings A [`data.table`] that defines receiver locations and associated information (see [`dat_moorings`] for an example). At a minimum, this must contain `receiver_id`, `receiver_easting`, `receiver_northing` and `receiver_range` columns that define unique receiver deployments, receiver locations and (receiver-specific) detection ranges. Receiver IDs should be an `integer` vector.
#'
#' @details Receiver detection containers are the regions within which an individual must be located, given a detection at a receiver. This function defines detection containers simply as a circular buffer (of distance `.moorings$receiver_range`) around receivers, masked by `.bathy` (e.g., land). Receiver detection containers are used to determine receiver overlaps (via [`acs_setup_detection_overlaps()`]), which are used in the AC* algorithms in detection probability calculations.
#'
#' @return The function returns a named `list`, with one element for each integer from `1:max(moorings$receiver_id)`. Any list elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a [`SpatRaster`] that defines the detection container around that receiver.
#'
#' @examples
#' #### Example (1): Use constant receiver detection ranges
#' # Define grid
#' grid <- dat_gebco()
#' terra::plot(grid)
#' # Define receiver detection ranges
#' dat_moorings$receiver_range <- 500
#' # Define detection containers
#' containers <- acs_setup_detection_containers(grid, dat_moorings)
#' # Visualise an example container
#' terra::plot(containers[[dat_moorings$receiver_id[1]]])
#' points(dat_moorings$receiver_easting[1], dat_moorings$receiver_northing[1])
#'
#' #### Example (2): Use receiver-specific detection ranges
#' dat_moorings$receiver_range[1] <- 100
#' dat_moorings$receiver_range[2] <- 1000
#' containers <- acs_setup_detection_containers(grid, dat_moorings)
#' terra::plot(containers[[dat_moorings$receiver_id[1]]], col = "red")
#' terra::lines(terra::as.polygons(containers[[dat_moorings$receiver_id[2]]]), col = "blue")
#'
#' @source This function is based on the [`acs_setup_containers`](https://edwardlavender.github.io/flapper/reference/acs_setup_containers.html) function in the [flappe`r](https://github.com/edwardlavender/flapper) package.
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_containers <- function(.bathy, .moorings) {

  #### Check user inputs
  # * moorings should required columns (receiver_id, receiver_easting, receiver_northing, receiver_range)
  # * moorings$receiver_id should be an integer from 1 to n
  check_moorings(.moorings, .class = "data.frame")
  check_names(input = .moorings, req = "receiver_range")

  #### Build containers
  rs <- seq_len(max(.moorings$receiver_id))
  containers <-
    pbapply::pblapply(rs, function(id) {
      out <- NULL
      bool <- .moorings$receiver_id == id
      if (any(bool)) {
        d <- .moorings[which(bool), ]
        g   <- terra::setValues(.bathy, NA)
        rxy <- matrix(c(d$receiver_easting, d$receiver_northing), ncol = 2)
        g[terra::cellFromXY(g, rxy)] <- 1
        # terra::plot(g)
        out <- terra::buffer(g, d$receiver_range)
        out <- (terra::mask(out, .bathy)) + 0
        # terra::plot(out)
      }
      out
    })
  names(containers) <- rs
  containers
}


#' @title AC* set up: define detection container overlaps
#' @description This function identifies receivers with overlapping detection containers in space and time for the AC* algorithms.
#'
#' @param .containers A named `list` of `SpatRaster`s that represent receiver detection containers, from [`acs_setup_detection_containers()`].
#' @param .moorings A [`data.table`] that defines receiver deployments and associated information (see [`dat_moorings`] for an example). At a minimum, this must contain the following columns:
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `receiver_start`---a `Date` vector that defines receiver deployment dates;
#' * `receiver_end`---a `Date` vector that defines receiver retrieval dates;
#' @param .services (optional) A [`data.table`] that defines receiver IDs and servicing `Date`s (times during the deployment period of a receiver when it was not active due to servicing) (see [`make_matrix_receivers()`]). If provided, this must contain the following columns:
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `service_start`---a `Date` vector that defines receiver servicing start dates;
#' * `service_end`---a `Date` vector that defines receiver servicing completion dates;
#'
#' @details In the AC* algorithms, at the moment of detection, the set of possible locations depends on the receiver(s) at which an individual is, and is not, detected. The outputs of this function are used to restrict the probability calculations to the set of receivers that overlap with the receiver(s) at which an individual is detected for improved efficiency.
#'
#' @return The function returns a named `list` with two elements:
#' * **`overlap_by_receiver`** is `list`, with one element for all integers from `1:max(.moorings$receiver_id)`. Any elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a `data.frame` that defines, for each day over the deployment period (defined in `timestamp`) of that receiver (defined in `receiver_id`), whether (1) or not (0) that receiver overlapped in space with every other receiver (defined in the remaining columns by their receiver IDs).
#' * **`overlap_by_date`** is a named `list`, with one element for each `Date` from the start until the end of the study `(min(.moorings$receiver_start_date):max(.moorings$receiver_end_date))`, that records an integer vector of all receivers with overlapping containers on that date. In this vector, each receiver overlaps with at least one other receiver (but not every receiver will necessarily overlap with every other receiver).
#'
#' @examples
#' #### Example (1): Basic implementation
#' # Define detection containers
#' dat_moorings$receiver_range <- 500
#' containers <- acs_setup_detection_containers(dat_gebco(), dat_moorings)
#' # Identify receiver overlaps
#' overlaps <- acs_setup_detection_overlaps(containers, dat_moorings)
#' summary(overlaps)
#'
#' @source This function is based on the [`get_detection_containers_overlaps`](https://edwardlavender.github.io/flapper/reference/get_detection_containers_overlap.html) function in the [`flapper`](https://github.com/edwardlavender/flapper) package.
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_overlaps <- function(.containers, .moorings, .services = NULL) {

  #### Check user inputs
  rlang::check_installed("tidyr")
  .moorings <- check_moorings(.moorings)
  check_inherits(.moorings$receiver_start, "Date")
  check_inherits(.moorings$receiver_end, "Date")
  if (!is.null(.services)) {
    .services <- check_services(.services, .moorings)
    check_inherits(.services$service_start, "Date")
    check_inherits(.services$service_end, "Date")
  }

  #### Define receiver activity status matrix
  # This defines whether or not each receiver was active on each date (0, 1)
  # We'll start from this point because it accounts for receiver activity status (including servicing).
  # We'll then update this, for each receiver, to define whether or not, if that receiver was active on a given date
  # ... which other receivers (if any) it overlapped in space (and time) with.
  rs_active_mat <- make_matrix_receivers(
    .moorings = .moorings,
    .services = .services,
    .delta_t = "days",
    .as_POSIXct = NULL
  )

  #### Define a list, with one dataframe element per receiver, that defines, for each time step, the overlapping receivers (0, 1)
  list_by_receiver <- pbapply::pblapply(seq_len(length(.containers)), function(i) {

    #### Collect container and receiver status information
    r1      <- names(.containers)[[i]]
    cont_r1 <- .containers[[i]]
    if (is.null(cont_r1)) return(NULL)
    # Define moorings & receiver status matrix
    m    <- .moorings[.moorings$receiver_id == r1, , drop = FALSE]
    info <- rs_active_mat
    info <- info[as.Date(rownames(info)) %within% lubridate::interval(m$receiver_start, m$receiver_end), , drop = FALSE]

    #### Convert receiver 'active' index (0, 1) to 'overlapping' index
    # ... A) Check for overlapping receivers
    # ... B) If there are overlapping receivers,
    # ... ... then we force all dates when the receiver of interest was not active to take 0 (no receivers could overlap with it then)
    # ... ... and we force all receivers that didn't overlap in space to 0
    # ... C) If there are no overlapping receivers, the whole matrix just gets forced to 0

    ## (A) Get an index of the receivers that intersected with the current receiver (in space)
    cont_r1 <- terra::mask(cont_r1, cont_r1 != 1, maskvalue = TRUE, updatevalue = NA)
    int <- lapply(seq_len(length(.containers)), function(j) {
      r2      <- names(.containers)[j]
      cont_r2 <- .containers[[j]]
      .int     <- data.frame(receiver_id = r2, overlap = 0)
      if (!is.null(cont_r2) && r1 != r2) {
        cont_r2 <- terra::mask(cont_r2, cont_r2 != 1, maskvalue = TRUE, updatevalue = NA)
        # terra::plot(check)
        cont_int <- terra::intersect(cont_r1, cont_r2)
        # terra::plot(.int)
        .int$overlap <- as.integer(terra::global(cont_int, "max", na.rm = TRUE))
      }
      .int
    }) |> dplyr::bind_rows()

    ## (B) If there are any overlapping receivers,
    if (any(int$overlap == 1)) {

      ## Process 'overlap' when the receiver was not active
      # ... Any time there is a '0' for activity status of the current receiver (e.g., due to servicing),
      # ... there can be no overlap with that receiver
      # ... so we will set a '0' to all other receivers
      # ... some of which may have been active on that date
      # ... Note the implementation of this step before the step below, when all rows for the receiver
      # ... of interest are (inadvertently) set to 0.
      info[which(info[, as.character(r1)] == 0), ] <- 0

      ## Process 'overlap' for overlapping/non-overlapping receivers
      # For overlapping receivers, we'll leave these as defined in the activity matrix
      # ... (if active, then they overlap;
      # ... if not active, e.g., due to a servicing event for that receiver, then they can't overlap).
      # ... For the non-overlapping receivers, we'll force '0' for the overlap (even if they were active).
      # Get receiver IDs
      overlapping_receivers <- int$receiver_id[int$overlap == 1]
      # For all non-overlapping receivers, set '0' for overlap
      # ... Note that this will include the receiver of interest
      # ... But that doesn't matter because we'll drop that column anyway
      info[, !(colnames(info) %in% overlapping_receivers)] <- 0

      ## (C) If there aren't any spatially overlapping receivers, then the whole matrix just takes on 0
    } else {
      info[] <- 0
    }

    #### Process dataframe
    rnms <- rownames(info)
    info <- data.frame(info)
    colnames(info) <- colnames(rs_active_mat)
    info[, as.character(r1)] <- NULL
    cnms             <- colnames(info)
    info$timestamp   <- as.Date(rnms)
    info$receiver_id <- r1
    info[, c("timestamp", "receiver_id", cnms)]
  })
  names(list_by_receiver) <- names(.containers)
  list_by_receiver        <- compact(list_by_receiver)

  #### On each date, get the vector of overlapping receivers
  # Note that not every receiver in this list will necessarily overlap with every other receiver though.
  lbd <- lapply(list_by_receiver, function(d) {
    tidyr::pivot_longer(
      data = d,
      cols = 3:ncol(d),
      names_to = "receiver_id_2",
      names_transform = list(receiver_id_2 = as.integer)
    )
  })
  lbd <- dplyr::bind_rows(lbd) |> dplyr::filter(.data$value == 1)
  lbd <- lapply(split(lbd, lbd$timestamp), function(d) unique(c(d$receiver_id[1], d$receiver_id_2)))

  ##### Process outputs
  # For the list_by_receiver, we will have one element for each receiver from 1:max(.moorings$receiver_id)
  # ... (for each indexing)
  list_by_receiver <- lapply(as.integer(1:max(.moorings$receiver_id)), function(i) {
    if (i %in% .moorings$receiver_id) {
      return(list_by_receiver[[as.character(i)]])
    } else {
      return(NULL)
    }
  })
  # For the list_by_date (lbd), we will have one element for each date from the start to the end of the array
  days <- as.character(seq(min(.moorings$receiver_start), max(.moorings$receiver_end), "days"))
  list_by_date <- lapply(days, function(day) {
    lbd[[day]]
  })
  names(list_by_date) <- days

  #### Return outputs
  out <- list()
  out$list_by_receiver <- list_by_receiver
  out$list_by_date <- list_by_date
  out
}


#' @title AC* set up: calculate detection probability around a receiver
#' @description This function is an example detection probability function, of the kind required by [`acs_setup_detection_kernels()`].
#' @param .data A one-row [`data.table`] that defines the location of the receiver and associated information used by the model of detection probability.
#' @param .bathy A [`SpatRaster`] that defines the grid over which detection probability is calculated.
#' @param ... Additional arguments (none implemented).
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

acs_setup_detection_pr <- function(.data, .bathy, ...) {
  # Define helper function to calculate detection probability given distance (m)
  calc_dpr <- function(distance) {
    pr <- stats::plogis(2.5 + -0.02 * distance)
    pr[distance > .data$receiver_range] <- 0
    pr
  }
  # Calculate Euclidean distance around receiver
  rxy <- matrix(c(.data$receiver_easting, .data$receiver_northing), ncol = 2)
  cell <- terra::cellFromXY(.bathy, rxy)
  grid <- terra::setValues(.bathy, NA)
  grid[cell] <- 1
  dist <- terra::distance(grid, unit = "m")
  dist <- terra::mask(dist, .bathy)
  # Convert distances to detection pr
  terra::app(dist, calc_dpr)
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
#'                 receiver_range = 500)
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
      dplyr::lead(array_design$array_start_date) - 1
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
