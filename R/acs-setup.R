#' @title AC* setup: Set up movement datasets
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
#' * Information required by the AC* algorithms (utimately [`.acs()`]) is added;
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
#' @author Edward Lavender
#' @export

acs_setup_obs <- function(.acoustics, .archival = NULL, .step, .mobility) {

  #### Check user inputs
  check_acoustics(.acoustics)
  check_archival(.archival)
  check_inherits(.step, "character")
  if (!is.null(.archival)) {
    # Check archival time series are spaced `.step` apart
    .step_req <- as.integer(lubridate::seconds(lubridate::period(.step)))
    .step_obs <- as.numeric(difftime(.archival$timestamp[2], .archival$timestamp[1], .step, units = "secs"))
    if (!isTRUE(all.equal(.step_req, .step_obs))) {
      abort("Archival time series are not spaced `.step` ('{.step}') units apart (observed step: {.step_obs} units).")
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
    mutate(detection_id = dplyr::row_number()) |>
    as.data.table()

  #### Align time series
  if (!is.null(.archival)) {
    # Filter acoustics by archival time series
    .acoustics <-
      .acoustics |>
      filter(.data$timestamp >= min(.archival$timestamp) &
               .data$timestamp <= max(.archival$timestamp))
    # Filter archival time series by acoustic time series
    .archival <-
      .archival |>
      filter(.data$timestamp >= min(.acoustics$timestamp) &
               .data$timestamp <= max(.acoustics$timestamp))
    if (nrow(.acoustics) == 0L | nrow(.archival) == 0L) {
      abort("There are no remaining observations after aligning the acoustic and archival time series.")
    }
  }

  #### Define output time series with acoustic & archival data
  # Define regular time series
  out <- data.table(timestamp = seq(min(.acoustics$timestamp), max(.acoustics$timestamp), by = .step))
  # Add acoustic data
  detection <- NULL
  out[, detection := (timestamp %in% .acoustics$timestamp) + 0]
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
           detection_id = data.table::nafill(.data$detection_id, type = "locf")) |>
    group_by(.data$detection_id) |>
    mutate(step_forwards = dplyr::row_number(),
           step_backwards = rev(.data$step_forwards),
           # We buffer the past by mobility
           buffer_past = .mobility,
           # We shrink the future
           buffer_future = .mobility * .data$step_backwards) |>
    ungroup() |>
    arrange(.data$timestamp) |>
    mutate(timestep = dplyr::row_number()) |>
    select(.data$timestep,
           .data$timestamp, .data$date,
           .data$detection_id, .data$detection, .data$receiver_id,
           .data$buffer_past, .data$buffer_future,
           dplyr::any_of("depth")) |>
    as.data.table()

}


#' @title Set up detection containers
#' @description This function defines receiver detection containers.
#' @param .grid A [`SpatRaster`].
#' @param .moorings A [`data.table`]
#'
#' @details Details
#'
#' @return The function returns a named list.
#'
#' @examples
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
#' @author Edward Lavender
#' @export

acs_setup_detection_containers <- function(.grid, .moorings) {

  # TO DO
  # Add checks
  # grid can contain NAs
  # moorings should be data.table contain required columns (receiver_id, receiver_easting, receiver_northing, receiver_range)
  # receiver_ids should be integer from 1 to n
  # receiver-specific ranges permitted
  # For time-step specific ranges, these are computed on the fly in ac()
  check_names(input = .moorings, req = "receiver_range")

  rs <- seq_len(max(.moorings$receiver_id))
  containers <-
    pbapply::pblapply(rs, function(id) {
      out <- NULL
      bool <- .moorings$receiver_id == id
      if (any(bool)) {
        d <- .moorings[which(bool), ]
        g   <- terra::setValues(.grid, NA)
        rxy <- matrix(c(d$receiver_easting, d$receiver_northing), ncol = 2)
        g[terra::cellFromXY(g, rxy)] <- 1
        # terra::plot(g)
        out <- terra::buffer(g, d$receiver_range)
        out <- (terra::mask(out, .grid)) + 0
        # terra::plot(out)
      }
      out
    })
  names(containers) <- rs
  containers
}


#' @title Set up detection container overlaps
#' @description
#'
#' @param .containers A named list
#' @param .moorings A [`data.table`]
#' @param .services A [`data.table`]
#'
#' @details Details.
#'
#' @return The function returns a named list.
#'
#' @examples
#'
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


#' @title AC* set up: Calculate detection probability around a receiver
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


#' @title Setup detection kernels
#' @description TO DO
#' @param .moorings TO DO
#' @param .services TO DO
#' @param .calc_detection_pr,... TO DO
#' @param .bathy TO DO
#' @param .verbose TO DO
#'
#' @examples
#' #### Define example 'moorings' & 'services' dataset
#' # receivers 3 and 4 overlap in space but receiver 5 is further afield
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
#'
#' #### Implement function
#' k <- acs_setup_detection_kernels(m, s,
#'                                  .calc_detection_pr = acs_setup_detection_pr,
#'                                  .bathy = dat_gebco())
#'
#' #### TO DO (continue)
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
    cat_to_console(paste0("flapper::acs_setup_detection_kernels() called (@ ", t_onset, ")..."))
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
          warn("Detection probability is NA at receiver {m$receiver_id}.")
        } else if (pr_at_receiver == 0) {
          warn("Detection probability is 0 at receiver {m$receiver_id}.")
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
    array_design_intervals <- data.frame(
      array_id = 1:nrow(rs_mat_cp),
      array_start_date = as.Date(rownames(rs_mat_cp))
    )
    array_design_intervals$array_end_date <-
      dplyr::lead(array_design_intervals$array_start_date) - 1
    array_design_intervals$array_end_date[nrow(array_design_intervals)] <-
      max(.moorings$receiver_end)
    array_design_intervals$array_interval <-
      lubridate::interval(
        array_design_intervals$array_start_date,
        array_design_intervals$array_end_date
      )

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
    out$array_design_intervals        <- array_design_intervals
    out$bkg_surface_by_design         <- bkg_by_design
    out$bkg_inv_surface_by_design     <- bkg_inv_by_design
    # Check function duration
    t_end <- Sys.time()
    total_duration <- difftime(t_end, t_onset, units = "mins")
    cat_to_console(paste0("... flapper::acs_setup_detection_kernels() call completed (@ ", t_end, ") after ~", round(total_duration, digits = 2), " minutes."))
    # Return outputs
    out
  }
