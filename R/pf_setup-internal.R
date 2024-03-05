#' @title PF: set up internals
#' @description These are internal functions that support [`pf_setup_obs()`].
#' @name pf_setup-internal

#' @rdname pf_setup-internal
#' @keywords internal

# Identify unique array designs
.setup_arrays <- function(.dlist) {
  # Get receiver status matrix from moorings and services
  # * Units are days (time unit is Date)
  rs_mat <- make_matrix_receivers(.dlist = .dlist,
                                  .delta_t = "days",
                                  .as_POSIXct = NULL)
  # Get receiver status change points
  # * I.e., dates when the array design changed
  rs_mat_cp <- unique(rs_mat)
  # Define the time interval for each array design
  array_design <- data.frame(
    array_id = 1:nrow(rs_mat_cp),
    array_start_date = as.Date(rownames(rs_mat_cp))
  )
  array_design$array_end_date <-
    lead(array_design$array_start_date) - 1
  array_design$array_end_date[nrow(array_design)] <-
    max(.dlist$data$moorings$receiver_end)
  array_design$array_interval <-
    lubridate::interval(
      array_design$array_start_date,
      array_design$array_end_date
    )
  array_design
}

#' @rdname pf_setup-internal
#' @keywords internal

# Process acoustic detections
# * Round time series & drop duplicates
.setup_acoustics <- function(.acoustics, .step) {
  .acoustics |>
    lazy_dt(immutable = TRUE) |>
    mutate(timestamp = lubridate::round_date(.data$timestamp, .step),
           date = as.Date(timestamp)) |>
    group_by(.data$receiver_id, .data$timestamp) |>
    slice(1L) |>
    ungroup() |>
    as.data.table()
}

#' @rdname pf_setup-internal
#' @keywords internal

# Process archival observations
# * Round time series & abort if duplicates
.setup_archival <- function(.archival, .step) {
  timestamp <- NULL
  .archival[, timestamp := lubridate::round_date(timestamp, .step)]
  # Validate duplicate observations
  tbl <- table(.archival$timestamp)
  if (any(tbl > 1L)) {
    abort("Multiple observations per `.step` in archival data are not currently supported. Are there multiple individuals in the archival data?")
  }
  .archival
}

#' @rdname pf_setup-internal
#' @keywords internal

# Align acoustic and archival time series
.setup_alignment <- function(.acoustics, .archival) {
  start <- max(c(min(.acoustics$timestamp), min(.archival$timestamp)))
  end   <- min(c(max(.acoustics$timestamp), max(.archival$timestamp)))
  .acoustics <-
    .acoustics |>
    filter(.data$timestamp >= start & .data$timestamp <= end) |>
    as.data.table()
  .archival <-
    .archival |>
    filter(.data$timestamp >= start & .data$timestamp <= end) |>
    as.data.table()
  if (nrow(.acoustics) == 0L | nrow(.archival) == 0L) {
    abort("There are no remaining observations after aligning the acoustic and archival time series.")
  }
  list(acoustics = .acoustics, archival = .archival)
}

#' @rdname pf_setup-internal
#' @keywords internal

# Define a blank time series
.setup_series <- function(.acoustics, .archival, .period, .step, .mobility) {
  if (!is.null(.period)) {
    start <- min(.period)
    end   <- max(.period)
  } else {
    timestamps <- list(.acoustics[["timestamp"]], .archival[["timestamp"]])
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
  data.table(timestep = seq_len(length(tss)),
             timestamp = tss,
             date = as.character(as.Date(tss)),
             mobility = .mobility)
}

#' @rdname pf_setup-internal
#' @keywords internal

# Add acoustic array IDs
.add_acoustics_arrays <- function(.obs, .dlist, .step) {
  # Define array IDs
  arrays <-
    .setup_arrays(.dlist) |>
    mutate(array_start_date = as.POSIXct(.data$array_start_date,  tz = "UTC"),
           array_end_date = as.POSIXct(.data$array_end_date, tz = "UTC")) |>
    group_by(.data$array_id) |>
    reframe(timestamp = seq(.data$array_start_date, .data$array_end_date, .step)) |>
    select("array_id", "timestamp") |>
    as.data.table()
  # Add to data.table
  array_id <- NULL
  .obs[, array_id := arrays$array_id[fmatch(timestamp, arrays$timestamp)]]
  .obs
}

#' @rdname pf_setup-internal
#' @keywords internal

# Add acoustic detection IDs
.add_acoustics_flags <- function(.obs, .acoustics) {

  # Summarise .acoustics by time step
  .acoustics <-
    .acoustics |>
    # List receivers with detections at each time step & define detection IDs
    group_by(.data$timestamp) |>
    summarise(receiver_id = list(unique(.data$receiver_id))) |>
    ungroup() |>
    arrange(timestamp) |>
    mutate(detection_id = as.integer(row_number())) |>
    as.data.table()

  # Add to data.table
  .obs |>
    mutate(detection = as.integer((timestamp %in% .acoustics$timestamp) + 0)) |>
    merge(.acoustics, all.x = TRUE, by = "timestamp") |>
    as.data.table()
}

#' @rdname pf_setup-internal
#' @keywords internal

# Add acoustic observation matrices
.add_acoustics_obs <- function(.obs, .dlist, .acoustics) {

  #### Process acoustics
  # .acoustics should be a simple data.table of detections
  detection <- NULL
  .acoustics[, detection := 1L]

  #### Define the complete set of acoustic observations (0, 1)
  # We only detections & overlapping absences at this stage
  .acoustics <-
    lapply(split(.acoustics, by = "timestamp"), function(d) {
      # Identify receivers that recorded detections
      # d$receiver_id
      # Identify overlapping receiver(s) that did not record detection(s)
      a <- .acs_absences(.date = d$date[1],
                         .detections = d$receiver_id,
                         .overlaps = .dlist$algorithm$detection_overlaps)
      # Expand data accordingly
      if (!is.null(a)) {
        d <-  rbind(d,
                    data.table(timestamp = d$timestamp[1],
                               date = d$date[1],
                               receiver_id = a,
                               detection = 0L))
      }
      d
    }) |> rbindlist()

  #### Define detection matrices
  # Define a blank list of acoustic observations matrices (one per time step)
  timestamps <- seq(min(.acoustics$timestamp), max(.acoustics$timestamp), "2 mins")
  als <- lapply(timestamps, function(x) NULL)
  names(als) <- timestamps
  # Define the detection matrix
  # * This contains NAs for non-overlapping or non-operational receivers
  dmat <-
    .acoustics |>
    select("timestamp", "receiver_id", "detection") |>
    collapse::pivot(ids = "timestamp",
                    values = "detection",
                    names = "receiver_id",
                    how = "wider")
  rnms <- as.character(dmat$timestamp)
  dmat <- as.matrix(dmat[, 2:ncol(dmat)])
  # Define a list of detection matrices
  dls <-
    lapply(seq_row(dmat), function(i) {
      # Identify detection data (0, 1, NA) for time step
      acc <- dmat[i, , drop = FALSE]
      if (all(acc == 0, na.rm = TRUE)) {
        # Use NULL if all detection data are 0 or NA
        # (This is purely to save space)
        return(NULL)
      } else {
        # Drop any NAs
        # * Focus on operational receivers
        # * Focus on the set of overlapping receivers (if applicable)
        return(acc[, complete.cases(acc[, ])])
      }
    })
  names(dls) <- rnms
  # Define the list of acoustic observations
  # * This is a list of matrices, one per time step
  # * If there were no detections, we use NULL
  # * (b/c we simply extract likelihoods from the precomputed layer)
  # * If there was a detection, we include 1s and 0s for overlapping receivers
  # * We do not need to include information from all receivers
  als[names(dls)] <- dls
  als

  #### Add to obs
  acoustics <- NULL
  .obs[, acoustics := als]
  .obs

}

#' @rdname pf_setup-internal
#' @keywords internal

# Add receiver_id_next
.add_acoustics_ridnxt <- function(.receiver_id) {
  rlang::check_installed("zoo")
  dt <- data.table(receiver_id = .receiver_id)
  dt$receiver_id[sapply(dt$receiver_id, is.null)] <- list(NA_integer_)
  out <-
    dt |>
    mutate(receiver_id_next = lead(.data$receiver_id),
           receiver_id_next = zoo::na.locf(.data$receiver_id_next,
                                           fromLast = TRUE,
                                           na.rm = FALSE)) |>
    as.data.table()
  out$receiver_id_next[nrow(out)][[1]] <- NA_integer_
  out$receiver_id_next
}

#' @rdname pf_setup-internal
#' @keywords internal

# Add acoustic container information
# * At each time step we filter particles outside of detection containers
# * This requires information the coordinates of future receivers
# * And the
.add_acoustics_containers <- function(.obs, .dlist, .mobility) {

  # Define receivers & buffers
  .obs <-
    .obs |>
    # Define the receiver(s) at which the individual was next detected
    arrange(.data$timestamp) |>
    mutate(receiver_id_next = .add_acoustics_ridnxt(.data$receiver_id),
           # Define allowable distances from next receivers
           detection_id = as.integer(data.table::nafill(.data$detection_id, type = "locf"))) |>
    group_by(.data$detection_id) |>
    mutate(buffer_future = .mobility * rev(row_number())) |>
    ungroup() |>
    arrange(.data$timestamp) |>
    as.data.table()

  # Define the required list of container information for each time step
  # * A matrix of receiver coordinates
  # * The buffer value(s)
  cinfo <- cl_lapply(seq_row(.obs), .fun = function(t) {
    r <- .obs$receiver_id_next[[t]]
    if (is.null(r)) {
      return(NULL)
    }
    # Define receiver data
    m <-
      .dlist$data$moorings |>
      lazy_dt() |>
      filter(.data$receiver_id %in% r[[1]]) |>
      select("receiver_x", "receiver_y", "receiver_range") |>
      as.data.table()
    # Define receiver coordinates
    mxy <- cbind(m$receiver_x, m$receiver_y)
    # Define receiver buffer(s), dependent upon on:
    # * `buffer_future` (mobility & delta T)
    # * `receiver_range` (receiver-specific)
    mbu <- .obs$buffer_future[t] + m$receiver_range
    list(coord = mxy, buffer = mbu)
  })

  # Add to data.table
  container <- NULL
  .obs[, container := cinfo]
  .obs

}

#' @rdname pf_setup-internal
#' @keywords internal

# Add archival observations
.add_archival <- function(.obs, .archival) {
  depth <- timestamp <- NULL
  .obs[, depth := .archival$depth[fmatch(timestamp, .archival$timestamp)]]
  bool <- is.na(.obs$depth)
  if (any(bool)) {
    nrw <- fnrow(.obs)
    nob <- nrw - length(which(bool))
    warn("There are {nob}/{nrw} ({round(nob / nrw * 100, digits = 1)} %) archival observations within the time series.",
         .envir = environment())
    if (all(bool)) {
      warn("The depth column has been dropped.")
      .obs[, depth := NULL]
    }
  }
  .obs
}
