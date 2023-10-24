#' @title AC* helper: internal checks
#' @description These are internal check functions.
#' @author Edward Lavender
#' @name acs_check

#' @rdname acs_check
#' @keywords internal

.acs_check_obs <- function(.obs) {
  if (inherits(.obs, "data.frame") & !inherits(.obs, "data.table")) {
    .obs <- as.data.table(.obs)
  }
  check_inherits(.obs, "data.table")
  check_names(.obs, c("timestep", "timestamp", "date", "detection_id", "detection", "receiver_id", "buffer_past", "buffer_future"))
  check_inherits(.obs$timestep, "integer")
  check_inherits(.obs$timestamp, "POSIXct")
  check_inherits(.obs$detection_id, "integer")
  check_inherits(.obs$detection, "integer")
  check_inherits(.obs$receiver_id, "list")
  check_inherits(.obs$buffer_past, "numeric")
  check_inherits(.obs$buffer_future, "numeric")
  if (!all(
    lubridate::year(.obs$timestamp) == lubridate::year(.obs$date),
    lubridate::month(.obs$timestamp) == lubridate::month(.obs$date),
    lubridate::day(.obs$timestamp) == lubridate::day(.obs$date)
  )) {
    abort("There is a discrepancy between `.obs$timestamp` and `.obs$date`.")
  }
  .obs
}

#' @rdname acs_check
#' @keywords internal

.acs_check_bathy <- function(.bathy) {
  check_inherits(.bathy, "SpatRaster")
}

#' @rdname acs_check
#' @keywords internal

.acs_check_detection_overlaps <- function(.detection_overlaps) {
  if (!is.null(.detection_overlaps)) {
    check_named_list(.detection_overlaps)
    check_names(.detection_overlaps, c("list_by_receiver", "list_by_date"))
  }
}

#' @rdname acs_check
#' @keywords internal

.acs_check_detection_kernels <- function(.detection_kernels, .bathy) {
  check_named_list(.detection_kernels)
  check_names(.detection_kernels, c("receiver_specific_kernels",
                    "receiver_specific_inv_kernels",
                    "array_design",
                    "array_design_by_date",
                    "bkg_surface_by_design",
                    "bkg_inv_surface_by_design"))
  # Check bathy, detection_overlaps & detection_kernels
  if (!terra::compareGeom(.bathy, compact(.detection_kernels$receiver_specific_kernels)[[1]],
                          messages = TRUE, stopOnError = FALSE)) {
    abort("The properties of the bathymetry grid and the detection kernel SpatRaster(s) are not equal.")
  }
}

#' @rdname acs_check
#' @keywords internal

.acs_check_write_record <- function(.write_record, .element = "filename") {
  if (!is.null(.write_record)) {
    check_named_list(.write_record)
    check_names(.write_record, .element)
    if (length(.write_record[[.element]]) != 1L) {
      abort("`.write_record${.element}` should be a single directory in which to write files.",
            .envir = environment())
    }
    check_dir(.write_record[[.element]])
    if (length(list.files(.write_record[[.element]])) != 0L) {
      warn("`.write_record${.element}` ('{.write_record[[.element]]}') is not an empty directory.",
           .envir = environment())
    }
  }
  .write_record[[.element]]
}


#' @rdname acs_check
#' @export

.acs_check_present <- function(.p, .t, .type = c("acs", "dc")) {
  is_blank <- as.logical(terra::global(.p, \(x) all(is.na(x) | x == 0)))
  if (is_blank) {
    .type <- match.arg(.type)
    if (.type == "acs") {
      abort("There are no possible locations at time step = {.t}. There may be errors in the data (e.g., false detections) or detection probability model and/or mobility may be too restrictive. It is also possible this is a bug.", .envir = environment())
    } else if (.type == "dc") {
      abort("There are no possible locations at time step = {.t}. There may be errors in the observations or bathymetry data, or the depth model may be incorrect.", .envir = environment())
    }
  }
  NULL
}

#' @title AC* helper: define the receiver(s) at which the next detection was recorded
#' @description This function, for each time step, the receiver(s) at which the next detection was recorded.
#' @param .receiver_id A `list` column.
#' @return The function returns a `list` column that defines, for each time step, the receiver(s) that recorded the next detection.
#' @author Edward Lavender
#' @seealso The `receiver_id_next` column is required by [`pf_forward_2()`].
#' @keywords internal

.acs_setup_obs_receiver_id_next <- function(.receiver_id) {
  rlang::check_installed("zoo")
  dt <- data.table(receiver_id = .receiver_id)
  dt$receiver_id[sapply(dt$receiver_id, is.null)] <- list(NA_integer_)
  out <-
    dt |>
    mutate(receiver_id_next = dplyr::lead(.data$receiver_id),
           receiver_id_next = zoo::na.locf(.data$receiver_id_next,
                                           fromLast = TRUE,
                                           na.rm = FALSE)) |>
    as.data.table()
  out$receiver_id_next[nrow(out)][[1]] <- NA_integer_
  out$receiver_id_next
}

#' @title AC* helper: define active, overlapping receivers with absences
#' @description Given a detection at one or more receivers on a given date, this function defines the set of remaining, active, overlapping receivers that did not record detections.
#' @param .date A `character` that defines the date (e.g., `obs$date[t]` internally in [`acs()`]).
#' @param .detections An `integer` vector that defines the receiver(s) that recorded detection(s) at the current time step (e.g., `recs_current` or `recs_next` internally in [`acs()`]).
#' @param .overlaps A named `list` from [`acs_setup_detection_overlaps()`] that defines receiver overlaps (e.g., `detection_overlaps` internally in [`acs()`]). `NULL` is permitted.
#' @details In the AC* algorithms, at the moment of detection, the probability kernels that describe the possible locations of an individual given the data depend on both the receivers that record detections and those that did not (eqn S5 in Lavender et al., 2023). This function is used to restrict the set of receivers to which eqn S5 needs to be applied.
#'
#' # Warning
#' For speed, this function performs no internal checks.
#'
#' @return The function returns an `integer` vector that defines the set of receivers that overlap with `.detections` but did not record detections. `NULL` indicates no overlapping receivers.
#' @seealso This function defines the `absences` argument for [`.acs_given_detection_SpatRaster()`] and [`.acs_given_detection_particles()`].
#' @author Edward Lavender
#' @keywords internal

.acs_absences <- function(.date, .detections, .overlaps){
  absences <- NULL
  if (!is.null(.overlaps)) {
    # Define overlapping receivers
    absences <-
      lapply(.detections, function(r) {
        ov <- .overlaps$list_by_receiver[[r]]
        ov <- ov[rownames(ov) == .date, 3:ncol(ov)]
        colnames(ov)[ov == 1]
      }) |>
      unlist() |>
      as.integer() |>
      unique()
    # Define the set of overlapping receivers that did not record detections
    absences <- absences[!(absences %in% .detections)]
    if (length(absences) == 0L) {
      absences <- NULL
    }
  }
  absences
}

#' @title AC* helper: define the individual's location given detection(s)
#' @description These function defines the relative plausibility possible locations of an individual given one or more detections, either across a [`SpatRaster`] (for [`acs()`]) or for selected particle positions (for [`pf_forward_2()`]).
#' @param .detections An `integer` vector of the receiver(s) that recorded detections at a given time step.
#' @param .absences An `integer` vector of the remaining, overlapping receiver(s) that did not record a detection, from [`.acs_absences()`].
#' @param .kernels A `list` from [`acs_setup_detection_kernels`].
#' @param .zero_to_na For [`.acs_given_detection_SpatRaster`], `.zero_to_na` is a `logical` variable that defines whether or not to classify zeros as `NA`s. This should be `FALSE` for defining `given_data` in [`acs()`], but `TRUE` for defining `next_kernel` in [`acs()`], so that `next_kernel` is correctly buffered.
#' @param .particles For [`.acs_given_detection_particles`], `.particles` is a [`data.table`] witha `cell_now` column that defines particle locations on the grid.

#' @details In the AC* algorithms, at the moment of detection, the probability kernels that describe the possible locations of an individual given the data depend on both the receivers that record detections and those that did not (eqn S5 in Lavender et al., 2023). This function solves eqn S5. For computational efficiency, the equation is solved in a stepwise manor such that the number of necessary operations is kept to a minimum.
#'
#' # Warning
#' For speed, these functions performs no internal checks.
#'
#' @author Edward Lavender
#' @name acs_given_detection

#' @rdname acs_given_detection
#' @keywords internal

.acs_given_detection_SpatRaster <- function(.detections, .absences, .kernels, .zero_to_na = FALSE) {
  # Define kernel around receiver(s) with detection(s)
  if (length(.detections) == 1L) {
    k <- .kernels$receiver_specific_kernels[[.detections]]
  } else {
    k <- do.call(c, .kernels$receiver_specific_kernels[.detections])
    k <- terra::app(k, "prod")
  }
  # Define inverse kernels around receiver(s) without detection(s)
  if (!is.null(.absences)) {
    if (length(.absences) == 1L) {
      ka <- .kernels$receiver_specific_inv_kernels[[.absences]]
    } else {
      ka <- do.call(c, .kernels$receiver_specific_inv_kernels[.absences])
      ka <- terra::app(ka, "prod")
    }
    k <- k * ka
  }
  # Reclassify 0 to NA
  # * This is necessary for next_kernel in .acs() b/c
  # * ... terra::buffer() ignores NAs but not zeros
  if (.zero_to_na) {
    k <- terra::classify(k, cbind(0, NA))
  }
  k
}

#' @rdname acs_given_detection
#' @keywords internal

.acs_given_detection_particles <- function(.detections, .absences, .kernels, .particles) {
  # Calculate Pr (detection | position) at each relevant receiver
  ldc <- length(.detections)
  mat <- matrix(NA, nrow(.particles), ncol = ldc)
  for (i in seq_len(ldc)) {
    mat[, i] <- terra::extract(.kernels$receiver_specific_kernels[[.detections[i]]], .particles$cell_now)[, 1]
  }
  # Calculate Pr (non detection | position) at each relevant receiver
  if (!is.null(.absences)) {
    mat_2 <- matrix(NA, nrow(.particles), ncol = length(.absences))
    lac <- length(.absences)
    for (i in seq_len(lac)) {
      mat_2[, i] <- terra::extract(.kernels$receiver_specific_inv_kernels[[.absences[i]]], .particles$cell_now)[, 1]
    }
    mat <- cbind(mat, mat_2)
  }
  # Calculate Pr (all data | position)
  colProds.matrix(mat)
}

#' @title AC* helper: define AC* container(s)
#' @description These functions define AC* containers in [`pf_forward_2()`].
#' @param .obs The `.obs` [`data.table`].
#' @param .moorings A [`data.table`] that defines receiver locations.
#' @param .coords A character vector that defines the name of the columns in `.moorings` that define receiver coordinates.
#' @param .receivers An `integer` vector of receivers for which to define containers, used to subset `.moorings`.
#' @param .bathy The bathymetry [`SpatRaster`].
#' @param .buffer A `numeric` value that defines the container radius.
#'
#' @details
#'
#' In [`acs()`], acoustic containers are defined using [`terra::buffer()`].
#'
#' These functions are used in [`pf_forward_2()`].
#' * [`.acs_container()`] defines the acoustic container around a particular set of receivers.
#' * [.acs_container_1()] defines the acoustic container at the first time step, which is the intersection between the container(s) around the receivers that recorded the first detection (if applicable) and the container(s) around the receivers that recorded the next detection.
#'
#' @return The function returns an [`sf::sf`] object (the acoustic container).
#'
#' @author Edward Lavender
#' @name acs_container

#' @rdname acs_container
#' @keywords internal

.acs_container <- function(.moorings,
                           .coords = c("receiver_x", "receiver_y"),
                           .receivers,
                           .bathy,
                           .buffer) {
  # Define receiver locations
  xy <-
    .moorings |>
    filter(.data$receiver_id %in% .receivers) |>
    select(dplyr::all_of(.coords)) |>
    as.matrix()
  cells <- terra::cellFromXY(.bathy, xy)

  # Define buffer(s)
  bufs <- lapply(cells, function(cell) {
    buf <- terra::setValues(.bathy, NA)
    buf[cell] <- 1
    buf <- terra::buffer(buf, .buffer)
  })

  # Define the intersection between buffer(s)
  # * This defines the possible locations of the individual
  spatIntersect(bufs)
}

#' @rdname acs_container
#' @keywords internal

.acs_container_1 <- function(.obs, .moorings, .bathy) {

  #### Define starting surface based on AC algorithm & extensions
  # * If the first time step is a detection, we will account for the current & next buffer
  # * If the first time step is not a detection, we will account for the next buffer only
  # * Pr(detection | position) is calculated accordingly

  # Define positions with detections
  pos_detections <- which(!sapply(.obs$receiver_id, is.null))

  # (A) Define possible locations given past (if applicable)
  start_with_detection <- .obs$detection[pos_detections[1]] == 1L
  if (start_with_detection) {
    # Identify the receivers that recorded the 'current' detection
    pos_current       <- pos_detections[1]
    receivers_current <- .obs$receiver_id[pos_current][[1]]
    # Define buffer around receiver(s)
    # * I.e., the detection container for the current time step
    detection_container <-
      .acs_container(.moorings,
                     .receivers = receivers_current,
                     .bathy = .bathy,
                     .buffer = .obs$buffer_past[pos_current])
  }

  # (B) Define possible locations given future
  # Define the receivers at which the next detection was recorded
  # * If we start with a detection, we simply identify the next detection
  # * Otherwise, the next detection is effectively the first detection (see below)
  pos_next <- ifelse(start_with_detection, yes = pos_detections[2], no = pos_detections[1])
  receivers_next <- .obs$receiver_id[[pos_next]]
  future_container <-
    .acs_container(.moorings,
                   .receivers = receivers_next,
                   .bathy = .bathy,
                   .buffer = .obs$buffer_past[pos_current])

  # (C) Define container accounting for detection & future
  if (start_with_detection) {
    container <- spatIntersect(list(detection_container, future_container))
  } else {
    container <- future_container
  }
  container

}

#' @title AC* helper: AC* filters
#' @description These functions filter particles according to AC* criteria in [`pf_forward_2()`].
#' @param .particles A [`data.table`] that defines particle locations. This should contain two columns named `x_now` and `y_now`.
#' @param .moorings A [`data.table`] of receiver IDs and locations.
#' @param .receivers An `integer` vector of receiver(s) at which the individual was next detected, used to filter `.moorings`.
#' @param .threshold A `numeric` value that defines the distance from `.receivers` within which valid particles are located.
#' @param .bathy The bathymetry [`SpatRaster`]. `NAs` are taken to define inhospitable habitats (e.g., on land).
#'
#' @return The functions return the `.particles` [`data.table`] with the subset of particles that passed the filtering criteria.
#'
#' @author Edward Lavender
#' @name ac_filter

#' @rdname ac_filter
#' @keywords internal

.acs_filter_by_land <- function(.particles, .bathy) {
  .particles |>
    filter(!is.na(terra::extract(.bathy, cbind(.data$x_now, .data$y_now))[, 1])) |>
    as.data.table()
}

#' @rdname ac_filter
#' @keywords internal

.acs_filter_by_container <- function(.particles, .moorings, .receivers, .threshold) {
  # Calculate distances between particle samples & the receivers that recorded the next detection
  dist <- terra::distance(.particles |>
                            select("x_now", "y_now") |>
                            as.matrix(),
                          .moorings |>
                            filter(.data$receiver_id %in% .receivers) |>
                            select("receiver_x", "receiver_y") |>
                            as.data.table() |>
                            as.matrix(),
                          lonlat = FALSE)
  # Eliminates particles using distance threshold
  # * Particles are always within `mobility` of the past container
  # * Particles are forced to be within (all) future containers
  .particles[Rfast::rowsums(dist <= .threshold) == ncol(dist), ]
}
