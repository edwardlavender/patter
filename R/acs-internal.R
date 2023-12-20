#' @title AC* helper: AC* container(s)
#' @description [`.acs_container_1()`] is designed for implementations of the AC*PF algorithm by [`pf_forward()`]. The function defines the first AC* container in [`.pf_rpropose_origin()`].
#'
#' @details
#' At the first time step, the acoustic container is the intersection between the container(s) around the receivers that recorded the first detection (if applicable) and the container(s) around the receivers that recorded the next detection.
#'
#' @return The functions return a [`SpatRaster`] or a [`SpatVector`].
#'
#' @author Edward Lavender
#' @name acs_container

#' @rdname acs_container
#' @keywords internal

.acs_container_detection.SpatRaster <- function(.receivers, .detection_kernels) {
  # Define container as SpatRaster
  # * Cells are 1 (inside container), 0 (outside container) or NA (on land)
  .detection_kernels$receiver_specific_kernels[.receivers] |>
    spatIntersect(.value = NULL, .fun = function(x) all(x > 0))
}

#' @rdname acs_container
#' @keywords internal

.acs_container_detection.SpatVector <- function(.receivers, .moorings) {
  # Define container as SpatVector
  # * Buffer each receiver & then intersect
  .receivers |>
    lapply(function(r) {
      i <- which(.moorings$receiver_id == r)
      cbind(cbind(.moorings$receiver_x[i], .moorings$receiver_y[i])) |>
        terra::vect() |>
        terra::buffer(.moorings$receiver_range[i], quadsegs = 1e3)
    }) |>
    spatIntersect()
}

#' @rdname acs_container
#' @keywords internal

.acs_container_detection <- function(.receivers, .detection_kernels, .moorings) {
  # Use .detection_kernels to decide which function to use
  if (!is.null(.detection_kernels)) {
    .acs_container_detection.SpatRaster(.receivers = .receivers,
                                        .detection_kernels = .detection_kernels)
  } else {
    .acs_container_detection.SpatVector(.receivers = .receivers,
                                        .moorings = .moorings)
  }
}

#' @rdname acs_container
#' @keywords internal

.acs_container_future.SpatRaster <- function(.receivers, .detection_kernels, .buffer) {
  # Define future container
  # * Cells are 1 (within container) or 0 (outside of buffer)
  # * NAs are dropped via terra::buffer()
  lapply(.receivers, function(r) {
    # * Define detection detection container for selected receiver
    # * Buffer detection container by .buffer (e.g., .obs$buffer_future)
    # * This assumes a constant detection range across all .receivers
    .detection_kernels$receiver_specific_kernels[[r]] |>
      terra::classify(cbind(0, NA)) |>
      terra::buffer(.buffer)
  }) |>
    spatIntersect(.value = 1)
}

#' @rdname acs_container
#' @keywords internal

.acs_container_future.SpatVector <- function(.receivers, .moorings, .buffer) {
  # Define future container as SpatVector (for speed)
  # * Buffer receivers by detection range + buffer & intersect
  .receivers |>
    lapply(function(r) {
      i <- which(.moorings$receiver_id == r)
      cbind(cbind(.moorings$receiver_x[i], .moorings$receiver_y[i])) |>
        terra::vect() |>
        terra::buffer(width = .moorings$receiver_range[i] + .buffer,
                      quadsegs = 1e3)
    }) |>
    spatIntersect()
}

#' @rdname acs_container
#' @keywords internal

.acs_container_future <- function(.receivers, .detection_kernels, .moorings, .buffer) {
  # Use .detection_kernels to decide which function to use
  if (!is.null(.detection_kernels)) {
    .acs_container_future.SpatRaster(.receivers = .receivers,
                                     .detection_kernels = .detection_kernels,
                                     .buffer = .buffer)
  } else {
    .acs_container_future.SpatVector(.receivers = .receivers,
                                     .moorings = .moorings,
                                     .buffer = .buffer)
  }
}

#' @rdname acs_container
#' @keywords internal

.acs_container_1 <- function(.obs, .detection_kernels, .moorings) {

  #### Define starting surface based on AC algorithm & extensions
  # * If the first time step is a detection, we will account for the current & next buffer
  # * If the first time step is not a detection, we will account for the next buffer only
  # * Pr(detection | position) is calculated accordingly

  # Define positions with detections
  pos_detections <- which(!sapply(.obs$receiver_id, is.null))
  if (length(pos_detections) == 0L) {
    abort("There are no detections in the acoustic time series.")
  }

  # (A) Define possible locations given past (if applicable)
  start_with_detection <- .obs$detection[pos_detections[1]] == 1
  detection_container <- NULL
  if (start_with_detection) {
    # Identify the receivers that recorded the 'current' detection
    pos_current       <- pos_detections[1]
    receivers_current <- .obs$receiver_id[pos_current][[1]]
    # Define buffer around receiver(s)
    # * I.e., the detection container for the current time step
    detection_container <- .acs_container_detection(.receivers = receivers_current,
                                                    .detection_kernels = .detection_kernels,
                                                    .moorings = .moorings)
    if (spatIsEmpty(detection_container)) {
      abort("Detection container(s) at t = 1 do not intersect.")
    }
  }

  # (B) Define possible locations given future
  # Define the receivers at which the next detection was recorded
  # * If we start with a detection, we simply identify the next detection
  # * Otherwise, the next detection is effectively the first detection (see below)
  future_container <- NULL
  if (length(pos_detections) > 1L) {
    pos_next <- ifelse(start_with_detection, yes = pos_detections[2], no = pos_detections[1])
    receivers_next <- .obs$receiver_id[[pos_next]]
    future_container <- .acs_container_future(receivers_next,
                                              .detection_kernels = .detection_kernels,
                                              .moorings = .moorings,
                                              .buffer = .obs$buffer_future[1])
    if (spatIsEmpty(future_container)) {
      abort("Future container(s) at t = 1 do not intersect.")
    }
  }

  # (C) Define container accounting for detection & future
  container <- spatIntersect(list(detection_container, future_container),
                             .value = NULL, .fun = function(x) all(x > 0))
  # For SpatRaster containers(0, 1), convert zero to NA for spatSampleDT()
  if (inherits(container, "SpatRaster")) {
    container <- terra::classify(container, cbind(0, NA))
  }
  if (spatIsEmpty(container)) {
    abort("Detection & future container(s) at t = 1 do not intersect.")
  }
  container
}

#' @title AC* helper: detection absences
#' @description Given a detection at one or more receivers on a given date, this function defines the set of remaining, active, overlapping receivers that did not record detections.
#' @param .date A `character` that defines the date.
#' @param .detections An `integer` vector that defines the receiver(s) that recorded detection(s) at the current time step.
#' @param .overlaps A named `list` from [`acs_setup_detection_overlaps()`] that defines receiver overlaps. `NULL` is permitted.
#' @details In the AC* algorithms, at the moment of detection, likelihood of the acoustic data given a particle sample depends on depends on both the receivers that record detections and those that did not (eqn S5 in Lavender et al., 2023). This function is used to restrict the set of receivers to which eqn S5 needs to be applied.
#'
#' # Warning
#' For speed, this function performs no internal checks.
#'
#' @return The function returns an `integer` vector that defines the set of receivers that overlap with `.detections` but did not record detections. `NULL` indicates no overlapping receivers.
#' @seealso This function defines the `absences` argument for [`.pf_lik_ac_detection()`].
#' @author Edward Lavender
#' @keywords internal

.acs_absences <- function(.date, .detections, .overlaps){
  absences <- NULL
  if (!is.null(.overlaps)) {
    # Define overlapping receivers (i.e., those with detection 'absences')
    absences <-
      lapply(.detections, function(r) {
        .overlaps[[r]][[.date]]
      }) |>
      unlist() |>
      unique()
    # Define the set of overlapping receivers that did not record detections
    absences <- absences[!(absences %in% .detections)]
    if (length(absences) == 0L) {
      absences <- NULL
    }
  }
  absences
}
