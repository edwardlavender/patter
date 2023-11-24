#' @title AC* helper: define AC* container(s)
#' @description This function defines the first AC* container in [`pf_forward_2()`].
#' @param .obs The `.obs` [`data.table`].
#' @param .detection_kernels A [`list`] of detection kernels.
#'
#' @details
#'
#' In [`acs()`], acoustic containers are defined using [`terra::buffer()`].
#'
#' This functions are used in [`pf_forward_2()`].
#' * [.acs_container_1()] defines the acoustic container at the first time step, which is the intersection between the container(s) around the receivers that recorded the first detection (if applicable) and the container(s) around the receivers that recorded the next detection.
#'
#' @return The function returns a [`SpatRaster`] or a [`SpatVector`].
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

  # (A) Define possible locations given past (if applicable)
  start_with_detection <- .obs$detection[pos_detections[1]] == 1L
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
  pos_next <- ifelse(start_with_detection, yes = pos_detections[2], no = pos_detections[1])
  receivers_next <- .obs$receiver_id[[pos_next]]
  future_container <- .acs_container_future(receivers_next,
                                            .detection_kernels = .detection_kernels,
                                            .moorings = .moorings,
                                            .buffer = .obs$buffer_future[1])
  if (spatIsEmpty(future_container)) {
    abort("Future container(s) at t = 1 do not intersect.")
  }

  # (C) Define container accounting for detection & future
  if (start_with_detection) {
    container <- spatIntersect(list(detection_container, future_container),
                               .value = NULL, .fun = function(x) all(x > 0))
  } else {
    container <- future_container
  }
  # For SpatRaster containers(0, 1), convert zero to NA for spatCellCoordsDT()
  if (inherits(container, "SpatRaster")) {
    container <- terra::classify(container, cbind(0, NA))
  }
  if (spatIsEmpty(container)) {
    abort("Detection & future container(s) at t = 1 do not intersect.")
  }
  container
}
