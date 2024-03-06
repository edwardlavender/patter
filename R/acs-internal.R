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

#' @title AC* helper: AC* kernels
#' @description These functions support [`acs_setup_detection_kernels()`].
#' @name acs_setup_detection_kernels-internal

#' @rdname acs_setup_detection_kernels-internal
#' @keywords internal

.acs_setup_detection_kernels_pk1 <- function(.dlist, .pdetkernel, ...) {

  #### Check user inputs
  check_dlist(.dlist = .dlist,
              .dataset = "moorings",
              .spatial = "bathy",
              .par = "spatna")
  moorings <- .dlist$data$moorings
  check_names(.dlist$data$moorings, req = c("receiver_x", "receiver_y"))
  bathy    <- .dlist$spatial$bathy

  #### Calculate detection Pr around each receiver
  # (used to up-weight areas around a receiver with a detection)
  receiver_specific_kernels <-
    pbapply::pblapply(split(moorings, moorings$receiver_id), function(m) {
      # Define receiver coordinates
      mxy <- cbind(m$receiver_x, m$receiver_y)
      # Define detection container around receiver
      container <-
        mxy |>
        terra::vect(crs = terra::crs(bathy)) |>
        terra::buffer(width = m$receiver_range, quadsegs = 1e3L)
      # Crop bathy for improved speed
      b <- terra::crop(bathy, container, snap = "out")
      # Define kernel using user-provided function
      k <- .pdetkernel(.mooring = m, .bathy = b, .mask = .dlist$pars$spatna, ...)
      # Calculate Pr at receiver and check it is not NA or 0
      pr_at_receiver <- terra::extract(k, mxy)[1, 1]
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
  names(receiver_specific_kernels) <- as.character(moorings$receiver_id)
  receiver_specific_kernels
}

#' @rdname acs_setup_detection_kernels-internal
#' @keywords internal

.acs_setup_detection_kernels_pk0 <- function(.pk1) {
  pk0 <- lapply(.pk1, function(k) 1 - k)
  names(pk0) <- names(pk0)
  pk0
}

#' @rdname acs_setup_detection_kernels-internal
#' @keywords internal

.acs_setup_detection_kernels_ll <- function(.dlist, .pk0) {

  # Define array designs
  rs_mat <- make_matrix_receivers(.dlist = .dlist,
                                  .delta_t = "days",
                                  .as_POSIXct = NULL)
  rs_mat_cp <- unique(rs_mat)

  # Iterate over array designs & get log-likelihood surfaces for non detection
  pbapply::pblapply(1:nrow(rs_mat_cp), function(icp) {

    #### Identify active receivers for array
    # icp <- 1
    cp <- rs_mat_cp[icp, , drop = FALSE]
    rs_active <- colnames(cp)[which(cp == 1)]

    #### Log relevant kernels for Pr(non-detection):
    log_pk0_by_rs_active <-
      lapply(rs_active, function(ra) log(.pk0[[ra]]))

    #### Evaluate log-likelihood of non detection at all active receivers
    if (length(rs_active) == 1) {
      # dbinom(0, size = 1L, prop = <kernel for active receiver>, log = TRUE) =
      return(log_pk0_by_rs_active[[1]])
    } else {
      # Get the extent of the list of (inverse) detection kernels
      # (hopefully this is considerably smaller than the total extent)
      ext <- terra::ext(do.call(terra::sprc, log_pk0_by_rs_active))
      # Align SpatRasters
      log_pk0_by_rs_active <-
        lapply(log_pk0_by_rs_active, function(r) {
          terra::extend(r, ext, fill = log(1))
        })
      # Calculate the background surface
      ll <- do.call(c, log_pk0_by_rs_active)
      return(terra::app(ll, "sum"))
    }
  })

}
