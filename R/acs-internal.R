#' @title AC* helper: AC* container(s)
#' @description These functions define acoustic containers for ACPF algorithm(s).
#'
#' @details
#' At the first time step, the acoustic container is the intersection between the container(s) around the receivers that recorded the first detection (if applicable) and the container(s) around the receivers that recorded the next detection.
#'
#' @return The functions return a [`SpatVector`].
#'
#' @author Edward Lavender
#' @name acs_container

#' @rdname acs_container
#' @keywords internal

# Define an acoustic container around the next receiver(s)
.acs_container <- function(.cinfo) {
  # Define future container as SpatVector (for speed)
  # * Buffer receivers intersect
  lapply(seq_row(.cinfo$coord), function(i) {
    .cinfo$coord[i, , drop = FALSE] |>
      terra::vect() |>
      terra::buffer(width = .cinfo$buffer[i],
                    quadsegs = 1e3L)
  }) |>
    spatIntersect()
}

#' @rdname acs_container
#' @keywords internal

# Define the initial acoustic container, accounting for the current detection & the future
.acs_container_init <- function(.obs, .dlist) {

  #### Define starting surface based on AC algorithm & extensions
  # * If the first time step is a detection, we will account for the current & next buffer
  # * If the first time step is not a detection, we will account for the next buffer only
  # * f(detection | position) is calculated accordingly

  #### Check user inputs
  check_dlist(.dlist = .dlist,
              .dataset = "moorings")
  pos_detections <- which(.obs$detection == 1L)
  if (all(.obs$detection == 0L)) {
    abort("There are no detections in the acoustic time series.")
  }

  #### (A) Define possible locations given past (if applicable)
  start_with_detection <- .obs$detection[1] == 1L
  detection_container <- NULL
  if (start_with_detection) {
    # Identify the receivers that recorded the 'current' detection
    amat <- .obs$acoustics[1][[1]]
    receivers_current <- as.integer(names(amat)[amat == 1L])
    # Define coordinates & detection ranges
    m <-
      .dlist$data$moorings |>
      filter(.data$receiver_id %in% receivers_current) |>
      as.data.table()
    # Define container as SpatVector
    detection_container <- .acs_container(.cinfo = list(coord = cbind(m$receiver_x, m$receiver_y),
                                                        buffer = m$receiver_range))
    if (spatIsEmpty(detection_container)) {
      abort("Detection container(s) at t = 1 do not intersect.")
    }
  }

  #### (B) Define possible locations given future
  # Define the receivers at which the next detection was recorded
  # * If we start with a detection, we simply identify the next detection
  # * Otherwise, the next detection is effectively the first detection (see below)
  future_container <- NULL
  if (length(pos_detections) > 1L) {
    future_container <- .acs_container(.obs$container[[1]])
    if (spatIsEmpty(future_container)) {
      abort("Future container(s) at t = 1 do not intersect.")
    }
  }

  #### (C) Define container accounting for detection & future
  container <- spatIntersect(list(detection_container, future_container))
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

# Compute detection kernels (pk1)
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
      # Set NAs to zero for pf_forward()
      # * In pf_lik_ac(), NAs may still occur for particles outside of detection containers
      k <- terra::classify(k, cbind(NA, 0))
      # Return kernel
      k
    })
  names(receiver_specific_kernels) <- as.character(moorings$receiver_id)
  receiver_specific_kernels
}

#' @rdname acs_setup_detection_kernels-internal
#' @keywords internal

# Compute inverse detection kernels (pk0)
.acs_setup_detection_kernels_pk0 <- function(.pk1) {
  pk0 <- lapply(.pk1, function(k) 1 - k)
  names(pk0) <- names(pk0)
  pk0
}

#' @rdname acs_setup_detection_kernels-internal
#' @keywords internal

# Compute background (inverse) log-likelihood surfaces
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
      # stats::dbinom(0, size = 1L, prop = <kernel for active receiver>, log = TRUE) =
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
      ll <- terra::app(ll, "sum")
      # Mask areas on land
      # * Within the domain of ll, areas on land are set to log(0)
      # * But not that beyond ll, particles may or may not be on land
      # * So in pf_forward(), acs_filter_land() remains necessary
      if (.dlist$pars$spatna) {
        ll <- terra::mask(ll,
                          terra::crop(.dlist$spatial$bathy, ll),
                          maskvalues = NA, updatevalue = log(0))
      }
      ll
    }
  })

}
