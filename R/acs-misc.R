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
    mutate(receiver_id_next = lead(.data$receiver_id),
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
