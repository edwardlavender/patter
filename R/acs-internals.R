#' @title AC* helper: internal checks
#' @description These are internal check functions.
#' @param .b The `.bathy` [`SpatRaster`].
#' @param .k The `.detection_kernels` `list`.
#' @param .o The `.obs` [`data.table`].
#' @param .ov The `detection_overlaps` `list`.
#' @param .p The `present` [`SpatRaster`].
#' @param .t The `timestep`.
#' @param .w,.con The `write_record` `list` and the name of the element that defines the directory in which to save files.
#' @author Edward Lavender
#' @name acs_check
NULL


#' @rdname acs_check
#' @keywords internal

.acs_check_obs <- function(.o) {
  if (inherits(.o, "data.frame") & !inherits(.o, "data.table")) {
    .o <- as.data.table(.o)
  }
  check_inherits(.o, "data.table")
  check_names(.o, c("timestep", "timestamp", "date", "detection_id", "detection", "receiver_id", "buffer_past", "buffer_future"))
  check_inherits(.o$timestep, "integer")
  check_inherits(.o$timestamp, "POSIXct")
  check_inherits(.o$detection_id, "integer")
  check_inherits(.o$detection, "integer")
  check_inherits(.o$receiver_id, "list")
  check_inherits(.o$buffer_past, "numeric")
  check_inherits(.o$buffer_future, "numeric")
  if (!all(
    lubridate::year(.o$timestamp) == lubridate::year(.o$date),
    lubridate::month(.o$timestamp) == lubridate::month(.o$date),
    lubridate::day(.o$timestamp) == lubridate::day(.o$date)
  )) {
    abort("There is a discrepancy between `.obs$timestamp` and `.obs$date`.")
  }
  .o
}

#' @rdname acs_check
#' @keywords internal

.acs_check_bathy <- function(.b) {
  check_inherits(.b, "SpatRaster")
}

#' @rdname acs_check
#' @keywords internal

.acs_check_detection_overlaps <- function(.ov) {
  if (!is.null(.ov)) {
    check_named_list(.ov)
    check_names(.ov, c("list_by_receiver", "list_by_date"))
  }
}

#' @rdname acs_check
#' @keywords internal

.acs_check_detection_kernels <- function(.k, .b) {
  check_named_list(.k)
  check_names(.k, c("receiver_specific_kernels",
                    "receiver_specific_inv_kernels",
                    "array_design",
                    "array_design_by_date",
                    "bkg_surface_by_design",
                    "bkg_inv_surface_by_design"))
  # Check bathy, detection_overlaps & detection_kernels
  if (!terra::compareGeom(.b, compact(.k$receiver_specific_kernels)[[1]], messages = TRUE, stopOnError = FALSE)) {
    abort("The properties of the bathymetry grid and the detection kernel SpatRaster(s) are not equal.")
  }
}

#' @rdname acs_check
#' @keywords internal

.acs_check_write_record <- function(.w, .con = "filename") {
  if (!is.null(.w)) {
    check_named_list(.w)
    check_names(.w, .con)
    if (length(.w[[.con]]) != 1L) {
      abort("`.write_record${.con}` should be a single directory in which to write files.",
            .envir = environment())
    }
    check_dir(.w[[.con]])
    if (length(list.files(.w[[.con]])) != 0L) {
      warn("`.write_record${.con}` ('{.w[[.con]]}') is not an empty directory.",
           .envir = environment())
    }
  }
  .w[[.con]]
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


#' @title AC* helper: Define active, overlapping receivers with absences
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
#' @seealso This function defines the `absences` argument for [`.acs_given_detection()`].
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


#' @title AC* helper: Define the individual's location given detection(s)
#' @description This function defines an (unnormalised) probability surface that represents the possible locations of an individual given one or more detections.
#' @param .detections An `integer` vector of the receiver(s) that recorded detections at a given time step.
#' @param .absences An `integer` vector of the remaining, overlapping receiver(s) that did not record a detection, from [`.acs_absences()`].
#' @param .kernels A `list` from [`acs_setup_detection_kernels`].
#' @param .zero_to_na A logical variable that defines whether or not to classify zeros as `NA`s. This should be `FALSE` for defining `given_data` in [`acs()`], but `TRUE` for defining `next_kernel` in [`acs()`], so that `next_kernel` is correctly buffered.

#' @details In the AC* algorithms, at the moment of detection, the probability kernels that describe the possible locations of an individual given the data depend on both the receivers that record detections and those that did not (eqn S5 in Lavender et al., 2023). This function solves eqn S5. For computational efficiency, the equation is solved in a stepwise manor such that the number of necessary operations is kept to a minimum.
#'
#' # Warning
#' For speed, this function performs no internal checks.
#'
#' @author Edward Lavender
#' @keywords internal

.acs_given_detection <- function(.detections, .absences, .kernels, .zero_to_na = FALSE) {
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
