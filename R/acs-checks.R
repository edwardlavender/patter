#' @title AC* checks
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
    check_inherits(.detection_overlaps, "list")
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
