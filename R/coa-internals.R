#' @title COA internals
#' @author Edward Lavender
#' @name coa_check

#' @rdname coa_check
#' @keywords internal

.coa_check_acoustics <- function(.acoustics, .split) {
  # Check class
  if (inherits(.acoustics, "data.frame") & !inherits(.acoustics, "data.table")) {
    .acoustics <- as.data.table(.acoustics)
  }
  check_inherits(.acoustics, "data.table")
  # Check names
  # * receiver_easting/receiver_northing or receiver_lon/receiver_lat
  # ... are automatically checked via .coa_check_lonlat()
  check_names(.acoustics, c("timestamp", "receiver_id", .split))
  .acoustics
}

