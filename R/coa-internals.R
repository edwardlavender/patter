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
  # ... are automatically checked via acoustics_is_lonlat()
  check_names(.acoustics, c("timestamp", "receiver_id", .split))
  .acoustics
}

#' @rdname coa_check
#' @keywords internal

.coa_check_lonlat <- function(.acoustics) {
  is_utm <- is_lonlat <- FALSE
  if (all(c("receiver_easting", "receiver_northing") %in% colnames(.acoustics))) {
    is_utm <- TRUE
  }
  if (all(c("receiver_lon", "receiver_lat") %in% colnames(.acoustics))) {
    is_lonlat <- TRUE
  }
  if (is_utm & is_lonlat) {
    warn("UTM coordinates used (both UTM and lon/lat coordinates detected).")
  }
  if (!is_utm & !is_lonlat) {
    abort("Neither UTM coordinates (`.acoustics$receiver_easting`, `.acoustics$receiver_northing`) nor lon/lat coordinates (`.acoustics$receiver_lon`, `.acoustics$receiver_lat`) detected. ")
  }
  if (is_utm) {
    is_lonlat <- FALSE
  }
  is_lonlat
}
