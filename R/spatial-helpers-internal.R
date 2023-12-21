#' @title Spatial helper: internals
#' @description These functions are internal spatial helpers.
#' @author Edward Lavender
#' @name spatial_helpers

#' @rdname spatial_helpers
#' @keywords internal

# Check if .moorings data are in lon/lat format
.is_lonlat <- function(.data) {
  is_utm <- is_lonlat <- FALSE
  if (all(c("receiver_easting", "receiver_northing") %in% colnames(.data))) {
    is_utm <- TRUE
  }
  if (all(c("receiver_lon", "receiver_lat") %in% colnames(.data))) {
    is_lonlat <- TRUE
  }
  if (is_utm & is_lonlat) {
    warn("UTM coordinates used (both UTM and lon/lat coordinates detected).")
  }
  if (!is_utm & !is_lonlat) {
    warn("Neither UTM coordinates (`.data$receiver_easting`, `.data$receiver_northing`) nor lon/lat coordinates (`.data$receiver_lon`, `.data$receiver_lat`) detected. ")
    return(NULL)
  }
  if (is_utm) {
    is_lonlat <- FALSE
  }
  if (is_lonlat) {
    # Check ranges
    if (min(.data$receiver_lon) < -180 | max(.data$receiver_lon) > 180) {
      abort("Longitudes should be between -180 and 180 degrees.")
    }
    if (min(.data$receiver_lat) < -90 | max(.data$receiver_lat) > 90) {
      abort("Latitudes should be between -90 and 90 degrees.")
    }
  }
  is_lonlat
}

#' @rdname spatial_helpers
#' @keywords internal

# An internal definition of `geosphere::geomean()`
# (without the checks of the `.pointsToMatrix()` function)
geomean <- function(xy, w = NULL) {
  if (nrow(xy) == 1L) {
    xy
  } else {
    # Implement geosphere::geomean()
    # Use internal code to avoid r-spatial warnings if geosphere is loaded
    if (is.null(w)) {
      w <- 1
    }
    else if (length(w) != nrow(xy)) {
      stop("length of weights not correct. It should be: ",
           nrow(xy))
    }
    w <- w/sum(w)
    xyw <- cbind(xy, w)
    xy <- stats::na.omit(xyw)
    xy <- xyw[, 1:2]
    w <- xyw[, 3]
    xy[, 1] <- xy[, 1] + 180
    xy <- xy * pi/180
    Sx <- mean(sin(xy[, 1]) * w)
    Cx <- mean(cos(xy[, 1]) * w)
    x <- atan2(Sx, Cx)
    x <- x %% (2 * pi) - pi
    Sy <- mean(sin(xy[, 2]) * w)
    Cy <- mean(cos(xy[, 2]) * w)
    y <- atan2(Sy, Cy)
    cbind(x, y) * 180/pi
  }
}
