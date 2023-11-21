#' @title Spatial helper: identify lon/lat moorings
#' @description This function identifies moorings data in lon/lat format.
#' @param .data A [`data.table`] with `receiver_easting` and `receiver_northing` or `receiver_lon` and `receiver_lat` columns (i.e., [`dat_moorings`]).
#' @return A logical variable that defines whether or not to use longitude/latitude coordinates.
#' @author Edward Lavender
#' @keywords internal

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
    abort("Neither UTM coordinates (`.data$receiver_easting`, `.data$receiver_northing`) nor lon/lat coordinates (`.data$receiver_lon`, `.data$receiver_lat`) detected. ")
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

#' @title Spatial helper: create a template [`SpatRaster`]
#' @description This function creates a template [`SpatRaster`].
#' @param .xmin,.xmax,.ymin,.ymax Numbers that define the [`SpatRaster`] limits.
#' @param .res A `numeric` vector that defines [`SpatRaster`] resolution.
#' @param .crs A `character` that defines the Coordinate Reference System.
#' @param .value The value of the [`SpatRaster`] cells.
#' @param .units A `character` that defines the units.
#' @param ... Additional arguments passed to [`SpatRaster`].
#' @return The function returns a [`SpatRaster`].
#' @examples
#' r <- rast_template()
#' terra::plot(r)
#' r <- rast_template(.value = 1)
#' terra::plot(r)
#' @author Edward Lavender
#' @export

rast_template <- function(.xmin = 0, .xmax = 1000,
                          .ymin = 0, .ymax = 1000,
                          .res = c(10, 10),
                          .crs = "+proj=utm +zone=1 +datum=WGS84",
                          .value = 0,
                          .units = "m", ...) {
  r <- terra::rast(xmin = .xmin, xmax = .xmax,
                   ymin = .ymin, ymax = .ymax,
                   res = .res,
                   crs = .crs, ...)
  r <- terra::setValues(r, .value)
  terra::units(r) <- .units
  r
}

#' @title Spatial helper: intersect `SpatRaster`s in a list
#' @description This function identifies the cells on a `SpatRaster`s where all `SpatRaster` layers in a `list` have the same value.
#' @param .x A list.
#' @param .value A number that defines the value.
#' @param .fun A function.
#' @keywords internal

spatIntersect <- function(.x, .value = 1, .fun = NULL) {
  check_inherits(.x, "list")
  if (!is.null(.value) & !is.null(.fun)) {
    abort("Either `.value` or `.fun` should be supplied.")
  }
  if (length(.x) == 1) {
    return(.x[[1]])
  }
  .x <- terra::rast(.x)
  if (!is.null(.value)) {
    terra::app(.x, function(x) all(x == .value))
  } else {
    terra::app(.x, .fun)
  }
}

#' @title Spatial helper: normalise a [`SpatRaster`]
#' @description This function normalises a [`SpatRaster`].
#' @param x A [`SpatRaster`].
#' @details
#' # Warning
#' * For speed in iterative applications (e.g., [`acs()`]), this function does not implement any internal checks.
#' * `NA`s are ignored.
#' @author Edward Lavender
#' @export

normalise <- function(x) {
  x / as.numeric(terra::global(x, "sum", na.rm = TRUE))
}

#' @title Spatial helper: create circular angle in degrees
#' @description This function is a wrapper for `circular::circular(x, units = "degrees")`.
#' @param .x A numeric vector of angles (in degrees).
#' @return The function returns a [`circular::circular`] object.
#' @examples
#' degrees(10)
#' degrees(c(10, 20))
#' @author Edward Lavender
#' @export

degrees <- function(.x) {
  circular::circular(x = .x, units = "degrees")
}

#' @title Spatial helper: calculate the centre of mass of weighted coordinates
#' @description This is a wrapper for `geosphere::geomean()` that handles one-row matrices.
#' @param xy,w Arguments passed to `geosphere::geomean()`.
#' @details This function uses the internal code of `geosphere::geomean()` without the checks of the `.pointsToMatrix()` function.
#' @author Edward Lavender
#' @keywords internal

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
