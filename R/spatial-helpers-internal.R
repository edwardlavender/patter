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

#' @title Spatial helper: `spat*` functions
#' @name spat

#' @rdname spat
#' @keywords internal

spatContainsNA <- function(.x) {
  if (is.null(.x)) {
    bool <- FALSE
  } else {
    bool <- terra::global(.x, function(x) any(is.na(x)))[1, 1]
  }
  bool
}

#' @rdname spat
#' @keywords internal

spatCellCoordsDT <- function(.x, .spatcell = .x) {
  check_inherits(.x, c("SpatRaster", "SpatVector"))
  if (inherits(.x, "SpatRaster")) {
    # Extract cell coordinates in .x (using cell IDs from .spatcell)
    dt <-
      .x |>
      terra::as.data.frame(cells = FALSE, xy = TRUE, na.rm = TRUE) |>
      mutate(cell = terra::cellFromXY(.spatcell, cbind(.data$x, .data$y)))
  } else if (inherits(.x, "SpatVector")) {
    # Extract cell coordinates in .x
    names(.spatcell) <- "layer"
    dt <-
      .spatcell |>
      terra::extract(.x, cells = TRUE, ID = FALSE, xy = TRUE) |>
      filter(!is.na(.data$layer))
  }
  dt |>
    mutate(cell = as.integer(.data$cell)) |>
    select(cell_id = "cell", cell_x = "x", cell_y = "y") |>
    as.data.table()
}

#' @rdname spat
#' @keywords internal

spatIntersect <- function(.x, .value = 1, .fun = NULL) {
  check_inherits(.x, "list")
  check_inherits(.x[[1]], c("SpatRaster", "SpatVector"))
  if (!is.null(.value) & !is.null(.fun)) {
    abort("Either `.value` or `.fun` should be supplied.")
  }
  if (length(.x) == 1) {
    return(.x[[1]])
  }
  if (inherits(.x[[1]], "SpatRaster")) {
    return(spatIntersect.SpatRaster(.x = .x, .value = .value, .fun = .fun))
  } else if (inherits(.x[[1]], "SpatVector")) {
    return(spatIntersect.SpatVector(.x = .x))
  }
}

#' @rdname spat
#' @keywords internal

spatIntersect.SpatRaster <- function(.x, .value, .fun) {
  .x <- terra::rast(.x)
  if (!is.null(.value)) {
    terra::app(.x, function(x) all(x == .value))
  } else {
    terra::app(.x, .fun)
  }
}

#' @rdname spat
#' @keywords internal

spatIntersect.SpatVector <- function(.x) {
  int <- .x[[1]]
  for (i in 2:length(.x)) {
    int <- terra::intersect(int, .x[[2]])
  }
  int
}

#' @rdname spat
#' @keywords internal

spatIsEmpty <- function(.x) {
  if (inherits(.x, "SpatRaster")) {
    is.na(terra::global(.x, "min", na.rm = TRUE)[1, 1])
  } else {
    length(.x) == 0
  }
}
