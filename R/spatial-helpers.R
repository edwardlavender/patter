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

#' @title Intersect `SpatRaster`s in a list
#' @description This function identifies the cells on a `SpatRaster`s where all `SpatRaster` layers in a `list` have the same value.
#' @param .x A list.
#' @param .value A number that defines the value.
#' @keywords internal

spatIntersect <- function(.x, .value = 1) {
  check_inherits(.x, "list")
  if (length(.x) == 1) {
    return(.x[[1]])
  }
  .x <- terra::rast(.x)
  terra::app(.x, function(x) all(x == .value))
}

#' @title Normalise a [`SpatRaster`]
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

#' @title Convert a `SpatRaster` to a `spatstat` `im`
#' @source This function is based on `maptools::as.im.RasterLayer`.
#' @keywords internal

as.im.SpatRaster <- function(from) {
  # Check user inputs
  rlang::check_installed("spatstat.geom")
  if (!terra::hasValues(from))
    abort("The SpatRaster is empty.")
  if (terra::is.rotated(from)) {
    abort("The SpatRaster is rotated.")
  }
  # Coerce SpatRaster
  rs <- terra::res(from)
  # Define xmin and ymin (shifted to the cell centre)
  # orig <- sp::bbox(from)[, 1] + 0.5 * rs
  orig <- terra::ext(from)[c(1, 3)] + 0.5 * rs
  dm <- dim(from)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(rs[1], dm[1] - 1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(rs[2], dm[2] - 1))))
  val <- terra::values(from)
  dim(val) <- dm
  val <- spatstat.geom::transmat(val, from = list(x = "-i", y = "j"), to = "spatstat")
  spatstat.geom::im(val, xcol = xx, yrow = yy)
}

#' @title Calculate the centre of mass of weighted coordinates
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
