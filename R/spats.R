#' @title Spatial helper: `spat*` functions
#' @description Internal helpers for `terra` [`SpatRaster`] and [`SpatVector`] objects.
#' @author Edward Lavender
#' @name spat

#' @rdname spat
#' @keywords internal

# Intersect SpatVectors in a `list`
spatIntersect <- function(.x) {
  check_inherits(.x, "list")
  if (length(.x) == 1L) {
    return(.x[[1]])
  }
  int <- .x[[1]]
  for (i in 2:length(.x)) {
    int <- terra::intersect(int, .x[[i]])
  }
  int
}

#' @rdname spat
#' @keywords internal

# Check if a SpatRaster contains any NAs (TRUE, FALSE)
spatContainsNA <- function(.x) {
  terra::global(.x, "isNA")[1, 1] > 0
}

#' @rdname spat
#' @keywords internal

# Check if a SpatRaster contains any NAs (TRUE, FALSE)
spatAllNA <- function(.x) {
  # Check if .x only contains NAs
  # * This may fail for large rasters b/c all values have to be loaded in memory
  bool <- tryCatch(terra::global(.x, function(x) all(is.na(x)))[1, 1],
                   error = function(e) e)
  # If we receive an error, set bool = FALSE
  if (inherits(bool, "error")) {
    bool <- FALSE
  }
  bool
}

#' @rdname spat
#' @keywords internal

# Define a 'validity map' within which 2D movements are always valid
spatVmap <- function(.map, .mobility, .plot = FALSE, ...) {
  # Set boundary as NA
  .map <- terra::deepcopy(.map)
  .map[1L, ]                <- NA
  .map[terra::nrow(.map), ] <- NA
  .map[, 1L]                <- NA
  .map[, terra::ncol(.map)] <- NA
  # Define land SpatRaster (land = TRUE, sea = NA)
  land <- is.na(.map)
  land <- terra::classify(land, cbind(FALSE, NA))
  # Compute distance from points in the sea to the nearest land
  distance_from_boundary <- terra::distance(land)
  # Identify the points that are > mobility from land
  # * Any XY movements from these points (1) are valid
  # * Any XY movements from other points (0) require simulation
  vmap <- distance_from_boundary > .mobility
  if (.plot) {
    terra::plot(vmap, ...)
  }
  vmap
}
