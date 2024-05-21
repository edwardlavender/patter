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

# Define a 'box' within which 2D movements are always valid
# - This is NULL if the SpatRaster contains NAs
# - Otherwise, it is the extent, shrunk by `.mobility`
# - (2D Movements in this region are always valid)
# * TO DO: generalise to spatPoly() in future
spatMobilityBox <- function(.x, .mobility) {
  if (spatContainsNA(.x)) {
    NULL
  } else {
    # Shrink the boundary box by .mobility
    bb <- terra::ext(.x) - .mobility
    # Update the extent, as in Patter.ext()
    c(min_x = bb[1], max_x = bb[2], min_y = bb[3], max_y = bb[4])
  }
}
