#' @title Spatial helper: `spat*` functions
#' @description Internal helpers for `terra` [`SpatRaster`] and [`SpatVector`] objects.
#' @author Edward Lavender
#' @name spat

#' @rdname spat
#' @keywords internal

# Check if a SpatRaster contains any NAs (TRUE, FALSE)
spatContainsNA <- function(.x) {
  if (is.null(.x)) {
    bool <- FALSE
  } else {
    bool <- terra::global(.x, "isNA")[1, 1] > 0
  }
  bool
}

#' @rdname spat
#' @keywords internal

# As above, but a subsample
spatSampleDT <- function(.x, .spatcell = .x, .size = 1e6, .method = "random") {
  # Define .x as a SpatRaster using .spatcell
  if (inherits(.x, "SpatVector")) {
    .x <- terra::mask(.spatcell, .x)
  }
  # Sample from the SpatRaster, ignoring NAs
  # * Random sampling is required for full coverage of small regions
  terra::spatSample(x = .x,
                    size = .size, method = .method, replace = FALSE,
                    na.rm = TRUE, cells = FALSE, xy = TRUE,
                    values = FALSE, warn = FALSE) |>
    as.data.table() |>
    mutate(cell = as.integer(terra::cellFromXY(.spatcell, cbind(.data$x, .data$y))),
           x = as.numeric(.data$x),
           y = as.numeric(.data$y)) |>
    select(cell_id = "cell", cell_x = "x", cell_y = "y") |>
    as.data.table()
}

#' @rdname spat
#' @keywords internal

# Intersect SpatRaster/Spatvectors in a list
spatIntersect <- function(.x, .value = 1, .fun = NULL) {
  # Check inputs
  check_inherits(.x, "list")
  check_inherits(.x[[1]], c("SpatRaster", "SpatVector"))
  if (!is.null(.value) & !is.null(.fun)) {
    abort("Either `.value` or `.fun` should be supplied.")
  }
  # Permit (but drop) NULL elements in `.x`
  .x <- list_compact(.x)
  # Handle one-length lists quickly
  if (length(.x) == 1L) {
    return(.x[[1]])
  }
  # Intersect Spat* objects
  if (inherits(.x[[1]], "SpatRaster")) {
    return(spatIntersect.SpatRaster(.x = .x, .value = .value, .fun = .fun))
  } else if (inherits(.x[[1]], "SpatVector")) {
    return(spatIntersect.SpatVector(.x = .x))
  }
}

#' @rdname spat
#' @keywords internal

# Intersect SpatRasters in a list
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

# Intersect SpatVectors in a list
spatIntersect.SpatVector <- function(.x) {
  int <- .x[[1]]
  for (i in 2:length(.x)) {
    int <- terra::intersect(int, .x[[i]])
  }
  int
}

#' @rdname spat
#' @keywords internal

# Check if a SpatRaster only contains NAs (TRUE/FALSE)
spatIsEmpty <- function(.x) {
  if (inherits(.x, "SpatRaster")) {
    is.na(terra::global(.x, "min", na.rm = TRUE)[1, 1])
  } else {
    length(.x) == 0
  }
}

#' @rdname spat
#' @keywords internal

# Normalise a SpatRaster, ignoring NAs
spatNormalise <- function(x) {
  x / as.numeric(terra::global(x, "sum", na.rm = TRUE))
}

#' @rdname spat
#' @keywords internal

# Extend and mask a SpatRaster
spatExtendMask <- function(.x, .y, .fill = 0, ...) {
  out <- terra::extend(x = .x, y = .y, fill = .fill, ...)
  terra::mask(out, .y)
}
