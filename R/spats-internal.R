#' @title Spatial helper: `spat*` functions
#' @description Internal `terra` helpers for [`SpatRaster`](s) and [`SpatVector`](s).
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

# Identify non NA cells/coordinates in a SpatRaster or SpatVector
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

# Intersect SpatRaster/Spatvectors in a list
spatIntersect <- function(.x, .value = 1, .fun = NULL) {
  # Check inputs
  check_inherits(.x, "list")
  check_inherits(.x[[1]], c("SpatRaster", "SpatVector"))
  if (!is.null(.value) & !is.null(.fun)) {
    abort("Either `.value` or `.fun` should be supplied.")
  }
  # Permit (but drop) NULL elements in `.x`
  .x <- compact(.x)
  # Handle one-length lists quickly
  if (length(.x) == 1) {
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

# Check if a SpatRaster only contains NAs (TRUE/FALSE)
spatIsEmpty <- function(.x) {
  if (inherits(.x, "SpatRaster")) {
    is.na(terra::global(.x, "min", na.rm = TRUE)[1, 1])
  } else {
    length(.x) == 0
  }
}
