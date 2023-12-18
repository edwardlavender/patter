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

# Define x, y, mark data.table from SpatRaster
spatMarks <- function(.x) {
  # Define coordinates
  .coord <- terra::as.data.frame(.x, xy = TRUE, na.rm = TRUE)
  colnames(.coord) <- c("x", "y", "mark")
  .coord <- .coord[which(!is.na(.coord$mark) & .coord$mark != 0), ]
  if (!isTRUE(all.equal(sum(.coord$marks), 1))) {
    abort("Weights on `.x` should sum to one.")
  }
  .coord |>
    mutate(cell_id = terra::cellFromXY(.x, cbind(.data$x, .data$y))) |>
    select("cell_id", "x", "y", "mark") |>
    as.data.table()
}

#' @rdname spat
#' @keywords internal

# Define x, y, mark data.table from coordinates data.table
spatMarksFromCoord <- function(.x, .coord, .simplify = FALSE) {

  # Coerce .coord to a data.table
  if (inherits(.coord, "matrix") |
      inherits(.coord, "data.frame") & !inherits(.coord, "data.table")) {
    .coord <- as.data.table(.coord)
  }
  check_inherits(.coord, "data.table")

  # Define x and y columns
  contains_xy      <- all(c("x", "y") %in% colnames(.coord))
  contains_cell_xy <- all(c("cell_x", "cell_y") %in% colnames(.coord))
  if (contains_xy) {
    if (contains_cell_xy) {
      msg("`.coord` contains both (`x`, `y`) and (`cell_x`, `cell_y`) coordinates: (`x`, `y`) coordinates used.")
      cell_x <- cell_y <- NULL
      .coord[, cell_x := NULL]
      .coord[, cell_y := NULL]
    }
  } else {
    if (contains_cell_xy) {
      .coord <-
        .coord |>
        rename(x = "cell_x", y = "cell_y") |>
        as.data.table()
    } else {
      abort("`.coord` should contain `x` and `y` (or `cell_x` and `cell_y`) coordinates.")
    }
  }

  # Define cell IDs (if un-supplied) for .map_mark()
  if (.simplify) {
    id <- cell_id <- NULL
    if (is.null(.coord$cell_id)) {
      .coord[, id := terra::cellFromXY(.x, cbind(.coord$x, .coord$y))]
    } else {
      .coord[, id := cell_id]
    }
  } else {
    x <- y <- NULL
    .coord[, id := paste(x, y)]
  }

  # Define coord marks, as required
  .coord <-
    .coord |>
    .map_mark() |>
    select("cell_id", "x", "y", "mark") |>
    as.data.table()
}
