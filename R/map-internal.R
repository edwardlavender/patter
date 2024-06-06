#' @title Map: location coordinates (`id`, `x`, `y`, `mark`)
#' @description [`.map_coord()`] defines coordinates and weights (marks) for utilisation distribution (UD) estimation.
#'
#' @param .map The [`SpatRaster`] used to represent the UD. This can be `NULL` if `.coord` is provided and `.discretise = FALSE`.
#' @param .coord A [`matrix`], [`data.frame`] or [`data.table`] with x and y coordinates, in columns named `x` and `y` or `cell_x` and `cell_y` (see Details). (`.coord` is coerced to a [`data.table`].) Additional columns (`timestep` and `mark`), as supported by [`.map_mark()`] are permitted. Other columns are ignored.
#' @param .discretise A `logical` variable that defines whether or not to discretise coordinates (i.e., redefine coordinates on `.map`). This is necessarily `TRUE` in the wrapper function [`map_pou()`] but optional for [`map_dens()`].
#'
#' @details
#' [`.map_coord()`] function defines a [`data.table`] that includes coordinate IDs (`id`), coordinates (`x` and `y`) and associated weights (`mark`) for UD estimation. The function wraps [`.map_coord.SpatRaster()`] and [`.map_coord.dt()`].
#'
#' * If `.coord` is `NULL`, [`.map_coord.SpatRaster()`] is used. This function extracts a [`data.table`] of coordinates from `.map` (in non `NA` regions) via `terra::as.data.frame(..., na.rm = TRUE)`. The values on `.map` are taken as weights and must sum to one.
#'
#' * If `.coord` is supplied, [`.map_coord.dt()`] is used. `x` and `y` and/or `cell_x` and `cell_y` columns are expected. If `.discretise = TRUE`, `cell_x` and `cell_y` are used preferentially. Irrespective of whether or not `x` and `y` or `cell_x` and `cell_y` are specified, coordinates are redefined on `.map` and coordinate IDs are defined as grid cell IDs on `.map`. If `.discretise = FALSE`, `x` and `y` coordinates are used preferentially. Coordinates remain unchanged and IDs are defined from the set of unique coordinate pairs. For each ID, weights are calculated by [`.map_mark()`].
#'
#' @return The function returns a [`data.table`] with four columns:
#' * `id`---a vector of coordinate IDs (if `.discretise = TRUE`, `id` represents grid cells on `.map`);
#' * `x`,`y`---`numeric` vectors that define coordinates (if `.discretise = TRUE`, `x` and `y` represent cell coordinates on `.map`);
#' * `mark`---a `numeric` vector of weights, normalised to sum to one;
#'
#' @seealso [`.map_coord()`] is used by [`map_pou()`] and [`map_dens()`].
#'
#' @author Edward Lavender
#' @name map_coord

#' @rdname map_coord
#' @keywords internal

.map_coord <- function(.map, .coord, .discretise) {
  if (is.null(.coord)) {
    xym <- .map_coord.SpatRaster(.map = .map)
  } else {
    xym <- .map_coord.dt(.map = .map, .coord = .coord, .discretise = .discretise)
  }
  xym
}

#' @rdname map_coord
#' @keywords internal

# Define x, y, mark data.table from SpatRaster
.map_coord.SpatRaster <- function(.map) {
  # Define coordinates
  .coord <- terra::as.data.frame(.map, xy = TRUE, na.rm = TRUE)
  colnames(.coord) <- c("x", "y", "mark")
  .coord <- .coord[which(!is.na(.coord$mark) & .coord$mark != 0), ]
  if (!isTRUE(all.equal(sum(.coord$mark), 1))) {
    abort("Weights on `.map` should sum to one.")
  }
  .coord |>
    mutate(id = terra::cellFromXY(.map, cbind(.data$x, .data$y))) |>
    select("id", "x", "y", "mark") |>
    as.data.table()
}

#' @rdname map_coord
#' @keywords internal

# Define x, y, mark data.table from coordinates data.table
.map_coord.dt <- function(.map, .coord, .discretise) {

  #### Copy .coord (and coerce to a data.table)
  .coord <- as.data.table(.coord)

  #### Define x and y columns
  # Identify whether or not the .coord contains x, y and/or cell_x and cell_y columns
  contains_xy      <- all(c("x", "y") %in% colnames(.coord))
  contains_cell_xy <- all(c("cell_x", "cell_y") %in% colnames(.coord))
  if (!contains_xy & !contains_cell_xy) {
    abort("`.coord` should contain `x` and `y` (or `cell_x` and `cell_y`) coordinates.")
  }
  # Define coordinates
  # * If .discretise = TRUE:
  #   - We use cell_x and cell_y; otherwise we use x and y
  #   - In both cases we move coordinates onto .map, in case this is different from the original grid
  #   - Coordinate IDs are defined as grid cells
  # * If .discretise = FALSE:
  #   - We use x and y by default; and only if unavailable we use cell_x and cell_y
  #   - Coordinate IDs are defined as unique coordinates (and not moved onto .map)
  id <- x <- y <- cell_x <- cell_y <- NULL
  if (.discretise) {
    if (contains_cell_xy) {
      .coord[, id := terra::cellFromXY(.map, cbind(cell_x, cell_y))]
      .coord[, x := cell_x]
      .coord[, y := cell_y]
    } else {
      .coord[, id := terra::cellFromXY(.map, cbind(x, y))]
    }
    .coord[, x := terra::xFromCell(.map, id)]
    .coord[, y := terra::yFromCell(.map, id)]
  } else {
    if (!contains_xy) {
      .coord[, x := cell_x]
      .coord[, y := cell_y]
      .coord[, id := terra::cellFromXY(.map, cbind(x, y))]
    } else {
      .coord[, id := .GRP, by = list(x, y)]
    }
  }

  #### Define coord marks, as required
  .coord |>
    .map_mark() |>
    select("id", "x", "y", "mark") |>
    as.data.table()

}

#' @title Map: location marks (weights)
#' @description This function calculates coordinate weights for a dataset or time series.
#' @param .coord A [`data.table`] that defines coordinates, with the following columns:
#' * (required) `id`---a vector that distinguishes coordinate IDs (such as grid cells or unique coordinate pairs);
#' * (optional) `timestep`---a vector that distinguishes time steps, if coordinates have been sampled through time;
#' * (optional) `mark`---a `numeric` vector of coordinate weights;
#'
#' @details
#' Equal weights are assumed unless specified in the `mark` column. Default or supplied weights are normalised to sum to one at each time step (if necessary). The total weight of each coordinate ID within time steps is calculated and then these weights are aggregated by coordinate ID across the whole time series (if applicable) and renormalised to sum to one.
#'
#' @return The function returns a [`data.table`]. Columns match `.coord`, excluding `timestep` if specified (since weights are summarised within and across time steps). There is one row for each unique coordinate ID and a `mark` column that defines the associated weights.
#'
#' @seealso This function is used by [`.map_coord()`].
#' @author Edward Lavender
#' @keywords internal

.map_mark <- function(.coord) {

  # Check user inputs
  check_inherits(.coord, "data.table")
  check_names(.coord, "id")

  # Define required columns
  if (is.null(.coord$timestep)) {
    timestep <- NULL
    .coord[, timestep := 1L]
  }

  # Define position weights
  # * If un-supplied, equal weights are assumed, summing to 1 at each time step
  # * Otherwise, existing weights are used, and forced to sum to 1 at each time step
  if (is.null(.coord$mark)) {
    .coord <-
      .coord |>
      group_by(.data$timestep) |>
      mutate(mark = rep(1/n(), n())) |>
      ungroup() |>
      as.data.table()
  } else {
    .coord <-
      .coord |>
      group_by(.data$timestep)  |>
      mutate(mark = .data$mark / sum(.data$mark)) |>
      ungroup() |>
      as.data.table()
  }

  # Calculate the total weight of each location within time steps
  .coord <-
    .coord |>
    # Drop NA or zero weights (required for `map_dens()`)
    filter(!is.na(.data$mark)) |>
    filter(.data$mark != 0) |>
    # Implement aggregation
    group_by(.data$timestep, .data$id) |>
    mutate(mark = sum(.data$mark)) |>
    slice(1L) |>
    ungroup() |>
    as.data.table()

  # Calculate the total weight of each location across the whole time series
  .coord |>
    group_by(.data$id) |>
    mutate(mark = sum(.data$mark)) |>
    slice(1L) |>
    ungroup() |>
    # Divide by number of time steps
    mutate(mark = .data$mark / sum(.data$mark)) |>
    select(!"timestep") |>
    as.data.table()
}

#' @title Spatial helper: raster volume
#' @description The `spatialEco::raster.vol()` function.
#' @source This function is copied, almost exactly, from the [`spatialEco`](https://github.com/jeffreyevans/spatialEco) package (v.2.0-2). It is defined separately in [`patter`] to reduce dependencies.
#' @keywords internal

raster.vol <- function(x,
                       p = 0.75,
                       sample = FALSE,
                       spct = 0.05,
                       type = c("random", "regular")) {
  rlang::check_installed("sf")
  if (!inherits(x, "SpatRaster"))
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if (sample == FALSE) {
    den <- x[][, 1]
    z <- sort(den[!is.na(den)], decreasing = TRUE)
    y <- cumsum(as.numeric(z))
    i <- sum(y <= p * y[length(y)])
    vol <- x
    vol[] <- as.integer(den >= z[i])
    return(vol)
  }
  else {
    ss = round(terra::ncell(x) * spct, digits = 0)
    den <- sf::st_as_sf(terra::spatSample(x, size = ss, method = type[1],
                                          na.rm = TRUE, as.points = TRUE, values = TRUE))
    names(den)[1] <- "den"
    den$idx <- 1:nrow(den)
    den$cls = 0
    sum.p <- sum(den$den, na.rm = TRUE) * p
    den <- den[order(-den$den), ]
    i = 0
    j = 0
    while (i <= sum.p) {
      j = j + 1
      if (!is.na(den$den[j])) {
        i = i + den$den[j]
        den$cls[j] <- 1
      }
      else {
        den$cls[j] <- NA
      }
    }
    den <- den[order(den$idx), ]
    return(den[den$cls == 1, ])
  }
}
