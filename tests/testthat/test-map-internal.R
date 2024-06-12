test_that(".map_*() functions work", {

  # Define helper to compute XYM data.table
  xym <- function(.coord) {
    .coord <- copy(.coord)
    # (optional) Assign equal weights
    if (!rlang::has_name(.coord, "mark")) {
      .coord[, mark := 1]
    }
    # Compute weights
    .coord |>
      # For each ID/timestep, compute total weight
      group_by(timestep, id) |>
      mutate(mark = sum(mark)) |>
      slice(1L) |>
      ungroup() |>
      # Normalise weights across all particles within time steps
      group_by(timestep) |>
      mutate(mark = mark / sum(mark)) |>
      ungroup() |>
      # Aggregate weights across time steps
      group_by(id) |>
      mutate(mark = sum(mark)) |>
      slice(1L) |>
      ungroup() |>
      # Renormalise
      mutate(mark = mark / sum(mark)) |>
      select(!"timestep") |>
      as.data.table()
  }

  # Define map
  map <- dat_gebco()
  map <- map / terra::global(map, "sum", na.rm = TRUE)[1, 1]

  # Test `.map_coord.SpatRaster()`
  coord  <- .map_coord(.map = map, .coord = NULL)
  output <- .map_coord.SpatRaster(.map = map)
  expect_equal(coord, output)
  expected <-
    map |>
    as.data.frame(xy = TRUE, na.rm = TRUE) |>
    mutate(id = terra::cellFromXY(map, cbind(x, y))) |>
    select("id", "x", "y", "mark" = map_value) |>
    as.data.table()
  expect_equal(output, expected)
  output <- .map_coord.SpatRaster(.map = dat_gebco()) |>
    expect_error("Weights on `.map` should sum to one.",
                 fixed = TRUE)

  # Test` .map_coord.dt()` with `discretise = FALSE`
  xy <- data.table(timestep = 1L, x = c(1, 2, 1), y = c(3, 4, 3))
  output   <- .map_coord(.map = map, .coord = xy, .discretise = FALSE)
  expected <- data.table(id = 1:2,
                         x = 1:2,
                         y = 3:4,
                         mark = c(2/3, 1/3))
  expect_equal(output, expected)

  # Test `.map_coord.dt()` with `discretise = TRUE`
  r        <- terra::rast()
  xy       <- data.table(timestep = 1L, x = c(1, 2, 1), y = c(3, 4, 3))
  output   <- .map_coord(.map = r, .coord = xy, .discretise = TRUE)
  id       <- terra::cellFromXY(r, cbind(xy$x[c(1, 2)], xy$y[c(1, 2)]))
  expected <- data.table(id = id,
                         x = terra::xFromCell(r, id),
                         y = terra::yFromCell(r, id),
                         mark = c(2/3, 1/3))
  expect_equal(output, expected)

  # Test `.map_mark()` with a simple `data.table`
  xy       <- data.table(timestep = 1L, id = c(1, 2, 1), x = c(1, 2, 1), y = c(3, 4, 3))
  output   <- .map_mark(copy(xy))
  expected <- data.table(id = c(1, 2),
                         x = c(1, 2),
                         y = c(3, 4),
                         mark = c(2/3, 1/3))
  expect_equal(output, expected)

  # Test `.map_mark()` with a `timestep` column:
  # * Computes total weight within time steps (normalised)
  # * Aggregates weights across time steps
  # * Renormalises
  xy          <- rbind(xy, xy)
  xy$timestep <- c(1, 1, 1, 2, 2, 2)
  output      <- .map_mark(copy(xy))
  expected    <- xym(xy)
  expect_equal(output, expected)

  # Test `.map_mark()` with a `mark` column:
  xy$mark <- c(1, 2, 3, 4, 5, 6)
  output <- .map_mark(copy(xy))
  expected <- xym(xy)
  expect_equal(output, expected)

})

