#' @title PF: proposal functions
#' @description TO DO
#'
#' @name pf_propose

#' @rdname pf_propose
#' @export

pf_rpropose_origin <- function(.obs, .origin, .grid = FALSE,
                                .detection_kernels, .moorings,
                                .bathy) {

  # Define .origin, if necessary
  if (is.null(.origin)) {

    # (1) Define 'quadrature points' within acoustic containers
    if (!is.null(.moorings)) {
      # Define container for possible locations
      container <- .acs_container_1(.obs,
                                    .detection_kernels = .detection_kernels,
                                    .moorings = .moorings)
      # Define cell coordinates within container
      .origin <- spatCellCoordsDT(container, .spatcell = .bathy)

      # (2) Define quadrature points across all of `.bathy`
    } else {
      .origin <- spatCellCoordsDT(.x = .bathy)
    }
  }

  # Tidy .origin data.table
  .origin |>
    mutate(timestep = 1L,
           cell_past = NA_integer_,
           cell_now = as.integer(.data$cell_id)) |>
    select("timestep", "cell_past", "cell_now", x_now = "cell_x", y_now = "cell_y") |>
    as.data.table()
}

#' @rdname pf_propose
#' @export

pf_rpropose_kick <- function(.particles,
                             .obs = NULL,
                             .t = NULL,
                             .bathy = NULL,
                             .sim_length = rlen,
                             .sim_angle = rangrw,
                             .lonlat = FALSE, ...) {
  # Simulate step length & turning angle
  n    <- nrow(.particles)
  rlen <- .sim_length(n, ...)
  rang <- .sim_angle(n, ...)
  # Kick each particle from previous location into new proposal locations
  x_past <- y_past <- NULL
  xy_next <- cstep(.xy_now = as.matrix(.particles[, list(x_past, y_past)]),
                   .lonlat = .lonlat,
                   .length = rlen,
                   .angle = rang)
  # Update data.table
  cell_now <- NULL
  .particles[, cell_now := terra::cellFromXY(.bathy, xy_next)]
  xy_next <- terra::xyFromCell(.bathy, .particles$cell_now)
  x_now <- y_now <- NULL
  .particles[, x_now := xy_next[, 1]]
  .particles[, y_now := xy_next[, 2]]
  .particles
}

#' @rdname pf_propose
#' @export

pf_rpropose_reachable <- function(.particles, .obs, .t, .bathy, ...) {

  # Isolate unique particles
  .particles <-
    .particles |>
    # select("cell_past", "x_past", "y_past") |>
    distinct(.data$cell_past, .keep_all = TRUE)

  # Define reachable zone(s) (given .mobility)
  zone <-
    .particles |>
    select("x_past", "y_past") |>
    as.matrix() |>
    terra::vect(type = "point") |>
    terra::buffer(width = .obs$mobility[.t], quadsegs = 1e3)
  terra::crs(zone) <- terra::crs(.bathy)

  # Identify permitted location choices given .mobility
  # * Use exact_extract() for much faster speeds
  choices <-
    exactextractr::exact_extract(.bathy,
                                 sf::st_as_sf(zone),
                                 include_cell = TRUE,
                                 include_xy = TRUE)
  for (i in seq_len(length(choices))) {
    choices[[i]]$cell_past <- .particles$cell_past[i]
  }
  choices <-
    choices |>
    rbindlist() |>
    filter(!is.na(.data$value)) |>
    select("cell_past", cell_now = "cell",
           x_now = "x", y_now = "y",
           bathy = "value") |>
    as.data.table()

  # Return 'proposal' (possible) cells (given .mobility only)
  # * In downstream functions, we filter & weight these
  merge(.particles, choices, by = "cell_past")
}
