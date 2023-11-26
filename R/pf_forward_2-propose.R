#' @title PF: proposal functions
#' @description TO DO
#'
#' @name pf_propose

#' @rdname pf_propose
#' @export

.pf_rpropose_origin <- function(.obs, .origin, .grid = FALSE,
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
  # Kick each particle into new proposal locations
  x_now <- y_now <- NULL
  xy_next <- cstep(.xy_now = as.matrix(.particles[, list(x_now, y_now)]),
                   .lonlat = .lonlat,
                   .length = rlen,
                   .angle = rang)
  # Update data.table
  x_next <- y_next <- NULL
  .particles[, x_next := xy_next[, 1]]
  .particles[, y_next := xy_next[, 2]]
  .particles
}

#' @rdname pf_propose
#' @export

pf_rpropose_reachable <- function(.particles, .obs, .t, .bathy, ...) {

  # Isolate unique particles
  .particles <-
    .particles |>
    # select("cell_now", "x_now", "y_now") |>
    distinct(.data$cell_now, .keep_all = TRUE)

  # Define reachable zone(s) (given .mobility)
  zone <-
    .particles |>
    select("x_now", "y_now") |>
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
    choices[[i]]$id <- .particles$cell_now[i]
  }
  if (length(zone) > 1L) {
    choices <- rbindlist(choices)
  }
  choices <-
    choices |>
    select(cell_now = "id",
           x_next = "x", y_next = "y",
           bathy = "value") |>
    as.data.table()

  # Return 'proposal' (possible) cells (given .mobility only)
  # * In downstream functions, we filter & weight these
  merge(.particles, choices, by = "cell_now")
}
