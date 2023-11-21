#' @title Simulate movement paths
#' @description Internal functions that support the simulation of movement paths.
#' * [`.sim_path_flux()`] simulates the movement path(s) from flux parameters that are generated dynamically at each time step. This is supported by the following helpers:
#'    * [`.flux_template()`] defines a list of [`data.table`] objects in which the simulated 'flux' parameters (e.g., step lengths and turning angles) for each step are stored;
#'    * [`.cstep_using_flux()`] and [`cstep()`] are functions which, given current location(s), calculate new locations, based on step lengths and turning angles;
#'    * [`.cstep_iter()`] is an internal wrapper for [`cstep()`] that validates proposal steps into new locations;
#'    * [`.sim_path_pivot()`] and [`.flux_pivot()`] reorientate simulated paths/flux values;
#'
#' @details
#'
#' These functions facilitate the simulation of animal movement paths.
#' * [`.sim_path_flux()`] implements the simulation.
#'    * A [`SpatRaster`] (`.bathy`) defines the area of the simulation.
#'    * A starting location (`.origin`) on `.bathy` can be specified or  sampled at random from `.bathy`.
#'    * `.n_path` movement path(s) from this point are simulated using time-specific ('flux') parameters (such as step lengths and turning angles).
#'
#' * To implement this approach, a 'flux template' must be provided (to the `.flux_vals` argument), which is a list of [`data.table`]s that will hold the 'flux' parameters for each time step and can be updated by reference.
#'    * The default [`.flux_template()`] function generates a list with place holders for simulated step lengths and turning angles.
#' * The `.flux` argument is a function that is used to simulate the new values of any flux parameters at each time step and update (by reference) the flux template (i.e., `.flux_vals`). This must accept `.fv`, `.row` and `.col` arguments, as implemented in [`.cstep_iter()`].
#' * `.move` is a function that defines new proposal locations based on the simulated flux values.
#'    * For example, [`.cstep_using_flux()`], which wraps [`cstep()`], defines proposal locations based on simulated step lengths and turning angles.
#' * Internally, `.move` is wrapped within [`.cstep_iter()`] and implemented iteratively to ensure that simulated location(s) at each time step are valid (in non NA cells on `.bathy`).
#'
#' @author Edward Lavender
#' @name sim_path_flux
NULL

#' @rdname sim_path_flux
#' @keywords internal

.sim_path_flux <- function(.bathy = spatTemplate(), .lonlat = FALSE,
                          .origin = NULL,
                          .n_step = 10L,
                          .flux, .flux_vals = .flux_template(.n_step, .n_path),
                          .move = .cstep_using_flux,
                          .n_path = 1L,
                          .plot = TRUE, .one_page = FALSE) {

  ### Define a matrix to store outputs
  # * Each row is a path
  # * Each pair of columns ([1, 2], [3, 4] etc.) is the xy coordinates of the path
  mat <- matrix(NA, nrow = .n_path, ncol = .n_step * 2)
  rownames(mat) <- paste0("path_", seq_len(.n_path))
  colnames(mat) <- paste0("step_", rep(seq_len(.n_step), each = 2), c(":x", ":y"))

  #### Simulate starting values
  # Define starting location
  if (is.null(.origin)) {
    .origin <- terra::spatSample(.bathy, size = 1,
                                 method = "random",
                                 xy = TRUE, values = FALSE,
                                 na.rm = TRUE)
  }
  mat[, 1] <- .origin[1]
  mat[, 2] <- .origin[2]

  #### Simulate future locations
  lookup <- lapply(seq_len(.n_step), \(i) c(i + i - 1, i + i))
  pb <- progress::progress_bar$new(total = .n_step - 1)
  pb$tick(0)
  for (t in seq_len(.n_step - 1)) {
    pb$tick()
    mat[, lookup[[t + 1]]] <- .cstep_iter(.xy_now = mat[, lookup[[t]], drop = FALSE],
                                          .xy_next = mat[, lookup[[t + 1]], drop = FALSE],
                                          .lonlat = .lonlat,
                                          .flux = .flux, .fv = .flux_vals,
                                          .move = .move, .t = t,
                                          .bathy = .bathy)
  }

  #### Pivot path(s) into long format
  paths <-
    mat |>
    .sim_path_pivot(.n_step = .n_step, .n_path = .n_path) |>
    # Add information from .bathy
    mutate(cell_id = terra::cellFromXY(.bathy, cbind(.data$x, .data$y)),
           cell_x = terra::xFromCell(.bathy, .data$cell_id),
           cell_y = terra::yFromCell(.bathy, .data$cell_id),
           cell_z = terra::extract(.bathy, .data$cell_id)[, 1]) |>
    as.data.table()
  # Save flux parameters
  attr(paths, "flux") <- .flux_vals

  #### Visualise paths
  if (.plot) {
    pp <- one_page(.one_page, length(unique(paths$path_id)))
    on.exit(graphics::par(pp), add = TRUE)
    lapply(split(paths, paths$path_id), function(d) {
      terra::plot(.bathy, main = d$path_id[1])
      add_sp_path(d$x, d$y, length = 0)
    }) |> invisible()
  }

  #### Return outputs
  paths

}

#' @rdname sim_path_flux
#' @keywords internal

.flux_template <- function(.n_step, .n_path) {
  list(
    length = data.table(matrix(NA_real_, ncol = .n_step, nrow = .n_path)),
    angle = data.table(matrix(NA_real_, ncol = .n_step, nrow = .n_path))
  )
}

#' @rdname sim_path_flux
#' @keywords internal

.cstep_using_flux <- function(.xy_now, .xy_next, .lonlat, .fv, .t) {
  cstep(.xy_now = .xy_now, .xy_next = .xy_next, .lonlat = .lonlat,
        .length = .fv$length[[.t]], .angle = .fv$angle[[.t]])
}

#' @rdname sim_path_flux
#' @keywords internal

.cstep_iter <- function(.xy_now,
                        .xy_next = matrix(NA, nrow = nrow(.xy_now), ncol = ncol(.xy_now)),
                        .lonlat,
                        .flux, .fv, .t,
                        .move,
                        .bathy) {
  counter <- 0
  run     <- TRUE
  while (run && counter < 100) {
    # Identify .xy_next positions that need to be updated
    if (counter == 0) {
      pos <- seq_len(nrow(.xy_now))
    } else{
      pos <- which(is.na(.xy_next[, 1]))
    }
    # Simulate proposal locations
    .flux(.fv, .row = pos, .col = .t)
    .xy_next <- .move(.xy_now = .xy_now,
                      .xy_next = .xy_next,
                      .lonlat = .lonlat,
                      .fv = .fv, .t = .t)
    # Validate simulated positions
    vals <- terra::extract(.bathy, .xy_next[pos, , drop = FALSE])
    .xy_next[pos[which(is.na(vals))], ] <- NA
    # Specify whether or not to rerun simulation
    run <- any(is.na(.xy_next))
    if (run) {
      counter <- counter + 1
    }
  }
  if (any(is.na(.xy_next))) {
    n_invalid <- length(which(is.na(.xy_next[, 1])))
    pc_invalid <- round((n_invalid / nrow(.xy_next)) * 100, digits = 2)
    abort("Failed to generate {n_invalid}/{nrow(.xy_next)} path(s) ({pc_invalid} %) at time {.t}.", .envir = environment())
  }
  .xy_next
}

#' @rdname sim_path_flux
#' @keywords internal

.sim_path_pivot <- function(.mat, .n_step, .n_path) {
  .mat |>
    as.data.table() |>
    collapse::pivot() |>
    mutate(path_id = rep(seq_len(.n_path), .n_step * 2),
           coordinate = rep(rep(c("x", "y"), each = .n_path), .n_step),
           timestep = rep(seq_len(.n_step), each = .n_path * 2),
           variable = NULL
    ) |>
    collapse::pivot(how = "wider", names = "coordinate") |>
    arrange(.data$path_id, .data$timestep) |>
    as.data.table()
}

#' @rdname sim_path_flux
#' @keywords internal

.flux_pivot <- function(.fv, .n_step = ncol(.fv), .n_path = nrow(.fv)) {
  .fv |>
    collapse::pivot() |>
    mutate(path_id = rep(seq_len(.n_path), .n_step),
           timestep = rep(seq_len(.n_step), each = .n_path)) |>
    arrange(.data$path_id, .data$timestep) |>
    select("path_id", "timestep", "value") |>
    as.data.table()
}


#' @title Simulate detections
#' @name sim_detections_internals
#' @keywords internal

#' @rdname sim_detections_internals
#' @keywords internal

.sim_detections <- function(.path, .array,
                            .calc_distance = terra::distance, .lonlat = FALSE,
                            .calc_detection_pr = calc_detection_pr, ...,
                            .sim_obs = stats::rbinom,
                            .return = c("array_id", "path_id",
                                        "timestep", "receiver_id",
                                        "dist", "pr")
                            ){

  #### Calculate distances between path and receivers
  # Define matrices for distance calculations
  x <- y <- receiver_x <- receiver_y <- NULL
  pxy <- as.matrix(.path[, list(x, y)])
  rxy <- as.matrix(.array[, list(receiver_x, receiver_y)])
  # Define (Euclidean) distance matrix between points along the path & receivers
  # * Rows represent time steps (for one paths)
  # * Columns represent receivers (for one array)
  # * Note that this approach will fail for very large matrices
  dist_mat <- .calc_distance(pxy, rxy, lonlat = .lonlat)

  #### Calculate detection probability
  out <-
    dist_mat |>
    as.data.table() |>
    collapse::pivot() |>
    mutate(path_id = .path$path_id[1],
           array_id = .array$array_id[1],
           receiver_id = rep(.array$receiver_id, each = nrow(dist_mat)),
           timestep = rep(.path$timestep, ncol(dist_mat))) |>
    select("path_id", "array_id", "timestep", "receiver_id", dist = "value") |>
    arrange(.data$timestep, .data$receiver_id) |>
    as.data.table()

  #### Define detection probabilities from distances
  out <-
    out |>
    merge(.path, by = c("path_id", "timestep")) |>
    merge(.array, by = c("array_id", "receiver_id"))

  #### Calculate detection probabilities
  pr <- NULL
  out[, pr := .calc_detection_pr(out, ...)]

  #### Simulate detections & return outputs
  out <-
    out |>
    # Simulate detections
    mutate(detection = .sim_obs(n = nrow(out), size = 1, prob = .data$pr)) |>
    # Drop non detections
    filter(.data$detection == 1L) |>
    mutate(detection = NULL) |>
    arrange(.data$timestep, .data$receiver_id) |>
    as.data.table()
  if (!is.null(.return)) {
    out <-
      out |>
      select(all_of(.return)) |>
      as.data.table()
  }
  if (nrow(out) == 0L) {
    warn("No detections generated.")
  }
  out
}

#' @rdname sim_detections_internals
#' @keywords internal

.sim_detections_call <- function(.path, .array, .args) {
  .args$.path <- .path
  .args$.array <- .array
  do.call(.sim_detections, .args)
}
