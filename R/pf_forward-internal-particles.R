#' @title PF: particle internals
#' @description These internal functions facilitate proposals, likelihood calculations and sampling in [`pf_forward()`].
#'
#' @details
#'
#' Function hierarchy is as follows:
#' * `pf_rpropose_*()` functions generate proposals;
#' * `pf_lik_*()` functions calculate likelihoods;
#' * `pf_sample_*()` functions sample proposals;
#' * `pf_particle_*()` functions combine proposals, likelihood calculations and, if applicable, sampling;
#'
#' Proposal and likelihood functions must accept the following core arguments:
#' * `.particles`
#' * `.obs`
#' * `.t`
#' * `.dlist`
#' * `.drop` (except for `rpropose` functions);
#'
#' Proposal functions must also accept:
#' * `.rargs`
#' * `.dargs`
#'
#' [`.pf_lik()`] is a wrapper for specified likelihood functions (see [`pf_lik`]). The function returns a [`data.table`] that defines valid proposal locations, likelihoods and weights. A `diagnostics` attribute contains proposal diagnostics. This also requires:
#' * `.stack`---a named `list` of likelihood functions (see [`pf_lik`]).
#' * `.diagnostics`---an empty `list` used to store likelihood diagnostics, or `NULL`.
#' * `.control`---a named `list` of control options, from [`pf_opt_control()`].
#'
#' Most likelihood functions are directly exported but some internals routines are also documented here for convenience, namely [`.pf_lik_ac_detection()`] and [`.pf_lik_drop()`]. See also `acs_*()` functions.
#'
#' Sampling functions must accept:
#' * `.particles`
#' * `.n`
#'
#' Wrapper functions must accept:
#' * Proposal function arguments (`.particles`, `.obs`, `.t`, `.dlist`);
#' * Proposal functions (`.rpropose`,`.dpropose`) and argument lists (`.rargs`, `.dargs`);
#' * Additional likelihood arguments (`.likelihood`);
#' * Sampling arguments (`.sample`, `.n`);
#' * Iteration arguments, if applicable (`.trial_crit`, `.trial_count`);
#' * Control arguments (`.control`);
#'
#' Internal functions may support additional arguments as required.
#'
#' At the first time step:
#' * [`.pf_rpropose_origin()`] 'proposes' starting locations (quadrature points);
#' * [`.pf_sample_origin()`] samples starting locations;
#' * [`.pf_lik()`] calculates likelihoods;
#' * [`.pf_particles_origin()`] integrates [`.pf_rpropose_origin()`], [`.pf_lik()`] and [`.pf_particles_origin()`];
#'
#' At subsequent time steps:
#' * The exported functions [`pf_rpropose_kick()`] and [`pf_rpropose_reachable`] propose locations;
#' * [`.pf_lik()`] calculates likelihoods;
#' * [`.pf_particles_kick()`] and [`.pf_particles_sampler()`] are the integration functions;
#'
#' `pf_particles_*()` functions are similar internally, but currently separated for convenience.
#'
#' @author Edward Lavender
#' @name pf_particle

#' @rdname pf_particle
#' @keywords internal

# Propose origin locations
.pf_rpropose_origin <- function(.particles = NULL, .obs, .t = 1L, .dlist, .rargs = NULL){

  # Check user inputs
  # * TO DO: validate origin & bahty CRS are identical
  check_dlist(.dlist = .dlist,
              .spatial = "bathy")

  # Extract required objects from .dlist
  if (is.null(.dlist$spatial$origin)) {
    .dlist$spatial$origin <- .dlist$spatial$bathy
  }
  moorings <- .dlist$data$moorings

  # (1) Define 'quadrature points' within acoustic containers
  if (!is.null(moorings)) {
    # Define container for possible locations
    # * We set detection_kernels = NULL to use a vector buffer
    # * ... rather than a gridded buffer, for speed
    container <- .acs_container_1(.obs,
                                  .detection_kernels = NULL,
                                  .moorings = moorings)
    # Sample cell coordinates within container
    terra::crs(container) <- terra::crs(.dlist$spatial$origin)
    samples <- spatSampleDT(container, .spatcell = .dlist$spatial$bathy)

    # (2) Sample quadrature points on `.origin`
  } else {
    samples <- spatSampleDT(.x = .dlist$spatial$bathy)
  }

  # Tidy data.table
  samples |>
    lazy_dt(immutable = FALSE) |>
    mutate(timestep = .obs$timestep[.t],
           cell_past = NA_integer_,
           x_past = NA_integer_,
           y_past = NA_integer_,
           cell_now = .data$cell_id) |>
    select("timestep",
           "cell_past", "x_past", "y_past",
           "cell_now", x_now = "cell_x", y_now = "cell_y") |>
    as.data.table()
}

#' @rdname pf_particle
#' @keywords internal

.pf_sample_origin <- function(.particles, .n,
                              .sample, .trial_crit) {
  # Sample particles for the current time step (pnow)
  pnow  <- .sample(.particles = .particles, .n = .n)
  # Validate sampling sufficiency
  if (.pf_diag_ess(pnow) < .trial_crit) {
    abort("Insufficient starting locations.")
  }
  # Return outputs
  pnow
}

#' @rdname pf_particle
#' @keywords internal

.pf_particles_origin <- function(.particles = NULL, .obs, .t = 1L, .dlist,
                                 .rpropose = NULL, .dpropose = NULL,
                                 .rargs = NULL, .dargs = NULL,
                                 .likelihood,
                                 .sample, .n,
                                 .trial_crit, .control) {
  # Generate proposal location(s)
  proposals <- .pf_rpropose_origin(.obs = .obs,
                                   .dlist = .dlist)
  # Calculate likelihood(s)
  proposals <- .pf_lik(.particles = proposals,
                       .obs = .obs,
                       .t = 1L,
                       .dlist = .dlist,
                       .stack = .likelihood,
                       .control = .control)
  # Calculate weights
  weight <- lik <- NULL
  proposals[, weight := normalise(lik)]
  # Sample starting locations
  pnow <- .pf_sample_origin(.particles = proposals,
                            .n = .n, .sample = .sample,
                            .trial_crit = .trial_crit)
  # Return outputs
  pnow
}

#' @rdname pf_particle
#' @keywords internal

.pf_lik <- function(.particles, .obs, .t, .dlist, .stack, .control) {

  #### Set up
  lik <- NULL
  .particles[, lik := 1]

  #### Calculate likelihoods
  for (i in seq_len(length(.stack))) {
    if (.pf_diag_any(.particles)) {
      .particles <- .stack[[i]](.particles = .particles,
                                .obs = .obs,
                                .t = .t,
                                .dlist = .dlist,
                                .drop = .control$drop)
    }
  }

  #### Return outputs
  .particles

}

#' @rdname pf_particle
#' @keywords internal

# Internal function for acs_filter_container()
.acs_filter_container <- function(.dist, .buffer) {

  # Arguments
  # * .dist is the distance matrix (rows = particles, columns = future receivers)
  # * .buffer is a vector of maximum distances from receivers

  # Scope
  # * Particles are forced to be within (all) future containers
  # * This does not eliminate all impossible locations e.g., due to peninsulas
  # * But it is a quick way of dropping particles

  # Routine
  n <- length(.buffer)
  if (n == 1L) {
    # Single receiver
    # * If the next detection was at one receiver
    # * ... we simply check whether each particle is within the required radius
    bool <- .dist < .buffer
  } else {
    # Multiple receivers:
    # * If the next detection was at multiple receivers
    # * .... each particle must be within the required radius of each receiver
    ind <- seq_len(n)
    txt <-
      paste(
        paste0(".dist[, ", ind, "] < .buffer[", ind, "]"),
        collapse = " & "
      )
    bool <- eval(parse(text = txt))
  }
  log(bool + 0L)

}

# Calculate the likelihood of detection(s)
.pf_lik_ac_detection <- function(.particles, .kernels, .detections, .absences) {

  # Arguments:
  # * .particles - the particles data.table
  # * .kernels - the list of detection kernels
  # * .detections - an `integer` vector of the receiver(s) that recorded detections at a given time step
  # * .absences - an `integer` vector of the remaining, overlapping receiver(s) that did not record a detection, from [`.acs_absences()`]

  # Calculate Pr (detection | position) at each relevant receiver
  ldc <- length(.detections)
  mat <- matrix(NA, nrow(.particles), ncol = ldc)
  for (i in seq_len(ldc)) {
    mat[, i] <- terra::extract(.kernels$receiver_specific_kernels[[.detections[i]]], .particles$cell_now)[, 1]
  }
  # Calculate Pr (non detection | position) at each relevant receiver
  if (!is.null(.absences)) {
    mat_2 <- matrix(NA, nrow(.particles), ncol = length(.absences))
    lac <- length(.absences)
    for (i in seq_len(lac)) {
      mat_2[, i] <- terra::extract(.kernels$receiver_specific_inv_kernels[[.absences[i]]], .particles$cell_now)[, 1]
    }
    mat <- cbind(mat, mat_2)
  }
  # Calculate Pr (all data | position)
  colProds.matrix(mat)
}

#' @rdname pf_particle
#' @keywords internal

# Calculate the likelihood of a depth observation (.x)
.pf_lik_dc <- function(.x, .a, .b) {
  .a[.a < 0] <- 0
  stats::dunif(x = .x, min = .a, max = .b)
}

#' @rdname pf_particle
#' @keywords internal

.pf_particles_kick <- function(.particles, .obs, .t, .dlist,
                               .rpropose,
                               .rargs = list(),
                               .likelihood,
                               .control, .trial) {

  #### Set variables
  count <- 1L
  index <- cell_now <- x_now <- y_now <- lik <- NULL

  #### Define output data.table with blank coordinate and lik columns
  # These will be iteratively updated below
  # TO DO: optimise code to avoid copying .particles here
  .particles[, index := seq_row(.particles)]
  cols <- colnames(.particles)
  output <-
    .particles |>
    lazy_dt(immutable = TRUE) |>
    mutate(cell_now = NA_integer_,
           x_now = NA_real_,
           y_now = NA_real_,
           lik = 0) |>
    as.data.table()

  #### Iteratively update `output` with new locations
  .rargs$.particles <- .particles
  while (count <= .trial) {
    # Propose particles
    proposals <- do.call(.rpropose, .rargs)
    # Calculate likelihood & weights (likelihood = weights)
    proposals <- .pf_lik(.particles = proposals,
                         .obs = .obs, .t = .t,
                         .dlist = .dlist,
                         .stack = .likelihood,
                         .control = .control)
    # Update output particles
    # * Output is updated in locations where we have generated proposals
    # * Matching is necessary b/c the default .rpropose routine drops
    # * ... locations beyond the study area (as necessary for likelihood routines)
    # * Here, `ind` defines, for each row in `output`, the position of the match in proposals
    # * E.g., 1 NA NA NA  2 ...
    # * `pos` defines the rows in `output` that correspond to the matches (e.g., 1, 5, ...)
    ind     <- fmatch(output$index, proposals$index)
    matches <- !is.na(ind)
    ind     <- ind[matches]
    pos     <- which(matches)
    output[pos, cell_now := proposals$cell_now[ind]]
    output[pos, x_now := proposals$x_now[ind]]
    output[pos, y_now := proposals$y_now[ind]]
    output[pos, lik := proposals$lik[ind]]
    # Update loop
    count <- count + 1L
    if (count <= .trial) {
      # Identify proposals beyond the study area or with zero likelihood
      bool <- is.na(output$cell_now) | (output$lik == 0)
      if (any(bool)) {
        # Optionally re-kick invalid proposals
        .rargs$.particles <-
          output |>
          lazy_dt() |>
          filter(bool) |>
          select(all_of(cols)) |>
          as.data.table()
      }
    }
  }
  # Drop residual proposals beyond study area & (optional) zero-likelihood proposals
  output |>
    lazy_dt(immutable = FALSE) |>
    mutate(index = NULL) |>
    filter(!is.na(cell_now)) |>
    filter_lik(.control$drop) |>
    as.data.table()
}

#' @rdname pf_particle
#' @keywords internal

.pf_particles_sampler <- function(.particles, .obs, .t, .dlist,
                                  .dpropose,
                                  .dargs = list(),
                                  .likelihood,
                                  .control) {
  #### Set variables
  nrw    <- fnrow(.particles)
  chunks <- parallel::splitIndices(nrw, nrw / .control$sampler_batch_size)

  #### Define reachable locations & likelihoods
  proposals <- lapply(chunks, function(index) {
    # Propose particles (identify all reachable particles)
    proposals_for_index <- pf_rpropose_reachable(.particles = .particles[index, ],
                                                 .obs = .obs, .t = .t,
                                                 .dlist = .dlist)
    # Calculate likelihood & drop invalid particles
    .pf_lik(.particles = proposals_for_index,
            .obs = .obs, .t = .t,
            .dlist = .dlist,
            .stack = .likelihood,
            .control = .control)
  }) |>
    rbindlist(fill = TRUE)

  #### Update 'likelihoods' with movement densities
  pnow <- proposals
  if (fnrow(pnow) > 0L) {
    .dargs$.particles <- proposals
    pnow <-
      # Calculate movement densities
      do.call(.dpropose, .dargs) |>
      lazy_dt(immutable = FALSE) |>
      # Update 'likelihood'
      # * It is necessary to update likelihoods in this way
      # * for correct calculation/updating of the weights
      mutate(lik = .data$lik * .data$dens) |>
      as.data.table()
  }

  #### Return outputs
  pnow
}
