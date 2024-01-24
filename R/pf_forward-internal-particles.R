#' @title PF: particle internals
#' @description These internal functions facilitate proposals, likelihood calculations and sampling in [`pf_forward()`].
#'
#' @details
#'
#' Function hierarchy is as follows:
#' * `pf_rpropose_*()` functions generate proposals;
#' * `pf_lik_*()` functions calculate likelihoods;
#' * `pf_sample_*()` functions sample proposals;
#' * `pf_particle_*()` functions wrap proposals, likelihood calculations and sampling in an iterative framework;
#'
#' Proposal and likelihood functions must accept the following core arguments:
#' * `.particles`
#' * `.obs`
#' * `.t`
#' * `.dlist`
#'
#' Proposal functions must also accept:
#' * `.rargs`
#' * `.dargs`
#'
#' [`.pf_lik()`] is a wrapper for specified likelihood functions (see [`pf_lik`]). The function returns a [`data.table`] that defines valid proposal locations, likelihoods and weights. A `diagnostics` attribute contains proposal diagnostics. This also requires:
#' * `.stack`---a named `list` of likelihood functions (see [`pf_lik`]).
#' * `.diagnostics`---an empty `list` used to store likelihood diagnostics, or `NULL`.
#' * `.trial`---an `integer` that distinguishes trials.
#'
#' Most likelihood functions are directly exported but some internals routines are also documented here for convenience, namely [`.pf_lik_ac_detection()`]. See also `acs_*()` functions.
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
#' * Iteration arguments (`.trial_crit`, `.trial_count`);
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
.pf_rpropose_origin <- function(.particles = NULL, .obs, .t = 1L, .dlist,
                                .rargs = NULL, .dargs = NULL){

  # Extract required objects from .dlist
  if (is.null(.dlist$spatial$origin)) {
    .dlist$spatial$origin <- .dlist$spatial$bathy
  }
  if (is.null(.dlist$spatial$origin)) {
    abort("`.dlist$spatial$origin` (and/or `.dlist$spatial$bathy`) is required.")
  }
  moorings          <- .dlist$data$moorings
  detection_kernels <- .dlist$algorithm$detection_kernels

  # (1) Define 'quadrature points' within acoustic containers
  if (!is.null(moorings)) {
    # Define container for possible locations
    # * .grid = FALSE uses a vector buffer rather than a gridded buffer, for speed
    .grid <- FALSE
    if (!.grid) {
      detection_kernels <- NULL
    }
    container <- .acs_container_1(.obs,
                                  .detection_kernels = detection_kernels,
                                  .moorings = moorings)
    # Sample cell coordinates within container
    terra::crs(container) <- terra::crs(.dlist$spatial$origin)
    samples <- spatSampleDT(container, .spatcell = .dlist$spatial$origin)

    # (2) Sample quadrature points on `.origin`
  } else {
    samples <- spatSampleDT(.x = .dlist$spatial$origin)
  }

  # Tidy data.table
  samples |>
    mutate(timestep = 1L,
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
                              .sample, .trial_crit, .trial_count) {
  # Set variables
  crit        <- 0
  count       <- 1L
  diagnostics <- list()
  # Implement iterative sampling
  while (crit < .trial_crit & count <= .trial_count) {
    # Sample particles for the current time step (pnow)
    label <- paste0("sample-origin-", count)
    pnow  <- .sample(.particles = .particles, .n = .n)
    # Calculate diagnostics of sample
    diagnostics[[label]] <-
      .pf_diag(.particles = pnow, .t = 1L, .trial = count, .label = "sample-origin")
    # Update loop
    crit  <- diagnostics[[label]]$n_u
    count <- count + 1L
  }
  if (crit < .trial_crit) {
    abort("Insufficient starting locations.")
  }
  # Return outputs
  attr(pnow, "diagnostics") <- diagnostics
  pnow
}

#' @rdname pf_particle
#' @keywords internal

.pf_particles_origin <- function(.particles = NULL, .obs, .t = 1L, .dlist,
                                 .rpropose = NULL, .dpropose = NULL,
                                 .rargs = NULL, .dargs = NULL,
                                 .likelihood,
                                 .sample, .n,
                                 .trial_crit, .trial_count, .control) {
  # Generate proposal location(s)
  diagnostics <- list()
  proposals <- .pf_rpropose_origin(.obs = .obs,
                                   .dlist = .dlist)
  # Calculate likelihood(s) & weights
  proposals <- .pf_lik(.particles = proposals,
                       .obs = .obs,
                       .t = 1L,
                       .dlist = .dlist,
                       .stack = .likelihood)
  diagnostics[["lik"]] <- attr(proposals, "diagnostics")
  # Sample proposal location(s)
  pnow <- .pf_sample_origin(.particles = proposals,
                            .n = .n, .sample = .sample,
                            .trial_crit = .trial_crit,
                            .trial_count = .trial_count)
  diagnostics[["sample"]] <- attr(pnow, "diagnostics")
  # Return outputs
  attr(pnow, "diagnostics") <- diagnostics
  pnow
}

#' @rdname pf_particle
#' @keywords internal

.pf_lik <- function(.particles, .obs, .t, .dlist,
                    .stack,
                    .diagnostics = list(), .trial = NA_integer_) {

  #### Set up
  # Define global variables
  lik <- weight <- NULL
  .particles[, lik := 1]
  .particles[, weight := 1 / fnrow(.particles)]
  # Define baseline (proposal) diagnostics
  if (!is.null(.diagnostics)) {
    .diagnostics[["proposal"]] <-
      .pf_diag(.particles = .particles, .t = .t,
               .trial = .trial, .label = "proposal")
  }

  #### Calculate likelihoods
  for (i in seq_len(length(.stack))) {
    if (.pf_diag_any(.particles)) {
      .particles <- .stack[[i]](.particles = .particles,
                               .obs = .obs,
                               .t = .t,
                               .dlist = .dlist)
      # Normalise weights to track ESS
      .particles[, weight := normalise(lik)]
      if (!is.null(.diagnostics)) {
        .diagnostics[[names(.stack)[i]]] <-
          .pf_diag(.particles = .particles, .t = .t,
                   .trial = .trial, .label = paste0("lik-", names(.stack)[i]))
      }
    }
  }

  #### Return outputs
  .particles[, weight := normalise(lik)]
  attr(.particles, "diagnostics") <- .diagnostics
  .particles

}

#' @rdname pf_particle
#' @keywords internal

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

.pf_particles_kick <- function(.particles, .obs, .t, .dlist,
                               .rpropose, .dpropose = NULL,
                               .rargs = list(), .dargs = NULL,
                               .likelihood,
                               .sample, .n,
                               .trial_crit, .trial_count, .control) {
  # Set variables
  diagnostics <- list()
  crit  <- 0
  count <- 1L
  .rargs$.particles <- .particles
  # Implement iterative sampling
  while (crit < .trial_crit & count <= .trial_count) {
    # Propose particles
    proposals <- do.call(.rpropose, .rargs)
    # Calculate likelihood & weights (likelihood = weights)
    proposals <- .pf_lik(.particles = proposals,
                         .obs = .obs, .t = .t, # .particles$timestep[1]
                         .dlist = .dlist,
                         .stack = .likelihood,
                         .trial = count)
    diagnostics[[paste0("kick-", count)]] <- attr(proposals, "diagnostics")
    # Sample particles
    pnow  <- .sample(.particles = proposals, .n = .n)
    label <- paste0("sample-kick-", count)
    diagnostics[[label]] <-
      .pf_diag(pnow, .t = .t, .label = "sample-kick", .trial = count)
    # Update loop
    crit  <- diagnostics[[label]]$n_u
    count <- count + 1L
  }
  # Return outputs
  attr(pnow, "diagnostics") <- diagnostics
  pnow
}

#' @rdname pf_particle
#' @keywords internal

.pf_particles_sampler <- function(.particles, .obs, .t, .dlist,
                                  .rpropose = NULL, .dpropose,
                                  .rargs = NULL, .dargs = list(),
                                  .likelihood,
                                  .sample, .n,
                                  .trial_crit, .trial_count, .control) {
  #### Set variables
  diagnostics <- list()
  chunks <- parallel::splitIndices(nrow(.particles),
                                   nrow(.particles) / .control$sampler_batch_size)

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
            .diagnostics = NULL)
  }) |>
    rbindlist(fill = TRUE) |>
    as.data.table()
  # Get summary diagnostics
  # * TO DO
  # * Provide a more detailed breakdown (include diagnostics in lapply())
  weight <- NULL
  proposals[, weight := normalise(weight)]
  diagnostics[["lik-directed"]] <- .pf_diag(.particles = proposals, .t = .t,
                                           .trial = NA_integer_, .label = "lik-directed")

  #### Implement sampler
  pnow <- proposals
  if (fnrow(pnow) > 0L) {

    ### Calculate movement densities & weights (likelihood * movement densities)
    .dargs$.particles <- proposals
    proposals <-
      # Calculate movement densities
      do.call(.dpropose, .dargs) |>
      # Calculate weights
      mutate(weight = normalise(.data$lik * .data$dens)) |>
      as.data.table()

    #### Sample particles (from the set of allowed particles)
    pnow  <- .sample(.particles = proposals, .n = .n)
    diagnostics[["sample-directed-1"]] <-
      .pf_diag(.particles = pnow, .t = .t, .label = "sample-directed", .trial = 1L)
    # Repeat sampling, if required
    count <- 2L
    crit  <- diagnostics[["sample-directed-1"]]$n_u
    while (crit < .trial_crit & count <= .trial_count) {
      pnow <- .sample(.particles = proposals, .n = .n)
      label <- paste0("sample-directed-", count)
      diagnostics[[label]] <-
        .pf_diag(.particles = pnow, .t = .t, .label = "sample-directed", .trial = count)
      crit  <- diagnostics[[label]]$n_u
      count <- count + 1L
    }
  }

  #### Return outputs
  attr(pnow, "diagnostics") <- diagnostics
  pnow
}
