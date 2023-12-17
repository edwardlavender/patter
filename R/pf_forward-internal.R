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
#' Proposal and likelihood functions must accept the following arguments:
#' * `.particles`
#' * `.obs`
#' * `.t`
#' * `.dlist`
#'
#' [`.pf_lik()`] is a wrapper for specified likelihood functions (see [`pf_lik`]). The function returns a [`data.table`] that defines valid proposal locations, likelihoods and weights. A `diagnostics` attribute contains proposal diagnostics. This also requires:
#' * `.stack`---a named `list` of likelihood functions (see [`pf_lik`]).
#' * `.diagnostics`---an empty `list` used to store likelihood diagnostics, or `NULL`.
#' * `.trial` An `integer` that distinguishes trials.
#'
#' Sampling functions must accept:
#' * `.particles`
#' * `.n`
#'
#' Wrapper functions must accept:
#' * Proposal function arguments (`.particles`, `.obs`, `.t`, `.dlist`);
#' * Proposal functions (`.rpropose`,`.dpropose`);
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
#' * [`.pf_particles_origin()`] integrates [`.pf_propose_origin()`], [`.pf_lik()`] and [`.pf_particles_origin()`];
#'
#' At subsequent time steps:
#' * The exported functions [`pf_rpropose_kick()`] and [`pf_rpropose_reachable`] propose locations;
#' * [`.pf_lik()`] calculates likelihoods;
#' * [`.pf_particles_kick()`] and [`.pf_particles_sampler()`] are the integration function;
#'
#' `pf_particles_*()` functions are similar internally, but currently separated for convenience.
#'
#' @author Edward Lavender
#' @name pf_particle

#' @rdname pf_particle
#' @keywords internal

# Propose origin locations
# * `.particles`, `.obs`, `.t` and `.dlist` are required for consistency but unused
.pf_rpropose_origin <- function(.particles = NULL, .obs, .t = 1L, .dlist, .origin, .grid = FALSE) {

  moorings          <- .dlist$data$moorings
  detection_kernels <- .dlist$algorithm$detection_kernels

  # (1) Define 'quadrature points' within acoustic containers
  if (!is.null(moorings)) {
    # Define container for possible locations
    # * .grid = FALSE uses a vector buffer rather than a gridded buffer, for speed
    if (!.grid) {
      detection_kernels <- NULL
    }
    container <- .acs_container_1(.obs,
                                  .detection_kernels = detection_kernels,
                                  .moorings = moorings)
    # Sample cell coordinates within container
    terra::crs(container) <- terra::crs(.origin)
    samples <- spatSampleDT(container, .spatcell = .origin)

    # (2) Sample quadrature points on `.origin`
  } else {
    samples <- spatSampleDT(.x = .origin)
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
    label <- paste0("sample-", count)
    pnow  <- .sample(.particles = .particles, .n = .n)
    # Calculate diagnostics of sample
    diagnostics[[label]] <-
      .pf_diag(.particles = pnow, .t = 1L, .trial = count, .label = "sample")
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
                                 .origin, .rpropose = NULL, .dpropose = NULL,
                                 .likelihood,
                                 .sample, .n,
                                 .trial_crit, .trial_count, .control) {
  # Generate proposal location(s)
  diagnostics <- list()
  proposals <- .pf_rpropose_origin(.obs = .obs,
                                   .dlist = .dlist,
                                   .origin = .origin,
                                   .grid = FALSE)
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
  # Define baseline diagnostics
  if (!is.null(.diagnostics)) {
    .diagnostics[["base"]] <-
      .pf_diag(.particles = .particles, .t = .t,
               .trial = .trial, .label = "base")
  }

  #### Calculate likelihoods
  for (i in seq_len(length(.stack))) {
    if (.pf_diag_any(.particles)) {
      .particles <- .stack[[i]](.particles = .particles,
                               .obs = .obs,
                               .t = .t,
                               .dlist = .dlist)
      if (!is.null(.diagnostics)) {
        .diagnostics[[names(.stack)[i]]] <-
          .pf_diag(.particles = .particles, .t = .t,
                   .trial = .trial, .label = names(.stack)[i])
      }
    }
  }

  #### Return outputs
  .particles[, weight := lik]
  attr(.particles, "diagnostics") <- .diagnostics
  .particles

}

#' @rdname pf_particle
#' @keywords internal

.pf_particles_kick <- function(.particles, .obs, .t, .dlist,
                               .rpropose, .dpropose = NULL,
                               .likelihood,
                               .sample, .n,
                               .trial_crit, .trial_count, .control) {
  # Set variables
  diagnostics <- list()
  crit  <- 0
  count <- 1L
  # Implement iterative sampling
  while (crit < .trial_crit & count <= .trial_count) {
    # Propose particles
    proposals <- .rpropose(.particles = .particles,
                           .obs = .obs,
                           .t = .t,
                           .dlist = .dlist)
    # Calculate likelihood & weights (likelihood = weights)
    proposals <- .pf_lik(.particles = proposals,
                         .obs = .obs, .t = .t, # .particles$timestep[1]
                         .dlist = .dlist,
                         .stack = .likelihood,
                         .trial = count)
    diagnostics[[paste0("kick-", count)]] <- attr(proposals, "diagnostics")
    # Sample particles
    pnow  <- .sample(.particles = proposals, .n = .n)
    label <- paste0("kick-sample-", count)
    diagnostics[[label]] <-
      .pf_diag(pnow, .t = .t, .label = "kick-sample", .trial = count)
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
                                  .likelihood,
                                  .sample, .n,
                                  .trial_crit, .trial_count, .control) {
  #### Set variables
  diagnostics <- list()
  diagnostics[["sampler-base"]] <-
    .pf_diag(.particles = .particles, .t = .t,
             .trial = NA_integer_, .label = "base")
  chunks <- parallel::splitIndices(nrow(.particles),
                                   nrow(.particles) / .control$sampler_batch_size)

  #### Define reachable locations & likelihoods
  proposals <- lapply(chunks, function(index) {
    # Propose particles (identify all reachable particles)
    proposals_for_index <- pf_rpropose_reachable(.particles = .particles[index, ],
                                                 .obs = .obs, .t = .t,
                                                 .dlist = dlist)
    # Calculate likelihood & drop invalid particles
    .pf_lik(.particles = proposals_for_index,
            .obs = .obs, .t = .t,
            .dlist = .dlist,
            .stack = .likelihood,
            .diagnostics = NULL)
  }) |>
    rbindlist() |>
    as.data.table()
  # Get summary diagnostics
  # * TO DO
  # * Provide a more detailed breakdown (include diagnostics in lapply())
  diagnostics[["sampler-lik"]] <- .pf_diag(.particles = .particles, .t = .t,
                                           .trial = NA_integer_, .label = "sampler-lik")

  #### Implement sampler
  pnow <- proposals
  if (fnrow(pnow) > 0L) {

    ### Calculate movement densities & weights (likelihood * movement densities)
    proposals <-
      # Calculate movement densities
      .dpropose(.particles = proposals,
                .obs = .obs,
                .t = .t,
                .dlist = .dlist) |>
      # Calculate weights & normalise
      mutate(weight = .data$lik * .data$dens,
             weight = .data$weight / sum(.data$weight)) |>
      as.data.table()

    #### Sample particles (from the set of allowed particles)
    pnow  <- .sample(.particles = proposals, .n = .n)
    diagnostics[["sampler-sample-1"]] <-
      .pf_diag(.particles = proposals, .t = .t, .label = "sampler-sample", .trial = 1L)
    # Repeat sampling, if required
    count <- 2L
    crit  <- diagnostics[["sampler-sample-1"]]$n_u
    while (crit < .trial_crit & count <= .trial_count) {
      pnow <- .sample(.particles = proposals, .n = .n)
      label <- paste0("sampler-sample-", count)
      diagnostics[[label]] <-
        .pf_diag(pnow, .t = .t, .label = "sampler-sample", .trial = count)
      crit  <- diagnostics[[label]]$n_u
      count <- count + 1L
    }
  }

  #### Return outputs
  attr(pnow, "diagnostics") <- diagnostics
  pnow
}
