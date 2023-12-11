#' @title PF: particle helpers
#' @details
#' Hierarchy
#' pf_propose_
#' pf_sample_
#' pf_particles

#' @name pf_particle

#' @rdname pf_particle
#' @keywords internal

.pf_sample_origin <- function(.particles, .sample, .n, .trial_crit, .trial_count) {
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

.pf_particles_origin <- function(.obs,
                                 .origin,
                                 .grid = FALSE,
                                 .detection_kernels, .moorings,
                                 .bathy,
                                 .pf_lik,
                                 .sample, .n,
                                 .trial_crit, .trial_count) {
  # Generate proposal location(s)
  diagnostics <- list()
  proposals <- pf_rpropose_origin(.obs = .obs, .origin = .origin, .grid = .grid,
                                  .detection_kernels = .detection_kernels, .moorings = .moorings)
  # Calculate likelihood(s) & weights
  proposals <- .pf_lik(.particles = proposals, .t = 1L)
  diagnostics[["lik"]] <- attr(proposals, "diagnostics")
  # Sample proposal location(s)
  pnow <- .pf_sample_origin(proposals,
                           .sample = .sample, .n = .n,
                           .trial_crit = .trial_crit,
                           .trial_count = .trial_count)
  diagnostics[["sample"]] <- attr(pnow, "diagnostics")
  # Return outputs
  attr(pnow, "diagnostics") <- diagnostics
  pnow
}

#' @rdname pf_particle
#' @keywords internal

.pf_particles_kick <- function(.particles,
                               .rpropose, .obs, .t, .bathy,
                               .pf_lik,
                               .sample, .n,
                               .trial_crit, .trial_count) {
  # Set variables
  diagnostics <- list()
  crit  <- 0
  count <- 1L
  # Implement iterative sampling
  while (crit < .trial_crit & count <= .trial_count) {
    # Propose particles
    proposals <- .rpropose(.particles = .particles, .obs = .obs, .t = .t, .bathy = .bathy)
    # Calculate likelihood & weights (likelihood = weights)
    proposals <- .pf_lik(.particles = proposals, .t = .particles$timestep[1], .trial = count)
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

.pf_particles_sampler <- function(.particles,
                                  .obs, .t, .bathy, .lonlat,
                                  .pf_lik, .dpropose,
                                  .sample, .n,
                                  .trial_crit, .trial_count) {
  # Set variables
  diagnostics <- list()
  # Propose particles (identify all reachable particles)
  proposals <- pf_rpropose_reachable(.particles = .particles, .obs = .obs, .t = .t, .bathy = .bathy)
  # Calculate likelihood
  proposals <- .pf_lik(.particles = proposals, .t = .t)
  diagnostics[["sampler-lik"]] <- attr(proposals, "diagnostics")
  # Calculate movement densities & weights (likelihood * movement densities)
  proposals <-
    # Calculate movement densities
    .dpropose(proposals, .lonlat = .lonlat) |>
    # Calculate weights & normalise
    mutate(weight = .data$lik * .data$dens,
           weight = .data$weight / sum(.data$weight)) |>
    as.data.table()
  diagnostics[["sampler-dens"]] <-
    .pf_diag(.particles = proposals, .t = .t, .trial = NA_integer_, .label = "directed-dens")
  # Sample particles (from the set of allowed particles)
  count <- 1L
  crit  <- diagnostics[["sampler-dens"]]$n_u
  pnow  <- proposals
  if (nrow(pnow) > 0L) {
    while (crit < .trial_crit & count <= .trial_count) {
      pnow <- .sample(.particles = proposals, .n = .n)
      label <- paste0("sampler-sample-", count)
      diagnostics[[label]] <-
        .pf_diag(pnow, .t = .t, .label = "sampler-sample", .trial = count)
      crit  <- diagnostics[[label]]$n_u
      count <- count + 1L
    }
  }
  # Return outputs
  attr(pnow, "diagnostics") <- diagnostics
  pnow
}
