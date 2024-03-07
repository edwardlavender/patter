#' @title PF: likelihood functions
#' @description These are likelihood functions for the forward simulation. [`pf_forward()`] expects a named `list` of likelihood functions that evaluate the log-likelihood of each dataset given location proposals. Convenience functions are provided that calculate the log-likelihood of acoustic and archival data, given location proposals.
#'
#' @param .particles A [`data.table`] that defines proposal locations. This contains the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * `cell_now`---an `integer` that defines the grid cell(s) of proposal location(s);
#' * `x_now`,`y_now`---`double`s that define proposal location coordinates;
#' * `loglik`---a `double` that defines the log-likelihood. At each time step, this begins as a vector of `0`s **that should be progressively updated by each likelihood function**.
#' @param .obs,.t (optional) The `.obs` [`data.table`] (from [`pf_forward()`]) and an `integer` that defines the time step (used to index `.obs`).
#' @param .dlist (optional) The `.dlist` `list` (from [`pf_forward()`]).
#' * For [`acs_filter_land()`], `.dlist` should contain `.dlist$spatial$bathy` and `.dlist$pars$spatna`.
#' * For [`acs_filter_container()`], `.dlist` should contain `.dlist$pars$lonlat`.
#' * For [`pf_lik_ac()`], `.dlist` should contain `.dlist$algorithm$detection_kernels` (from [`acs_setup_detection_kernels()`].
#' * For [`pf_lik_dc()`], `.dlist` should contain `.dlist$spatial$bathy`.
#' * For custom likelihood functions, `.dlist` may require other datasets.
#' @param .drop A `logical` variable that defines whether or not to drop `.particles` rows with zero likelihood.
#'
#' @details
#' In [`pf_forward()`], the log-likelihood of the data given proposal locations is evaluated using a named `list` of user-defined functions (one for each dataset). Each function must accept a [`data.table`] of proposal locations (`.particles`) alongside the arguments named above (even if they are ignored), evaluate the log-likelihood of the data and return an updated [`data.table`] for the subset of valid proposals and _updated_ log-likelihoods (in the `loglik` column). For computational efficiency, it is desirable that functions are ordered by the extent to which they invalidate proposal locations and that each function drops invalid proposals (since this reduces the number of subsequent likelihood calculations). For faster evaluations, it might also pay to group likelihood terms under a single wrapper function (since this eliminates the need to loop over individual terms in [`.pf_lik()`]).
#'
#' The following convenience functions are provided:
#' * [`acs_filter_land()`] is a binary 'habitability' (land/water) filter. This is useful when the 'stochastic kick' methodology is used to generate proposal locations in systems that include inhospitable habitats. The function calculates the log-likelihood (-Inf, 0) of the 'hospitable' data given sampled particles. Location proposals in `NA` cells on the bathymetry grid (`.dlist$spatial$bathy`) are dropped.
#' * [`acs_filter_container()`] is recommended for acoustic time series. This is a binary filter that excludes location proposals that are incompatible with acoustic container dynamics, since proposal locations must be within a moveable distance of receiver locations. Note that receivers by default are defined on a grid and the mobility term should account for this if required (see [`check_moorings()`]). [`acs_filter_container()`] is strictly optional but facilitates algorithm convergence by filtering location proposals that are inconsistent with the location of the next detection.
#' * [`pf_lik_ac()`] calculates the log-likelihood of acoustic data (detection or non detection at each operational receiver), given location proposals. Likelihood evaluations are implemented on a grid using precomputed fields for speed (see [`acs_setup_detection_kernels()`]).
#' * [`pf_lik_dc()`] calculates the log-likelihood of archival (depth) data, given particle proposals. This is based on a modification of Lavender et al.'s (2023) depth-contour algorithm in which depth observations are considered valid (with uniform probability density) in locations whose depth's lie between a shallow and deep limit (`.particles$bathy - .obs$depth_shallow_eps[.t]` and `.particles$bathy` + `.obs$depth_deep_eps[.t]`) but impossible otherwise. This evaluation is necessarily implemented on a grid.
#'
#' In [`pf_forward()`], [`acs_filter_container()`] and [`pf_lik_ac()`] effectively replace the role of [`flapper::ac()`](https://edwardlavender.github.io/flapper/reference/ac.html) function. [`pf_lik_dc()`] effectively replaces the role of [`flapper::dc()`](https://edwardlavender.github.io/flapper/reference/dc.html).
#'
#' @return Functions return the `.particles` [`data.table`] for valid proposal locations and updated likelihoods (in the `loglik` column).
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @name pf_lik

#' @rdname pf_lik
#' @export

# Eliminate particles in inhospitable locations (e.g., on land)
acs_filter_land <- function(.particles, .obs, .t, .dlist, .drop) {
  if (.t == 1L) {
    check_dlist(.dlist = .dlist, .spatial = "bathy", .par = "spatna")
    if (is.null(.dlist$pars$spatna)) {
      warn("`.dlist$pars$spatna` is undefined & assumed FALSE by `acs_filter_land()`.")
    }
  }
  if (isTRUE(.dlist$pars$spatna)) {
    # Define bathy by reference
    set_bathy(.data = .particles, .dlist = .dlist)
    # Update log-likelihoods by reference
    set_loglik(.particles, .loglik = log(!is.na(.particles$bathy) + 0L))
    # Drop zero likelihoods
    if (.drop) {
      loglik <- NULL
      .particles <- .particles[loglik > -Inf, ]
    }
  }
  .particles
}

#' @rdname pf_lik
#' @export

# Eliminate particles incompatible with container dynamics
acs_filter_container <- function(.particles, .obs, .t, .dlist, .drop) {

  #### Checks
  if (.t == 1L) {
    check_names(.obs, req = "container")
    check_dlist(.dlist,
                .par = "lonlat")
    rlang::check_installed("Rfast")
  }

  #### Filter
  if (.t > 1 && .t < fnrow(.obs)) {
    # Calculate distances between particle samples & the receivers that recorded the next detection
    dist <- terra::distance(x = cbind(.particles$x_now, .particles$y_now),
                            y = .obs$container[[.t]]$coord,
                            lonlat = .dlist$par$lonlat)
    # Set log-likelihoods by reference
    set_loglik(.particles,
               .loglik = .acs_filter_container(.dist = dist, .buffer = .obs$container[[.t]]$buffer))
    # Drop zero likelihoods
    if (.drop) {
      loglik <- NULL
      .particles <- .particles[loglik > -Inf, ]
    }
  }
  .particles
}

#' @rdname pf_lik
#' @export

# Evaluate likelihood of acoustic data
pf_lik_ac <- function(.particles, .obs, .t, .dlist, .drop) {

  #### Checks
  if (.t == 1L) {
    check_names(.obs, c("array_id", "detection", "acoustics"))
    check_dlist(.dlist, .algorithm = "detection_kernels")
  }

  #### Calculate likelihood _detection/non-detection_ given positions
  # For each particle, evaluate the likelihood of all relevant acoustic data
  if (.obs$detection[.t] == 1L) {
    pxy  <- cbind(.particles$x_now, .particles$y_now)
    amat <- .obs$acoustics[[.t]]
    loglik <-
      lapply(names(amat), function(r) {
        # For each particle, extract probability of detection at receiver
        # * This is much faster for receiver kernels in memory
        prob <- terra::extract(.dlist$algorithm$detection_kernels$pkernel[[r]], pxy)[, 1]
        prob[is.na(prob)] <- 0
        # For each particle, evaluate the log-likelihood of the acoustic data at that receiver
        dbinom(amat[r], size = 1L, prob = prob, log = TRUE)
      })
      # Combine likelihoods across receivers
    loglik <- Rfast::rowsums(do.call(cbind, loglik))

  #### Calculate likelihood _non detections_ at all receivers given positions
  } else if (.obs$detection[.t] == 0L) {
    loglik <-
      terra::extract(
        .dlist$algorithm$detection_kernels$loglik[[.obs$array_id[.t]]],
        cbind(.particles$x_now, .particles$y_now))[, 1]
    loglik[is.na(loglik)] <- -Inf # log(0)

  }

  #### Update likelihoods
  set_loglik(.particles, loglik)
  if (.drop) {
    .particles <- .particles[loglik > -Inf, ]
  }
  .particles

}

#' @rdname pf_lik
#' @export

# Evaluate particles incompatible with the depth data
pf_lik_dc <- function(.particles, .obs, .t, .dlist, .drop) {
  # Checks
  if (.t == 1L) {
    check_names(.obs, req = c("depth_shallow_eps", "depth_deep_eps"))
    check_dlist(.dlist = .dlist, .spatial = "bathy")
  }
  # Set log-likelihoods by reference
  set_bathy(.data = .particles, .dlist = .dlist)
  set_loglik(.data = .particles,
             .loglik = .pf_lik_dc(.x = .obs$depth[.t],
                                  .a =  .particles$bathy - .obs$depth_shallow_eps[.t],
                                  .b = .particles$bathy + .obs$depth_deep_eps[.t]))
  # Drop zero likelihoods
  if (.drop) {
    loglik <- NULL
    .particles <- .particles[loglik > -Inf, ]
  }
  .particles
}
