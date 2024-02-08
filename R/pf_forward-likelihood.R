#' @title PF: likelihood functions
#' @description These are likelihood functions for the forward simulation. [`pf_forward()`] expects a named `list` of likelihood functions that evaluate the likelihood of each dataset given location proposals. Convenience functions are provided that calculate the likelihood of acoustic and archival data, given location proposals, as resolved by the acoustic-container and depth-contour algorithms presented by Lavender et al. (2023).
#'
#' @param .particles A [`data.table`] that defines proposal locations. This contains the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * `cell_now`---an `integer` that defines the grid cell(s) of proposal location(s);
#' * `x_now`,`y_now`---`double`s that define proposal location coordinates;
#' * `lik`---a `double` that defines the likelihood. At each time step, this begins as a vector of `1`s **that should be progressively updated by each likelihood function**.
#' @param .obs,.t (optional) The `.obs` [`data.table`] (from [`pf_forward()`]) and an `integer` that defines the time step (used to index `.obs`).
#' @param .dlist (optional) The `.dlist` `list` (from [`pf_forward()`]).
#' * For [`acs_filter_land()`], `.dlist` should contain `.dlist$spatial$bathy` and `.dlist$pars$spatna`.
#' * For [`acs_filter_container()`], `.dlist` should contain `.dlist$data$moorings` and `.dlist$pars$lonlat`.
#' * For [`pf_lik_ac()`], `.dlist` should contain `.dlist$algorithm$detection_overlaps` (from [`acs_setup_detection_overlaps()`]) and `.dlist$algorithm$detection_kernels` (from [`acs_setup_detection_kernels()`].
#' * For [`pf_lik_dc()`], `.dlist` should contain `.dlist$spatial$bathy`.
#' * For custom likelihood functions, `.dlist` may require other datasets.
#' @param .drop A `logical` variable that defines whether or not to drop `.particles` rows with zero likelihood.
#'
#' @details
#' In [`pf_forward()`], the likelihood of the data given proposal locations is evaluated using a named `list` of user-defined functions (one for each dataset). Each function must accept a [`data.table`] of proposal locations (`.particles`) alongside the arguments named above (even if they are ignored), evaluate the likelihood of the data and return an updated [`data.table`] for the subset of valid proposals and _updated_ likelihoods (in the `lik` column). For computational efficiency, it is desirable that functions are ordered by the extent to which they invalidate proposal locations and that each function drops invalid proposals (since this reduces the number of subsequent likelihood calculations). For faster evaluations, it might also pay to group likelihood terms under a single wrapper function (since this eliminates the need to loop over individual terms in [`.pf_lik()`]).
#'
#' The following convenience functions are provided:
#' * [`acs_filter_land()`] is a binary 'habitability' (land/water) filter. This is useful when the 'stochastic kick' methodology is used to generate proposal locations in systems that include inhospitable habitats. The function calculates the likelihood (0, 1) of the 'hospitable' data given sampled particles. Location proposals in `NA` cells on the bathymetry grid (`.dlist$spatial$bathy`) are dropped.
#' * [`acs_filter_container()`] is recommended for acoustic time series. This function calculates the approximate likelihood of the next acoustic detection, given particle proposals. This is a binary filter that excludes location proposals that are incompatible with acoustic container dynamics, since proposal locations must be within a moveable distance of receiver locations. Note that receivers by default are defined on a grid and the mobility term should account for this if required (see [`check_moorings()`]). [`acs_filter_container()`] is strictly optional but facilitates algorithm convergence by filtering location proposals that are inconsistent with the location of the next detection.
#' * [`pf_lik_ac()`] calculates the likelihood of acoustic data (detection or non detection at each operational receiver), given location proposals. Likelihood evaluations are implemented on a grid using precomputed fields for speed (see [`acs_setup_detection_kernels()`]).
#' * [`pf_lik_dc()`] calculates the likelihood of archival (depth) data, given particle proposals. This is based on Lavender et al.'s (2023) depth-contour algorithm in which depth observations are considered valid in locations whose depth's lie between a shallow and deep limit (`.obs$depth_shallow[.t]` and `.obs$depth_deep[.t]`) but impossible otherwise. The scaling factor (`.obs$depth_deep[.t] - .obs$depth_shallow[.t]`) must be specified in `.obs$depth_scale[.t]`). This evaluation is necessarily implemented on a grid.
#'
#' In [`pf_forward()`], [`acs_filter_container()`] and [`pf_lik_ac()`] effectively replace the role of [`flapper::ac()`](https://edwardlavender.github.io/flapper/reference/ac.html) function. [`pf_lik_dc()`] effectively replaces the role of [`flapper::dc()`](https://edwardlavender.github.io/flapper/reference/dc.html).
#'
#' @return Functions return the `.particles` [`data.table`] for valid proposal locations and updated likelihoods (in the `lik` column).
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
    .particles <-
      .particles |>
      .pf_bathy(.dlist = .dlist) |>
      mutate(lik = .data$lik * ((!is.na(.data$bathy)) + 0L)) |>
      .pf_lik_drop(.drop = .drop)
  }
  .particles
}

#' @rdname pf_lik
#' @export

# Eliminate particles incompatible with container dynamics
acs_filter_container <- function(.particles, .obs, .t, .dlist, .drop) {

  #### Checks
  if (.t == 1L) {
    check_names(.obs, req = c("receiver_id_next", "buffer_future_incl_gamma"))
    check_dlist(.dlist,
               .dataset = "moorings",
               .par = "lonlat")
    rlang::check_installed("Rfast")
  }

  #### Filter
  if (.t > 1 && .t < fnrow(.obs)) {
    # Calculate distances between particle samples & the receivers that recorded the next detection
    dist <- terra::distance(.particles |>
                              select("x_now", "y_now") |>
                              as.matrix(),
                            .dlist$data$moorings |>
                              filter(.data$receiver_id %in% .obs$receiver_id_next[.t][[1]]) |>
                              select("receiver_x", "receiver_y") |>
                              as.data.table() |>
                              as.matrix(),
                            lonlat = .dlist$par$lonlat)
    # Eliminates particles using distance threshold
    # * Particles are always within `mobility` of the past container
    # * Particles are forced to be within (all) future containers
    # * This does not eliminate all impossible locations e.g., due to peninsulas
    # * But it is a quick way of dropping particles
    .particles <-
      .particles |>
      mutate(lik = .data$lik * ((Rfast::rowsums(dist <= .obs$buffer_future_incl_gamma[.t]) == ncol(dist)) + 0L)) |>
      .pf_lik_drop(.drop = .drop)

  }
  .particles
}

#' @rdname pf_lik
#' @export

# Evaluate likelihood of acoustic data
pf_lik_ac <- function(.particles, .obs, .t, .dlist, .drop) {

  #### Checks
  if (.t == 1L) {
    check_names(.obs, c("detection", "receiver_id", "date"))
    check_dlist(.dlist, .algorithm = c("detection_kernels", "detection_overlaps"))
  }

  #### Calculate likelihood _detection_ given position
  lik <- NULL
  if (.obs$detection[.t] == 1L) {
    # (1) Calculate AC weights _given_detection_ at current time step
    # Identify receiver(s) that recorded detections at the selected time step
    detections_current <- .obs$receiver_id[.t][[1]]
    # Identify remaining (active) receivers which did not record a detection (if any)
    absences_current <- .acs_absences(.date = .obs$date[.t],
                                      .detections = detections_current,
                                      .overlaps = .dlist$algorithm$detection_overlaps)
    # Calculate weights
    .particles[, lik := lik * .pf_lik_ac_detection(.particles = .particles,
                                                   .kernels = .dlist$algorithm$detection_kernels,
                                                   .detections = detections_current,
                                                   .absences = absences_current)]

    #### Calculate likelihood given _non detection_
  } else {
    .particles[, lik := lik *
                 terra::extract(
                   .dlist$algorithm$detection_kernels$bkg_inv_surface_by_design[[.dlist$algorithm$detection_kernels$array_design_by_date[[.obs$date[.t]]]]],
                   .particles$cell_now
                 )[, 1]
    ]
  }

  #### Return particles
  .pf_lik_drop(.particles = .particles, .drop = .drop)

}

#' @rdname pf_lik
#' @export

# Evaluate particles incompatible with the depth data
pf_lik_dc <- function(.particles, .obs, .t, .dlist, .drop) {
  # Checks
  if (.t == 1L) {
    check_names(.obs, req = c("depth_shallow", "depth_deep", "depth_scale"))
    check_dlist(.dlist = .dlist, .spatial = "bathy")
  }
  # Likelihood evaluation
  .particles |>
    .pf_bathy(.dlist = .dlist) |>
    mutate(lik =
             .data$lik *
             .obs$depth_scale[.t] *
             ((.particles$bathy >= .obs$depth_shallow[.t] &
                 .particles$bathy <= .obs$depth_deep[.t]) + 0L)) |>
    .pf_lik_drop(.drop = .drop)
}
