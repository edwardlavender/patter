#' @title PF: calculate particle likelihoods
#' @description
#' @param .particles
#' @param .obs,.t,.bathy
#' @param .is_land For [`acs_filter_land`],
#' @param .moorings For [`acs_filter_container`],
#' @param .detection_overlaps,.detection_kernels, For [`pf_lik_ac`]
#' @param .update_ac For [`pf_lik_update`]
#'
#' @details
#' TO DO
#' provide likelihood so you can stack them how you like e.g. filters first then DC or AC
# land filter
# AC* filter
# AC weights
# .update_ac weights
#'each function should accept particles, return filtered particles & likelihood column
#'
#' @return
#'
#' @author Edward Lavender
#' @name pf_lik

#' @rdname pf_lik
#' @keywords internal

acs_filter_land <- function(.particles, .bathy) {
  .particles |>
    filter(!is.na(terra::extract(.bathy, .data$cell_now))[, 1]) |>
    as.data.table()
}

#' @rdname pf_lik
#' @keywords internal

acs_filter_container <- function(.particles, .moorings, .receivers, .threshold) {
  # Calculate distances between particle samples & the receivers that recorded the next detection
  dist <- terra::distance(.particles |>
                            select("x_now", "y_now") |>
                            as.matrix(),
                          .moorings |>
                            filter(.data$receiver_id %in% .receivers) |>
                            select("receiver_x", "receiver_y") |>
                            as.data.table() |>
                            as.matrix(),
                          lonlat = FALSE)
  # Eliminates particles using distance threshold
  # * Particles are always within `mobility` of the past container
  # * Particles are forced to be within (all) future containers
  # * This does not eliminate all impossible locations e.g., due to peninsulas
  # * But it is a quick way of dropping particles
  .particles[Rfast::rowsums(dist <= .threshold) == ncol(dist), ]
}

#' @rdname pf_lik
#' @keywords internal

pf_lik_ac <- function(.particles, .obs, .t, .detection_overlaps, .detection_kernels) {

  #### Calculate likelihood _given detection_
  lik <- NULL
  if (.obs$detection[.t] == 1) {
    # (1) Calculate AC weights _given_detection_ at current time step
    # Identify receiver(s) that recorded detections at the selected time step
    detections_current <- .obs$receiver_id[.t][[1]]
    # Identify remaining (active) receivers which did not record a detection (if any)
    absences_current <- .acs_absences(.obs$date[.t], detections_current, .detection_overlaps)
    # Calculate weights
    .particles[, lik := .acs_given_detection_particles(detections_current, absences_current, .detection_kernels, .particles)]

    #### Calculate likelihood _given non-detection_ at current time step
  } else {
    .particles[, lik :=
                 terra::extract(
                   .detection_kernels$bkg_inv_surface_by_design[[.detection_kernels$array_design_by_date[[.obs$date[t]]]]],
                   .particles$cell_now
                 )[, 1]
    ]
  }

  #### Return particles
  .particles

}

#' @rdname pf_lik
#' @keywords internal

pf_lik_update <- function(.particles, .obs, .t, .bathy, .update_ac) {
  lik <- matrix(NA, nrow(.particles), ncol = length(.update_ac) + 1)
  lik[, 1] <- .particles$weight
  update_ac_index <- seq_len(length(.update_ac))
  for (i in update_ac_index) {
    lik[, i + 1] <- .update_ac(.particles = .particles,
                               .obs = .obs, .t = .t,
                               .bathy = .bathy)
  }
  .particles[, lik := colProds.matrix(lik)]
}

#' @rdname pf_lik
#' @keywords internal

pf_lik <- function(.particles, .obs, .t, .bathy,
                   .is_land,
                   .moorings,
                   .detection_overlaps, .detection_kernels,
                   .update_ac
) {

  #### Set up
  diagnostics <- list()
  lik <- NULL
  .particles[, lik := 1]

  #### Land filter
  # Eliminate particles in inhospitable locations (e.g., on land)
  if (.is_land) {
    .particles <- acs_filter_land(.particles, .bathy)
    diagnostics[["acs_filter_land"]] <-
      .pf_diag(.particles, .t = .t, .label = "acs_filter_land")
  }

  #### (2) AC* likelihood
  if (!is.null(.moorings) && .pf_diag_any(.particles)) {

    ## (A) Container filter
    # Eliminate particles incompatible with container dynamics
    if (.t > 1 && .t < max(.obs$timestep)) {
      .particles <- acs_filter_container(.particles = .particles,
                                         .moorings = .moorings,
                                         .receivers = .obs$receiver_id_next[.t][[1]],
                                         .threshold = .obs$buffer_future_incl_gamma[.t])
      diagnostics[["acs_filter_container"]] <-
        .pf_diag(.particles, .t = .t, .label = "acs_filter_container")
    }

    ## (B) Likelihood
    if (.pf_diag_any(.particles)) {
      .particles <- pf_lik_ac(.particles,
                              .obs, .t,
                              .detection_overlaps,
                              .detection_kernels)
      diagnostics[["pf_lik_ac"]] <-
        .pf_diag(.particles, .t = .t, .label = "pf_lik_ac")

    }
  }

  #### (3) Update AC weights using user-defined model(s)
  if (!is.null(.update_ac) && .pf_diag_any(.particles)) {
    .particles <- pf_lik_update(.particles,
                                .obs, .t,
                                .bathy,
                                .update_ac)
    diagnostics[["update_ac"]] <-
      .pf_diag(.particles, .t = .t, .label = "update_ac")
  }

  ## Return outputs
  attr(.particles, "diagnostics") <- diagnostics
  .particles

}
