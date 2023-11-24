#' @title AC* helper: AC* filters
#' @description These functions filter particles according to AC* criteria in [`pf_forward_2()`].
#' @param .particles A [`data.table`] that defines particle locations. This should contain two columns named `x_now` and `y_now`.
#' @param .moorings A [`data.table`] of receiver IDs and locations.
#' @param .receivers An `integer` vector of receiver(s) at which the individual was next detected, used to filter `.moorings`.
#' @param .threshold A `numeric` value that defines the distance from `.receivers` within which valid particles are located.
#' @param .bathy The bathymetry [`SpatRaster`]. `NAs` are taken to define inhospitable habitats (e.g., on land).
#'
#' @return The functions return the `.particles` [`data.table`] with the subset of particles that passed the filtering criteria.
#'
#' @author Edward Lavender
#' @name ac_filter

#' @rdname ac_filter
#' @keywords internal

.acs_filter_by_land <- function(.particles, .bathy) {
  .particles |>
    filter(!is.na(terra::extract(.bathy, cbind(.data$x_now, .data$y_now))[, 1])) |>
    as.data.table()
}

#' @rdname ac_filter
#' @keywords internal

.acs_filter_by_container <- function(.particles, .moorings, .receivers, .threshold) {
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
  .particles[Rfast::rowsums(dist <= .threshold) == ncol(dist), ]
}
