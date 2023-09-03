#' @title Set up movement datasets
#' @description TO DO
#' @param .acoustics A [`data.table`]
#' @param .archival A [`data.table`]
#' @param .step An character
#' @param .mobility A number that defines ...

#' @return The function returns a [`data.table`].
#'
#' @examples
#'
#' @author Edward Lavender
#' @export

acs_setup_obs <- function(.acoustics, .archival = NULL, .step, .mobility) {

  #### Check user inputs
  check_acoustics(.acoustics)
  check_archival(.archival)

  #### Assumptions
  # * TO DO
  # * Improve checks in due course
  # * .archival time series are spaced step apart (important for alignment)

  #### Process acoustics
  # * Round time series & drop duplicates
  # * List receivers with detections
  .acoustics <-
    .acoustics |>
    lazy_dt(immutable = TRUE) |>
    # Round time series & drop duplicates
    mutate(timestamp = lubridate::round_date(.data$timestamp, .step)) |>
    group_by(.data$receiver_id, .data$timestamp) |>
    slice(1L) |>
    ungroup() |>
    # List receivers with detections at each time step
    group_by(.data$timestamp) |>
    summarise(receiver_id = list(unique(.data$receiver_id))) |>
    ungroup() |>
    arrange(timestamp) |>
    mutate(detection_id = dplyr::row_number()) |>
    as.data.table()

  #### Align time series
  if (!is.null(.archival)) {
    .acoustics <-
      .acoustics |>
      filter(.data$timestamp >= min(.archival$timestamp) &
               .data$timestamp <= max(.archival$timestamp))
    .archival <-
      .archival |>
      filter(.data$timestamp >= min(.acoustics$timestamp) &
               .data$timestamp <= max(.acoustics$timestamp))
  }

  #### Define output time series with acoustic & archival data
  # Define regular time series
  out <- data.table(timestamp = seq(min(.acoustics$timestamp), max(.acoustics$timestamp), by = .step))
  # Add acoustic data
  detection <- NULL
  out[, detection := (timestamp %in% .acoustics$timestamp) + 0]
  out <- merge(out, .acoustics, all.x = TRUE, by = "timestamp")
  # Add archival data
  if (!is.null(.archival)) {
    depth <- timestamp <- NULL
    out[, depth := .archival$depth[match(timestamp, .archival$timestamp)]]
  }

  #### Tidy outputs & return
  out |>
    lazy_dt(immutable = TRUE) |>
    mutate(detection_id = data.table::nafill(.data$detection_id, type = "locf")) |>
    group_by(.data$detection_id) |>
    mutate(step_forwards = dplyr::row_number(),
           step_backwards = rev(.data$step_forwards),
           buffer_1 = .mobility * .data$step_forwards - .mobility,
           buffer_2 = .mobility * .data$step_backwards) |>
    ungroup() |>
    select(.data$timestamp,
           .data$detection_id, .data$detection, .data$receiver_id,
           .data$step_forwards, .data$step_backwards,
           .data$buffer_1, .data$buffer_2,
           .data$depth) |>
    arrange(.data$timestamp) |>
    mutate(timestep = dplyr::row_number()) |>
    as.data.table()

}

