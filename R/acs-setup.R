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


#' @title Set up detection containers
#' @description This function defines receiver detection containers.
#' @param .grid A [`SpatRaster`].
#' @param .moorings A [`data.table`]
#'
#' @details Details
#'
#' @return The function returns a named list.
#'
#' @examples
#' # Define grid
#' grid <- dat_gebco()
#' terra::plot(grid)
#' # Define receiver detection ranges
#' dat_moorings$receiver_range <- 500
#' # Define detection containers
#' containers <- acs_setup_detection_containers(grid, dat_moorings)
#' # Visualise an example container
#' terra::plot(containers[[dat_moorings$receiver_id[1]]])
#' points(dat_moorings$receiver_easting[1], dat_moorings$receiver_northing[1])
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_containers <- function(.grid, .moorings) {

  # TO DO
  # Add checks
  # grid can contain NAs
  # moorings should be data.table contain required columns (receiver_id, receiver_easting, receiver_northing, receiver_range)
  # receiver_ids should be integer from 1 to n
  # receiver-specific ranges permitted
  # For time-step specific ranges, these are computed on the fly in ac()
  check_names(input = .moorings, req = "receiver_range")

  rs <- seq_len(max(.moorings$receiver_id))
  containers <-
    pbapply::pblapply(rs, function(id) {
      out <- NULL
      bool <- .moorings$receiver_id == id
      if (any(bool)) {
        d <- .moorings[which(bool), ]
        g   <- terra::setValues(.grid, NA)
        rxy <- matrix(c(d$receiver_easting, d$receiver_northing), ncol = 2)
        g[terra::cellFromXY(g, rxy)] <- 1
        # terra::plot(g)
        out <- terra::buffer(g, d$receiver_range)
        out <- (terra::mask(out, .grid)) + 0
        # terra::plot(out)
      }
      out
    })
  names(containers) <- rs
  containers
}


#' @title Set up detection container overlaps
#' @description
#'
#' @param .containers A named list
#' @param .moorings A [`data.table`]
#' @param .services A [`data.table`]
#'
#' @details Details.
#'
#' @return The function returns a named list.
#'
#' @examples
#'
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_overlaps <- function(.containers, .moorings, .services = NULL) {

  #### Check user inputs
  rlang::check_installed("tidyr")
  .moorings <- check_moorings(.moorings)
  check_inherits(.moorings$receiver_start, "Date")
  check_inherits(.moorings$receiver_end, "Date")
  if (!is.null(.services)) {
    .services <- check_services(.services, .moorings)
    check_inherits(.services$service_start, "Date")
    check_inherits(.services$service_end, "Date")
  }

  #### Define receiver activity status matrix
  # This defines whether or not each receiver was active on each date (0, 1)
  # We'll start from this point because it accounts for receiver activity status (including servicing).
  # We'll then update this, for each receiver, to define whether or not, if that receiver was active on a given date
  # ... which other receivers (if any) it overlapped in space (and time) with.
  rs_active_mat <- make_matrix_receivers(
    .moorings = .moorings,
    .services = .services,
    .delta_t = "days",
    .as_POSIXct = NULL
  )

  #### Define a list, with one dataframe element per receiver, that defines, for each time step, the overlapping receivers (0, 1)
  list_by_receiver <- pbapply::pblapply(seq_len(length(.containers)), function(i) {

    #### Collect container and receiver status information
    r1      <- names(.containers)[[i]]
    cont_r1 <- .containers[[i]]
    if (is.null(cont_r1)) return(NULL)
    # Define moorings & receiver status matrix
    m    <- .moorings[.moorings$receiver_id == r1, , drop = FALSE]
    info <- rs_active_mat
    info <- info[as.Date(rownames(info)) %within% lubridate::interval(m$receiver_start, m$receiver_end), , drop = FALSE]

    #### Convert receiver 'active' index (0, 1) to 'overlapping' index
    # ... A) Check for overlapping receivers
    # ... B) If there are overlapping receivers,
    # ... ... then we force all dates when the receiver of interest was not active to take 0 (no receivers could overlap with it then)
    # ... ... and we force all receivers that didn't overlap in space to 0
    # ... C) If there are no overlapping receivers, the whole matrix just gets forced to 0

    ## (A) Get an index of the receivers that intersected with the current receiver (in space)
    cont_r1 <- terra::mask(cont_r1, cont_r1 != 1, maskvalue = TRUE, updatevalue = NA)
    int <- lapply(seq_len(length(.containers)), function(j) {
      r2      <- names(.containers)[j]
      cont_r2 <- .containers[[j]]
      .int     <- data.frame(receiver_id = r2, overlap = 0)
      if (!is.null(cont_r2) && r1 != r2) {
        cont_r2 <- terra::mask(cont_r2, cont_r2 != 1, maskvalue = TRUE, updatevalue = NA)
        # terra::plot(check)
        cont_int <- terra::intersect(cont_r1, cont_r2)
        # terra::plot(.int)
        .int$overlap <- as.integer(terra::global(cont_int, "max", na.rm = TRUE))
      }
      .int
    }) |> dplyr::bind_rows()

    ## (B) If there are any overlapping receivers,
    if (any(int$overlap == 1)) {

      ## Process 'overlap' when the receiver was not active
      # ... Any time there is a '0' for activity status of the current receiver (e.g., due to servicing),
      # ... there can be no overlap with that receiver
      # ... so we will set a '0' to all other receivers
      # ... some of which may have been active on that date
      # ... Note the implementation of this step before the step below, when all rows for the receiver
      # ... of interest are (inadvertently) set to 0.
      info[which(info[, as.character(r1)] == 0), ] <- 0

      ## Process 'overlap' for overlapping/non-overlapping receivers
      # For overlapping receivers, we'll leave these as defined in the activity matrix
      # ... (if active, then they overlap;
      # ... if not active, e.g., due to a servicing event for that receiver, then they can't overlap).
      # ... For the non-overlapping receivers, we'll force '0' for the overlap (even if they were active).
      # Get receiver IDs
      overlapping_receivers <- int$receiver_id[int$overlap == 1]
      # For all non-overlapping receivers, set '0' for overlap
      # ... Note that this will include the receiver of interest
      # ... But that doesn't matter because we'll drop that column anyway
      info[, !(colnames(info) %in% overlapping_receivers)] <- 0

      ## (C) If there aren't any spatially overlapping receivers, then the whole matrix just takes on 0
    } else {
      info[] <- 0
    }

    #### Process dataframe
    rnms <- rownames(info)
    info <- data.frame(info)
    colnames(info) <- colnames(rs_active_mat)
    info[, as.character(r1)] <- NULL
    cnms             <- colnames(info)
    info$timestamp   <- as.Date(rnms)
    info$receiver_id <- r1
    info[, c("timestamp", "receiver_id", cnms)]
  })
  names(list_by_receiver) <- names(.containers)
  list_by_receiver        <- compact(list_by_receiver)

  #### On each date, get the vector of overlapping receivers
  # Note that not every receiver in this list will necessarily overlap with every other receiver though.
  lbd <- lapply(list_by_receiver, function(d) {
    tidyr::pivot_longer(
      data = d,
      cols = 3:ncol(d),
      names_to = "receiver_id_2",
      names_transform = list(receiver_id_2 = as.integer)
    )
  })
  lbd <- dplyr::bind_rows(lbd) |> dplyr::filter(.data$value == 1)
  lbd <- lapply(split(lbd, lbd$timestamp), function(d) unique(c(d$receiver_id[1], d$receiver_id_2)))

  ##### Process outputs
  # For the list_by_receiver, we will have one element for each receiver from 1:max(.moorings$receiver_id)
  # ... (for each indexing)
  list_by_receiver <- lapply(as.integer(1:max(.moorings$receiver_id)), function(i) {
    if (i %in% .moorings$receiver_id) {
      return(list_by_receiver[[as.character(i)]])
    } else {
      return(NULL)
    }
  })
  # For the list_by_date (lbd), we will have one element for each date from the start to the end of the array
  days <- as.character(seq(min(.moorings$receiver_start), max(.moorings$receiver_end), "days"))
  list_by_date <- lapply(days, function(day) {
    lbd[[day]]
  })
  names(list_by_date) <- days

  #### Return outputs
  out <- list()
  out$list_by_receiver <- list_by_receiver
  out$list_by_date <- list_by_date
  return(out)


}

