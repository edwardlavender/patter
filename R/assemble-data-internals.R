# Assemble capture/recapture containers forwards/backwards in time
.assemble_xinit_containers <- function(.timeline,
                                        .xinit = list(),
                                        .radius,
                                        .mobility,
                                        .map = NULL,
                                        .threshold = NULL,
                                        .direction = c("forward", "backward")) {

  # Define direction
  .direction <- match.arg(.direction)

  # Define container information for the 'ending' coordinate
  # * If direction = "forward", .xinit$backward defines the container coordinates
  # * If direction = "backward", .xinit$forward defines the container coordinates
  if (.direction == "forward") {
    cinfo <- .xinit$backward
  } else if (.direction == "backward") {
    cinfo <- .xinit$forward
  }
  if (is.null(cinfo)) {
    return(NULL)
  }

  # Define container coordinates
  # * cx, cy are container coordinates
  # * radius is container radius
  if (nrow(cinfo) == 1L) {
    cx     <- cinfo$x
    cy     <- cinfo$y
    radius <- .radius
  } else {
    # Use the central point if multiple points provided
    # radius is defined as the maximum distance from the centre to a point
    # (plus the .radius buffer)
    cx     <- mean(cinfo$x)
    cy     <- mean(cinfo$y)
    radius <- max(dist_2d(cbind(cx, cy), cbind(cinfo$x, cinfo$y, pairwise = TRUE)))
    radius <- radius + .radius
  }

  # Define sequence of radii
  # For .direction = "forward", radii shrink _forward_ in time
  # For .direction = "backward", radii shrink _backward_ in time
  # radii[1] or radii[T] = radius
  radii <- radius + (1:length(.timeline) - 1) * .mobility
  if (.direction == "forward") {
    radii <- rev(radii)
  }

  # Build containers data.table
  # * Use sensor_id = 0L
  # * (This shouldn't be confused for a receiver id (1, ..., n_receiver))
  containers <-
    data.table(timestamp  = .timeline,
               obs        = 1L,
               sensor_id  = 0L,
               centroid_x = cx,
               centroid_y = cy,
               radius     = radii) |>
    filter_containers(.map = .map, .threshold = .threshold)

  # Return containers
  containers
}

# Assemble acoustic containers forwards/backwards in time
.assemble_acoustics_containers <- function(.timeline,
                                           .acoustics,
                                           .mobility,
                                           .map,
                                           .threshold,
                                           .direction = c("forward", "backward")) {

  # Handle inputs
  rlang::check_installed("tidyr")
  acoustics  <- copy(.acoustics)
  .direction <- match.arg(.direction)

  # Define the timeline
  # * If .direction == "forward", time_index runs from 1:T
  # * If .direction == "backward", time_index runs from T:1
  # * Using time index to sort the time series sorts the data forwards/backwards in time
  # * We can use the same code to define acoustic containers for the forward/backward filter
  # * (On the forward run, the 'next' detection should reflect the next detection)
  # * (On the backward run, the 'next' detection should be the previous detection)
  timeline   <- data.table(timestamp = .timeline)
  if (.direction == "forward") {
    timeline <-
      timeline |>
      mutate(time_index = dense_rank(.data$timestamp)) |>
      as.data.table()
  } else if (.direction == "backward") {
    timeline <-
      timeline |>
      mutate(time_index = dense_rank(desc(.data$timestamp))) |>
      as.data.table()
  }

  # Define detections
  # * We use the processed acoustics data.table for safety
  # * (e.g., b/c timestamps have been processed)
  detections <-
    acoustics |>
    # Focus on detections
    filter(.data$obs == 1L) |>
    # List unique receivers & associated parameters w/ detections at time step
    # * The data are nested in a 'data' column
    # * We drop the obs column to avoid duplicate copies of this column and issues with unnest(), below
    mutate(obs = NULL) |>
    group_by(.data$timestamp) |>
    filter(!duplicated(.data$sensor_id)) |>
    tidyr::nest() |>
    ungroup() |>
    arrange(.data$timestamp) |>
    # Define detection_id(s)
    mutate(detection_id = as.integer(row_number())) |>
    select("timestamp", "detection_id", "data") |>
    as.data.table()

  # Define a regular timeline of the sensors that recorded detections
  # * For each time stamp, we have:
  # - A list of receivers (and associated parameters) that recorded detection(s) at that time stamp
  # - A list of receivers (and associated parameters) that recorded the next detection(s)
  # - max_dist_mobility, which defines the max moveable distance of the individual from those receivers
  acoustics <-
    # Add the list of sensor_id(s) and detection_id(s)
    # * time stamps w/o detections are assigned detection_id = NA
    timeline |>
    merge(detections, all.x = TRUE, by = "timestamp") |>
    # Carry the last detection_id forward, so if:
    # ... there are no detections at a given time step (detection_id = NA)
    # ... we carry forward the detection_id from the last time step
    # ... (i.e., that group of time steps belongs to the same detection)
    # ... This is required to define max_dist_mobility, below.
    arrange(.data$time_index) |>
    mutate(detection_id = as.integer(nafill(.data$detection_id, type = "locf"))) |>
    # The individual may be within receiver_gamma + (.mobility * time steps) from the next sensor
    group_by(.data$detection_id) |>
    arrange(.data$time_index, .by_group = TRUE) |>
    mutate(max_dist_mobility = .mobility * rev(row_number())) |>
    ungroup() |>
    # At each time step, list the receivers & associated data that recorded the 'next' detection
    # * For forward filter runs, the 'next' detection is the next detection
    # * For backward filter runs, the 'next' detection is the previous detection
    arrange(.data$time_index) |>
    mutate(data_next = list_data_next(.data$data)) |>
    as.data.table()

  # Define acoustic containers dataset
  # * The contains the following columns:
  # - timestamp (required)
  # - sensor_id (required)
  # - obs (nominally 1, unused)
  # - centroid_x, centroid_y, radius
  containers <-
    acoustics |>
    # Define obs (nominally 1, unused)
    mutate(obs = 1L) |>
    select("timestamp", "obs", data = "data_next", "max_dist_mobility") |>
    # Unnest data column
    # * For each time step, we now have:
    # * The sensor_id for the next detection
    # * Corresponding receiver parameters for that moment in time
    # * The last row of acoustics, which as data = list(NULL) is automatically dropped
    # * (An acoustic container cannot be defined for the final time step)
    tidyr::unnest(cols = "data") |>
    arrange(.data$timestamp, .data$sensor_id) |>
    # Compute the radius of the acoustic container
    # * This is the max distance of the individual from the receiver(s)
    # * ... that recorded the next detection, based on:
    # * ... a) receiver- and time-specific detection range
    # * ... b) the maximum moveable distance in the time before the next detection
    mutate(radius = .data$receiver_gamma + .data$max_dist_mobility) |>
    select("timestamp",
           "obs",
           "sensor_id",
           centroid_x = "receiver_x", centroid_y = "receiver_y", "radius") |>
    filter(!is.na(.data$sensor_id) & !is.na(.data$radius)) |>
    # Filter containers
    filter_containers(.map = .map, .threshold = .threshold) |>
    as.data.table()

  # Return containers
  containers

}

# For each time step, list the receivers and associated parameters for the next detection
# * .anest is the processed acoustic data defined in .assemble_acoustics_containers()
# * .anest.data is a the column of dataframes
list_data_next <- function(.anest.data) {

  rlang::check_installed("zoo")

  # For time steps without a detection, assign NA
  # * We will define the receivers that recorded the next detection below
  # * For time steps missing a detection, we assign NA here
  # * Below, where we have NAs, we can use 'next observation carried backward'
  # * to fill those NAs with the data for the next time step
  data   <- NULL
  .anest <- data.table(data = .anest.data)
  .anest[sapply(.anest$data, is.null), data := list(NA)]

  # Define a data.table that defines, for each time step, the data for the next time step
  out <-
    .anest |>
    mutate(
      # Get the data for the next detection
      data_next = lead(.data$data),
      # For time steps without an immediately following detection, carry data backwards
      # * This means every time step is associated with the data of the next detection
      # * We use zoo here b/c data.table::nafill requires numeric input
      data_next = zoo::na.locf(.data$data_next, fromLast = TRUE, na.rm = FALSE)) |>
    as.data.table()

  # Return the data for the next time step
  # * The last element is list(NULL)
  # * This is automatically dropped by tidyr::unnest() as desired
  out$data_next
}

# Get the maximum value of each row, using Rfast if available
rowMax <- function(.x) {
  if (requireNamespace("Rfast", quietly = TRUE)) {
    Rfast::rowMaxs(.x, value = TRUE)
  } else {
    warn("Install Rfast for faster rowMax() function.")
    apply(.x, 1, max)
  }
}

# Get the boundary box of a .map as a two-columm matrix
map_bbox <- function(.map) {
  # Define bb
  # * This creates 5 points (1, 2, 3, 4, 1) forming a closed polygon
  bb <-
    .map |>
    terra::ext() |>
    terra::vect() |>
    terra::crds()
  # Return the four corner points
  bb[1:4, ]
}

# Filter containers
# * For speed, we only implement containers
# * ... when the distance an individual must be from a point is < .threshold
# * If .bbox is supplied, .threshold is set as the maximum distance
# * ... from each point to the edge of the study area
# * Otherwise, a user-supplied (i.e., smaller) value may be used
filter_containers <- function(.containers, .map, .threshold) {

  # Check inputs
  if (!is.null(.map) & !is.null(.threshold)) {
    .threshold <- NULL
    warn("`.map` is used in place of `.threshold`.")
  }

  # Define .threshold automatically from .map, if unspecified
  if (!is.null(.map)) {
    # (A) Define boundary box
    bb <- .map
    if (inherits(.map, c( "SpatRaster", "SpatVector"))) {
      bb <- map_bbox(.map)
    } else if (!(inherits(.map, "matrix") && nrow(.map) == 4L && ncol(.map) == 2L)) {
      abort("`.map` should be a SpatRaster, SpatVector or 4-row matrix of boundary coordinates.")
    }
    # (B) Define .threshold as maximum distance to boundary box
    .threshold <-
      cbind(.containers$centroid_x, .containers$centroid_y) |>
      dist_2d(bb, pairwise = FALSE) |>
      rowMax()
  }

  # Filter containers by .threshold
  if (!is.null(.threshold)) {
    .containers <- .containers[.containers$radius <= .threshold, ]
  }

  # Return filtered containers
  .containers
}
