# Assemble acoustic containers forwards or backwards in time
.assemble_acoustics_containers <- function(.timeline,
                                           .acoustics,
                                           .mobility,
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
  # - receiver_x, receiver_y, radius (ModelObsAcousticContainer parameters)
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
    select("timestamp", "obs", "sensor_id", "receiver_x", "receiver_y", "radius") |>
    filter(!is.na(.data$sensor_id) & !is.na(.data$radius)) |>
    # For speed, we only implement acoustic containers
    # ... when the distance an individual must be from a receiver is < .threshold
    # ... (e.g., threshold may be the size of the study area)
    filter(.data$radius < .threshold) |>
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
