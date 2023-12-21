#' @title Study metrics: receiver deployments
#'
#' @description This function creates a matrix that, for each time step (matrix row) in a sequence of user-defined times, defines whether or not each receiver (matrix column) was active, optionally accounting for servicing dates. The timeline is defined from a user-provided `start` and `end` date (or the range of deployment times in the input data) and an interval (`.delta_t`) parameter that defines the duration between successive time steps.
#'
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$data$moorings`, with the following columns: `receiver_id`, `receiver_start`, and `receiver_end`.
#' *  (optional) `.dlist$data$services`, with the following columns: `receiver_id`, `service_start` and `service_end`.
#' @param .start,.end `Date` or `POSIXct` objects that define the start and end time. If unspecified, these are taken from the range of deployment times in `.moorings`.
#' @param .delta_t A `numeric` or `character` input that defines the time interval between successive time steps. This is passed to the `by` argument of [`base::seq.POSIXt()`] or [`base::seq.Date()`] (depending on `.as_POSIXct`, below).
#' @param .as_POSIXct (optional) A `function` that coerces supplied any supplied times (`.dlist$data$moorings$receiver_start`, `.dlist$data$$moorings$receiver_end`, `.dlist$data$services$service_start`, `.dlist$data$services$service_end`, `start` and `end`) that are not `POSIXct` objects to `POSIXct` objects. This can be suppressed via `.as_POSIXct = NULL` if supplied times are `Date` objects and `.delta_t` >= one day.
#' @param .set_names A `logical` variable that defines whether or not to set the row and column names of the matrix to the time steps and the receiver IDs respectively.
#'
#' @return The function returns a [`matrix`] with one row for each time step and one column for each receiver. Each cell defines whether (1) or not (0) each receiver was at active during that time step. A `bins` attribute is included, which defines the time steps as a `Date` or `POSIXct` vector.
#'
#' @source This function is based on the [`make_matrix_receivers`](https://edwardlavender.github.io/flapper/reference/make_matrix_receivers.html) function in the [`flapper`](https://github.com/edwardlavender/flapper) package.
#'
#' @seealso This function is used internally in [`acs_setup_detection_kernels()`].
#'
#' @author Edward Lavender
#' @keywords internal

make_matrix_receivers <- function(.dlist,
                                  .start = NULL,
                                  .end = NULL,
                                  .delta_t = "days",
                                  .as_POSIXct = NULL,
                                  .set_names = TRUE) {

  #### Check user inputs
  check_dlist(.dlist = .dlist, .dataset = "moorings")
  moorings <- .dlist$data$moorings
  services <- .dlist$data$services
  # Convert inputs to data.frames
  # * This avoids a subsetting issue with data.table & interval columns below
  # * (https://github.com/Rdatatable/data.table/issues/4315)
  moorings <- as.data.frame(moorings)
  if (!is.null(services)) {
    services <- as.data.frame(services)
  }

  #### Define start/end times
  if (is.null(.start)) {
    .start <- min(moorings$receiver_start)
  }
  if (is.null(.end)) {
    .end <- max(moorings$receiver_end)
  }

  #### Define POSIXct times
  if (!is.null(.as_POSIXct)) {
    if (!inherits(moorings$receiver_start, "POSIXct")) {
      moorings$receiver_start <- .as_POSIXct(moorings$receiver_start)
    }
    if (!inherits(moorings$receiver_end, "POSIXct")) {
      moorings$receiver_end <- .as_POSIXct(moorings$receiver_end)
    }
    if (!inherits(.start, "POSIXct")) {
      .start <- .as_POSIXct(.start)
    }
    if (!inherits(.end, "POSIXct")) {
      .end <- .as_POSIXct(.end)
    }
    if (!is.null(services)) {
      if (!inherits(services$service_start, "POSIXct")) {
        services$service_start <- .as_POSIXct(services$service_start)
      }
      if (!inherits(services$service_end, "POSIXct")) {
        services$service_end <- .as_POSIXct(services$service_end)
      }
    }
  }

  #### Define operational intervals
  moorings$int <- lubridate::interval(moorings$receiver_start, moorings$receiver_end)
  if (!is.null(services)) {
    services$int <- lubridate::interval(services$service_start, services$service_end)
  }

  #### Define status matrix
  # Define bins
  bins <- seq(.start, .end, by = .delta_t)
  # Loop over each receiver
  mat <- lapply(split(moorings, seq_len(nrow(moorings))), function(m) {
    # Define bins within the deployment window
    active <- bins[(bins %within% m$int)]
    # Exclude bins when the receiver was serviced
    if (!is.null(services)) {
      s <- services[which(services$receiver_id == m$receiver_id), ]
      if (nrow(s) > 0L) {
        s_int <- lapply(split(s, seq_len(nrow(s))), function(.s) {
          bins[bins %within% .s$int]
        })
        active <- active[!(active %in% do.call(c, s_int))]
      }
    }
    # Collect status (0, 1)
    status <- rep(0, length(bins))
    status[bins %in% active] <- 1
    status
    })
  mat <- do.call(cbind, mat)
  attr(mat, "bins") <- bins

  #### Define names
  if (.set_names) {
    rownames(mat) <- as.character(bins)
    colnames(mat) <- as.character(moorings$receiver_id)
  }

  #### Return matrix
  mat
}
