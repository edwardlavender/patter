#' @title Matricise receiver deployment time series
#' @importFrom lubridate `%within%`
#'
#' @description This function creates a matrix that, for each time step (matrix row) in a sequence of user-defined times, defines whether or not each receiver (matrix column) was active. To implement the function, a dataframe with receiver IDs and deployment start and end times must be supplied (via `.moorings`). Servicing dates can also be accounted for via a dataframe with receiver IDs and servicing times (`.services`). The times for which to express whether or not each receiver was active are provided by optionally defining a `start` and `end` date (these can be taken from the range of deployment times in `.moorings` if unspecified) and the interval (`delta_t`) between time steps.
#'
#' @param .moorings A dataframe that defines receiver IDs and deployment times. This must contain the following columns: an identifier for receivers (named `receiver_id'), the start time of receiver' deployment periods (`receiver_start') and the end time of receivers' deployment periods (`receiver_end') (see [flapper::dat_moorings()] for an example). Deployment times can be recorded as Date or POSIXct objects.
#' @param .services (optional) A dataframe that defines receiver IDs and servicing dates (times during the deployment period of a receiver when it was not active due to servicing). If provided, this must contain the following columns: an identifier for serviced receivers (named `receiver_id') and two columns that define the time of the service(s) (`service_start' and `service_end'). Times can be recorded as Date or POSIXct objects. Before/after service events, receivers are assumed to have been deployed in the same locations; receiver deployments in different locations before/after servicing should be treated as distinct deployments in `.moorings`.
#' @param .start,.end Date or POSIXct objects that define the start and end time. If unspecified, these are taken from the range of deployment times in `.moorings`.
#' @param .delta_t A number or character that defines the time interval between successive time steps. This is passed to the `by' argument of [base::seq.POSIXt()] or [base::seq.Date()] (depending on `as_POSIXct`, below).
#' @param .as_POSIXct (optional) A function that coerces supplied any supplied times (`.moorings$receiver_start`, `.moorings$receiver_end`, `.services$service_start`, `.services$service_end`, `start` and `end`) that are not POSIXct objects to POSIXct objects. This can be suppressed via `as_POSIXct = NULL` if supplied times are Date objects and `delta_t` is not less than one day.
#' @param .set_names A logical variable that defines whether or not to set the row and column names of the matrix to the time steps and the receiver IDs respectively.
#'
#' @return The function returns a matrix with one row for each time step and one column for each receiver. Each cell defines whether (1) or not (0) each receiver was at active during that time step. A `bins' attribute is included, which defines the time steps as a Date or POSIXct vector.
#'
#' @examples
#' #### Example (1): Illustration using fake data
#'
#' ## Define some example 'moorings' data
#' # ... with receiver IDs and deployment times
#' require(data.table)
#' moorings <- data.table(
#'   receiver_id = c(1, 2, 3, 4, 5),
#'   receiver_start = as.Date(c(
#'     "2016-01-01",
#'     "2016-01-02",
#'     "2016-01-03",
#'     "2016-01-04",
#'     "2016-01-05"
#'   )),
#'   receiver_end = as.Date(c(
#'     "2016-01-06",
#'     "2016-01-07",
#'     "2016-01-08",
#'     "2016-01-09",
#'     "2016-01-09"
#'   ))
#' )
#'
#' ## Define some example 'servicing' data
#' # ... with receiver IDs and servicing times
#' # ... Here, receiver 1 was serviced twice
#' # ... ... from 2016-01-02--3 and 2016-01-04--5
#' # ... and receiver 5 was serviced
#' # ... ... on 2016-01-08.
#' services <- data.table(
#'   receiver_id = c(1, 1, 5),
#'   service_start = as.Date(c(
#'     "2016-01-02",
#'     "2016-01-04",
#'     "2016-01-08"
#'   )),
#'   service_end = as.Date(c(
#'     "2016-01-03",
#'     "2016-01-05",
#'     "2016-01-08"
#'   ))
#' )
#'
#' ## Get daily receiver status (0, 1) matrix
#' make_matrix_receivers(moorings, .delta_t = "days", .as_POSIXct = NULL)
#'
#' ## Get daily receiver status (0, 1) matrix
#' # ... accounting for servicing dates
#' make_matrix_receivers(moorings, services, .delta_t = "days", .as_POSIXct = NULL)
#'
#' #### Example (2): Illustration using actual data
#' # ... for different time windows
#' mat_days  <- make_matrix_receivers(dat_moorings, .delta_t = "days", .as_POSIXct = NULL)
#' mat_hours <- make_matrix_receivers(dat_moorings, .delta_t = "hours")
#' utils::str(mat_days)
#' utils::str(mat_hours)
#'
#' @author Edward Lavender
#' @export

make_matrix_receivers <- function(.moorings,
                                  .services = NULL,
                                  .start = NULL,
                                  .end = NULL,
                                  .delta_t = "120 mins",
                                  .as_POSIXct = as.POSIXct,
                                  .set_names = TRUE) {
  #### Check user inputs
  .moorings <- check_moorings(.moorings)
  .services <- check_services(.services, .moorings)

  #### Process dates, if necessary
  if (is.null(.start)) {
    .start <- min(.moorings$receiver_start, na.rm = TRUE)
  }
  if (is.null(.end)) {
    .end <- max(.moorings$receiver_end, na.rm = TRUE)
  }
  if (!is.null(.as_POSIXct)) {
    if (!inherits(.moorings$receiver_start, "POSIXct")) {
      .moorings$receiver_start <- .as_POSIXct(.moorings$receiver_start)
    }
    if (!inherits(.moorings$receiver_end, "POSIXct")) {
      .moorings$receiver_end <- .as_POSIXct(.moorings$receiver_end)
    }
    if (!inherits(.start, "POSIXct")) {
      .start <- .as_POSIXct(.start)
    }
    if (!inherits(.end, "POSIXct")) {
      .end <- .as_POSIXct(.end)
    }
    if (!is.null(.services)) {
      if (!inherits(.services$service_start, "POSIXct")) {
        .services$service_start <- .as_POSIXct(.services$service_start)
      }
      if (!inherits(.services$service_end, "POSIXct")) {
        .services$service_end <- .as_POSIXct(.services$service_end)
      }
    }
  }

  #### Define time steps over which to consider operations and define operational intervals
  # Define time steps over which to check operational status
  bin <- seq(.start, .end, .delta_t)
  # Define intervals of .moorings deployment
  .moorings$interval <- lubridate::interval(.moorings$receiver_start, .moorings$receiver_end)
  # Define a corresponding list, in the same order as receivers in .moorings, with servicing intervals
  if (!is.null(.services)) {
    .services$interval <- lubridate::interval(.services$service_start, .services$service_end)
    services_ls <- lapply(split(.moorings, 1:nrow(.moorings)), function(d) {
      out <- NULL
      if (d$receiver_id %in% .services$receiver_id) {
        out <- .services[which(.services$receiver_id %in% d$receiver_id), ]
      }
      return(out)
    })
  }

  #### Define matrix of activity status (0, 1)
  # Blank matrix
  mat <- matrix(NA, nrow = length(bin), ncol = length(unique(.moorings$receiver_id)))
  attr(mat, "bins") <- bin
  # Fill matrix
  for (j in 1:ncol(mat)) {
    # Fill matrix with 0,1 according to the overlap between bins and the deployment interval
    mat[, j] <- (bin %within% .moorings$interval[j]) + 0
    # Post-hoc adjustment to suppress any time steps (bins) during this interval when receivers were being serviced)
    if (!is.null(.services)) {
      if (!is.null(services_ls[[j]])) {
        services_for_j <- services_ls[[j]]
        for (k in 1:nrow(services_for_j)) {
          mat[(bin %within% services_for_j$interval[k]), j] <- 0
        }
      }
    }
  }
  # Define matrix names
  if (.set_names) {
    rownames(mat) <- as.character(bin)
    colnames(mat) <- as.character(.moorings$receiver_id)
  }

  #### Return outputs
  return(mat)
}
