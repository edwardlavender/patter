#' @title Simulate an acoustic array
#' @description This function simulates acoustic receivers on a grid.
#' @param .bathy A [`SpatRaster`] that defines the region of interest.
#' @param .arrangement A character that defines the receiver arrangement (passed to the `method` argument of [`terra::spatSample()`]).
#' @param .n_receivers An `integer` that defines the number of receivers (passed to the `size` argument of [`terra::spatSample()`]).
#' @param ... Additional arguments passed to [`terra::spatSample()`].
#' @param .receiver_start,.receiver_end,.receiver_range (optional) Additional columns to include in the output:
#'  * `.receiver_start` and `.receiver_end` specify the deployment time period;
#'  * `.receiver_range` defines the detection range;
#'
#'  Single inputs are expected to these arguments, which are constant across all receivers.
#'
#' @param .n_arrays An `integer` that defines the number of array designs to simulate.
#' @param .plot A `logical` variable that defines whether or not to plot simulated arrays.
#' @param .one_page If `.plot = TRUE`, `.one_page` is a `logical` variable that defines whether or not to produce plots on a single page (see [`par_mf()`]).
#'
#' @details
#' Receiver locations are simulated using [`terra::spatSample()`].
#'
#' @return The function returns a `list`. Each element of the list is a `data.table` with the following columns:
#' * `receiver_id`---an `integer` vector of receiver IDs;
#' * `receiver_easting`---a `numeric` vector that defines receiver x coordinates;
#' * `receiver_northing`---a `numeric` vector that defines receiver y coordinates;
#' * `receiver_start`---receiver start dates (if defined);
#' * `receiver_end`---receiver end dates (if defined);
#' * `receiver_range`---receiver detection ranges (if defined);
#'
#' @examples
#' #### Example (1): The default implementation
#' # The function returns a list, with one element for each simulated array
#' a <- sim_array()
#' summary(a)
#'
#' #### Example (2): Customise receiver placement/number
#' a <- sim_array(.arrangement = "regular", .n_receivers = 100)
#'
#' #### Example (3): Add additional columns for downstream functions
#' a <- sim_array(.receiver_start = as.Date("2013-01-01"),
#'                .receiver_end = as.Date("2014-01-01"),
#'                .receiver_range = 500)
#'
#' #### Example (4): Control the plot(s)
#' sim_array(.plot = FALSE)
#' sim_array(.n_arrays = 5L, .plot = TRUE, .one_page = TRUE)
#' sim_array(.n_arrays = 5L, .plot = TRUE, .one_page = FALSE)
#'
#' @author Edward Lavender
#' @export

sim_array <- function(.bathy = rast_template(),
                      .arrangement = "random", .n_receivers = 10L, ...,
                      .receiver_start = NULL, .receiver_end = NULL, .receiver_range = NULL,
                      .n_arrays = 1L,
                      .plot = TRUE, .one_page = TRUE) {
  #### Check user inputs
  # * Check ... are not implement internally
  # * Check receiver_end is after receiver_start (if supplied)

  #### Simulate arrays
  arrays <-
    lapply(seq_len(.n_arrays), function(i) {
      # Sample receiver locations
      array <-
        .bathy |>
        terra::spatSample(size = .n_receivers,
                          method = .arrangement, replace = FALSE,
                          na.rm = TRUE, xy = TRUE, cells = TRUE, values = FALSE, ...) |>
        as.data.table() |>
        mutate(receiver_id = as.integer(dplyr::row_number())) |>
        select("receiver_id", receiver_easting = "x", receiver_northing = "y") |>
        as.data.table()
      # Add optional columns
      if (!is.null(.receiver_start)) {
        receiver_start <- NULL
        array[, receiver_start := .receiver_start]
      }
      if (!is.null(.receiver_end)) {
        receiver_end <- NULL
        array[, receiver_end := .receiver_end]
      }
      if (!is.null(.receiver_range)) {
        receiver_range <- NULL
        array[, receiver_range := .receiver_range]
      }
      # Return simulated array (moorings data.table)
      array
    })

  #### Plot arrays
  if (.plot) {
    if (.one_page) {
      pp <- graphics::par(mfrow = par_mf(length(arrays)))
      on.exit(graphics::par(pp), add = TRUE)
    }
    lapply(seq_len(length(arrays)), function(i) {
      terra::plot(.bathy, main = paste("Array", i))
      graphics::points(arrays[[i]]$receiver_easting, arrays[[i]]$receiver_northing)
    }) |> invisible()
  }

  #### Return outputs
  arrays
}



