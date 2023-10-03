#' @title COA internals
#' @author Edward Lavender
#' @name coa_check

#' @rdname coa_check
#' @keywords internal

.coa_check_acoustics <- function(.acoustics, .split) {
  # Check class
  if (inherits(.acoustics, "data.frame") & !inherits(.acoustics, "data.table")) {
    .acoustics <- as.data.table(.acoustics)
  }
  check_inherits(.acoustics, "data.table")
  # Check names
  # * receiver_easting/receiver_northing or receiver_lon/receiver_lat
  # ... are automatically checked via acoustics_is_lonlat()
  check_names(.acoustics, c("timestamp", "receiver_id", .split))
  .acoustics
}

#' @rdname coa_check
#' @keywords internal

.coa_check_lonlat <- function(.acoustics) {
  is_utm <- is_lonlat <- FALSE
  if (all(c("receiver_easting", "receiver_northing") %in% colnames(.acoustics))) {
    is_utm <- TRUE
  }
  if (all(c("receiver_lon", "receiver_lat") %in% colnames(.acoustics))) {
    is_lonlat <- TRUE
  }
  if (is_utm & is_lonlat) {
    warn("UTM coordinates used (both UTM and lon/lat coordinates detected).")
  }
  if (!is_utm & !is_lonlat) {
    abort("Neither UTM coordinates (`.acoustics$receiver_easting`, `.acoustics$receiver_northing`) nor lon/lat coordinates (`.acoustics$receiver_lon`, `.acoustics$receiver_lat`) detected. ")
  }
  if (is_utm) {
    is_lonlat <- FALSE
  }
  is_lonlat
}

#' @title The centres of activity (COA) algorithm
#' @description This function calculates centres of activity (COAs).
#' @param .acoustics A [`data.table`] of acoustic detections. At a minimum, this must contain the following columns:
#' * `receiver_id`---a unique identifier of each receiver;
#' * `timestamp`---a time variable that defines the time stamp of detections;
#' * `receiver_easting` and `receiver_northing` (planar coordinates) or `receiver_lon` and `receiver_lat` (longitude/latitude coordinates)---receiver locations;
#' @param .delta_t The time interval over which to calculate COAs. This can be specified in any way understood by [`cut.POSIXt()`] (see the `breaks` argument).
#' @param .split (optional) A `character` that defines the name of the grouping factor in `.acoustics` (e.g., `individual_id` for [`dat_acoustics`]).
#' @param .lonlat (optional) A `logical` variable that defines whether or not to calculate COAs using planar coordinates (`receiver_easting` and `receiver_northing` columns) or longitude/latitude coordinates (`receiver_lon` and `receiver_lat` columns). If unsupplied, this is defined automatically based on the columns in `.acoustics`. `receiver_easting` and `receiver_northing` are used preferentially, if available, unless `.lonlat` is specified. `.lonlat = TRUE` requires the [`geosphere::geomean()`] function.
#' @param .plot_weights A `logical` variable that defines whether or not to plot the frequency distribution of weights for each `.split` value.
#' @param .one_page A `logical` variable that defines whether or not to plot all histograms on one page.
#' @param ... Additional arguments passed to [`graphics::hist()`].
#'
#' @details COAs are calculated as a weighted mean of the locations of receivers at which individuals are detected over consecutive time intervals, weighted by the frequency of detections at each of those receivers.
#'
#' @example man/examples/coa-examples.R
#' @author Edward Lavender
#' @export

coa <- function(.acoustics, .delta_t, .split = NULL, .lonlat = NULL,
                .plot_weights = TRUE, ..., .one_page = TRUE) {

  #### Check user inputs
  .acoustics <- .coa_check_acoustics(.acoustics, .split)
  if (is.null(.lonlat)) {
    .lonlat <- .coa_check_lonlat(.acoustics)
  }
  if (.lonlat) {
    rlang::check_installed("geosphere")
  }
  check_dots_for_missing_period(formals(), list(...))

  #### Identify split column e.g., individual_id
  keep_split <- TRUE
  if (is.null(.split)) {
    keep_split <- FALSE
    .split <- "individual_id"
    individual_id <- NULL
    .acoustics[, individual_id := 1L]
  }

  #### Prepare acoustics for COA calculations
  # * Define split column
  # * Group by split & define bins
  # * Calculate the frequency of detections in each bin
  .acoustics <-
    .acoustics |>
    mutate(split = .acoustics[[.split]]) |>
    group_by(.data$split) |>
    mutate(bin = cut(.data$timestamp, .delta_t)) |>
    ungroup() |>
    group_by(.data$split, .data$bin, .data$receiver_id) |>
    mutate(n = dplyr::n()) |>
    slice(1L) |>
    ungroup() |>
    as.data.table()
  # Plot the frequency distribution of weights
  if (.plot_weights) {
    pp <- graphics::par(mfrow = par_mf(length(unique(.acoustics$split))))
    on.exit(graphics::par(pp), add = TRUE)
    lapply(split(.acoustics, .acoustics$split), function(d) {
      graphics::hist(d$n, main = d$split[1], ...)
    }) |> invisible()
  }

  #### Calculate COAs
  if (.lonlat) {
    # Calculate COAs using geomean() if lonlat
    out <-
      .acoustics |>
      group_by(.data$split, .data$bin) |>
      dplyr::summarise(
        coa_xy = geomean(xy = as.matrix(cbind(x = .data$receiver_lon,
                                              y = .data$receiver_lat),
                                        ncol = 2),
                         w = .data$n),
        coa_x = .data$coa_xy[, 1],
        coa_y = .data$coa_xy[, 2]
      ) |>
      ungroup() |>
      as.data.table()
  } else {
    # Calculate COAs using weighted.mean() if planar
    out <-
      .acoustics |>
      group_by(.data$split, .data$bin) |>
      summarise(coa_x = stats::weighted.mean(.data$receiver_easting, .data$n),
                coa_y = stats::weighted.mean(.data$receiver_northing, .data$n)) |>
      ungroup() |>
      as.data.table()
  }

  #### Return outputs
  # Select relevant columns
  out <-
    out |>
    select("split", "bin", "coa_x", "coa_y") |>
    arrange(.data$split, .data$bin) |>
    as.data.table()
  colnames(out) <- c(.split, "timestamp", "coa_x", "coa_y")
  # Drop split column, if unspecified
  if (!keep_split) {
    out[[.split]] <- NULL
  }
  # Return outputs
  out
}
