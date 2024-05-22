#' @title COA: centres of activity
#' @description This function calculates centres of activity (COAs) from detections at acoustic receivers.
#' @param .acoustics,.moorings Acoustic detection [`data.table`](s).
#' * `.acoustics` is a [`data.table`] of acoustic detections, with the following columns: `receiver_id` (or `sensor_id`), `timestamp` and (optionally) `receiver_x` and `receiver_y` columns;
#' * (optional) `.moorings` is a [`data.table`] of receiver coordinates, which should be provided if unavailable in `.acoustics`, with `receiver_id`, `receiver_x` and `receiver_y` columns;
#'
#' Receiver coordinates **must be planar**.
#'
#' @param .split (optional) A `character` that defines the name of the grouping factor in `.acoustics` (e.g., `individual_id` for [`dat_acoustics`]).
#' @param .delta_t The time interval over which to calculate COAs. This can be specified in any way understood by [`cut.POSIXt()`] (see the `breaks` argument).
#' @param .plot_weights,...,.one_page Plot arguments.
#' * `.plot_weights` is a `logical` variable that defines whether or not to plot the frequency distribution of weights for each `.split` value (i.e., the frequency distribution of the number of detections at each receiver in each time interval, excluding time intervals without detections).
#' * `...` is a placeholder for arguments passed to [`graphics::hist()`], excluding `main`.
#' * `.one_page` A `logical` variable that defines whether or not to plot all histograms on one page.
#'
#' @details COAs are calculated as a weighted mean of the locations of receivers at which individuals are detected over consecutive time intervals, weighted by the frequency of detections at each of those receivers. COAs are calculated via [`stats::weighted.mean()`], which assumes planar coordinates, for consistency with other [`patter`] routines. To handle longitude/latitude coordinates, use `geosphere::geomean()`.
#'
#' This function replaces [`flapper::coa()`](https://edwardlavender.github.io/flapper/reference/coa.html). See  [`flapper::coa_setup_delta_t()`](https://edwardlavender.github.io/flapper/reference/coa_setup_delta_t.html) to evaluate alternative time internals.
#'
#' @return The function returns a [`data.table`] with the following columns:
#' * `.split`---a `character` vector that distinguishes groups, if applicable;
#' * `timestamp`---a `POSIXt` vector of time stamps;
#' * `coa_x`, `coa_y`---the coordinates of the COAs;
#'
#' Data are arranged by `.split` and `timestamp`.
#'
#' @example man/example/example-coa.R
#' @seealso
#' * To derive location samples from a particle filtering algorithm, see [`pf_filter()`] and associates;
#' * For mapping utilisation distributions from coordinates, see `map_*()` functions (i.e., [`map_pou()`] and [`map_dens()`]);
#' @author Edward Lavender
#' @export

coa <- function(.acoustics, .moorings = NULL, .delta_t, .split = NULL,
                .plot_weights = TRUE, ..., .one_page = TRUE) {

  #### Check user inputs
  # check_dots_used: hist() warnings used
  check_dots_allowed("main", ...)
  check_dots_for_missing_period(formals(), list(...))

  #### Define dataset
  # (optional) Add receiver coordinates to `.acoustics`
  acoustics <- copy(.acoustics)
  if (rlang::has_name(acoustics, "sensor_id")) {
    receiver_id <- sensor_id <- NULL
    acoustics[, receiver_id := sensor_id]
  }
  if (!is.null(.moorings)) {
    ind        <- fmatch(acoustics$receiver_id, .moorings$receiver_id)
    receiver_x <- receiver_y <- NULL
    acoustics[, receiver_x := .moorings$receiver_x[ind]]
    acoustics[, receiver_y := .moorings$receiver_y[ind]]
    check_names(acoustics, req = .split)
  }

  #### Identify split column e.g., individual_id
  keep_split <- TRUE
  if (is.null(.split)) {
    keep_split <- FALSE
    .split <- "individual_id"
    individual_id <- NULL
    acoustics[, individual_id := 1L]
  }

  #### Prepare acoustics for COA calculations
  # * Define split column
  # * Group by split & define bins
  # * Calculate the frequency of detections in each bin
  tz <- lubridate::tz(acoustics$timestamp)
  acoustics <-
    acoustics |>
    mutate(split = acoustics[[.split]]) |>
    group_by(.data$split) |>
    mutate(bin = as.POSIXct(cut(.data$timestamp, .delta_t), tz = tz)) |>
    ungroup() |>
    group_by(.data$split, .data$bin, .data$receiver_id) |>
    mutate(n = n()) |>
    slice(1L) |>
    ungroup() |>
    as.data.table()
  # Plot the frequency distribution of weights
  if (.plot_weights) {
    pp <- one_page(.one_page, fndistinct(acoustics$split))
    on.exit(par(pp), add = TRUE)
    lapply(split(acoustics, acoustics$split), function(d) {
      hist(d$n, main = d$split[1], ...)
    }) |> invisible()
  }

  #### Calculate COAs
  # Calculate COAs using weighted.mean() (assumed planar coordinates)
  out <-
    acoustics |>
    group_by(.data$split, .data$bin) |>
    summarise(coa_x = stats::weighted.mean(.data$receiver_x, .data$n),
              coa_y = stats::weighted.mean(.data$receiver_y, .data$n)) |>
    ungroup() |>
    as.data.table()

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
