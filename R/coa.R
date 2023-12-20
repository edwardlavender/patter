#' @title COA: centres of activity
#' @description This function calculates centres of activity (COAs) from detections at acoustic receivers.
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$data$acoustics`, with the following columns: `receiver_id` and `timestamp`;
#' * `.dlist$data$moorings`, with the following columns: `receiver_id`, `receiver_x` and  `receiver_y`;
#' * `.dlist$pars$lonlat`, which specifies the coordinate format (longitude/latitude or planar);
#' @param .split (optional) A `character` that defines the name of the grouping factor in `.dlist$data$acoustics` (e.g., `individual_id` for [`dat_acoustics`]).
#' @param .delta_t The time interval over which to calculate COAs. This can be specified in any way understood by [`cut.POSIXt()`] (see the `breaks` argument).
#' @param .plot_weights,...,.one_page Plot arguments.
#' * `.plot_weights` is a `logical` variable that defines whether or not to plot the frequency distribution of weights for each `.split` value (i.e., the frequency distribution of the number of detections at each receiver in each time interval, excluding time intervals without detections).
#' * `...` is a placeholder for arguments passed to [`graphics::hist()`].
#' * `.one_page` A `logical` variable that defines whether or not to plot all histograms on one page.
#'
#' @details COAs are calculated as a weighted mean of the locations of receivers at which individuals are detected over consecutive time intervals, weighted by the frequency of detections at each of those receivers. COAs are calculated via [`stats::weighted.mean()`] (for planar coordinates) or [`geosphere::geomean()`] (for longitude/latitude coordinates).
#'
#' @return The function returns a [`data.table`] with the following columns:
#' * `.split`---a `character` vector that distinguishes groups, if applicable;
#' * `timestamp`---a `POSIXt` vector of time stamps;
#' * `coa_x`,`coa_y`---the coordinates of the COAs;
#'
#' Data are arranged by `.split` and `timestamp`.
#'
#' @seealso
#' * To derive location samples from the forward-filter backward-sampler, see [`pf_forward()`], [`pf_backward_sampler()`] and [`pf_coord()`];
#' * For mapping utilisation distributions from coordinates, see `map_*()` functions (i.e., [`map_pou()`] and [`map_dens()`];
#'
#' @source This function replaces the [`coa()`](https://edwardlavender.github.io/flapper/reference/coa.html) function in the [`flapper`](https://edwardlavender.github.io/flapper/) package. See the [`coa_setup_delta_t()`](https://edwardlavender.github.io/flapper/reference/coa_setup_delta_t.html) function to evaluate alternative time internals.
#'
#' @example man/examples/coa-examples.R
#'
#' @author Edward Lavender
#' @export

coa <- function(.dlist, .delta_t, .split = NULL,
                .plot_weights = TRUE, ..., .one_page = TRUE) {

  #### Check user inputs
  check_dlist(.dlist = .dlist,
             .dataset = c("acoustics", "moorings"),
             .par = "lonlat")
  check_dots_for_missing_period(formals(), list(...))
  rlang::check_dots_used()

  #### Define datasets
  acoustics  <- .dlist$data$acoustics
  moorings   <- .dlist$data$moorings
  ind        <- match(acoustics$receiver_id, moorings$receiver_id)
  receiver_x <- receiver_y <- NULL
  acoustics[, receiver_x := moorings$receiver_x[ind]]
  acoustics[, receiver_y := moorings$receiver_y[ind]]
  check_names(acoustics, req = .split)
  lonlat <- .dlist$par$lonlat

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
    pp <- par(mfrow = par_mf(length(unique(acoustics$split))))
    on.exit(par(pp), add = TRUE)
    lapply(split(acoustics, acoustics$split), function(d) {
      hist(d$n, main = d$split[1], ...)
    }) |> invisible()
  }

  #### Calculate COAs
  if (lonlat) {
    # Calculate COAs using geomean() if lonlat
    out <-
      acoustics |>
      group_by(.data$split, .data$bin) |>
      summarise(
        coa_xy = geomean(xy = as.matrix(cbind(x = .data$receiver_x,
                                              y = .data$receiver_y),
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
      acoustics |>
      group_by(.data$split, .data$bin) |>
      summarise(coa_x = stats::weighted.mean(.data$receiver_x, .data$n),
                coa_y = stats::weighted.mean(.data$receiver_y, .data$n)) |>
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
