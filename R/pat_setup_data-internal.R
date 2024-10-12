#' @title Utilities: check data
#' @description These functions validate user datasets. See Details for [`patter`] requirements.
#'
#' @param .map A [`SpatRaster`] that defines the study area for the simulation (see [`glossary`]).
#' @param .detections,.moorings,.services,.archival Observation [`data.table`]s.
#'
#' @details
#' These are internal functions that ensure that input dataset(s) meet [`patter`] requirements. Users with acoustic and/or archival data should implement [`pat_setup_data()`] to prepare datasets for [`patter`] functions. Downstream functions (often) silently assume that input datasets meet all requirements, without subsequent checks. This simplifies internal code and documentation.
#'
#' * [`check_map()`] checks the `.map` [`SpatRaster`]:
#'    - Class: [`SpatRaster`];
#'    - Name(s): `"map_value"`;
#'    - Properties:
#'        * A square grid is recommended;
#'        * Absolute values (m) are recommended;
#'        * A planar grid with coordinates in metres is currently required;
#'    - Source:
#'        * The data source may be in memory or on disk, but the former is faster for pure `R` functions;
#'
#' * [`check_detections()`] checks detection time series:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `timestamp`---an ordered, `POSIXct` vector of time stamps with a defined `tzone` attribute;
#'        * `receiver_id`---see `.moorings$receiver_id`;
#'    - Properties:
#'        * Each row defines a detection event;
#'        * Functions assume accurate clock alignment between receivers;
#'        * For most functions, a single time series (for one individual) is required;
#'
#' * [`check_services()`] checks acoustic servicing records:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `receiver_id`---see `.moorings$receiver_id`;
#'        * `receiver_start`, `receiver_end`---`POSIXct` vectors of service start and end dates. Before/after service events, receivers are assumed to have been deployed in the same locations; receiver deployments in different locations before/after servicing should be treated as distinct deployments in `.moorings`.
#'    - Properties:
#'        * Serviced receivers should be found in the `.moorings` dataset;
#'
#' * [`check_moorings()`] checks receiver deployment records:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `receiver_id`---an `integer` vector of unique receiver deployments (`1, ..., N`);
#'        * `receiver_x` and `receiver_y``---Receiver coordinates on a planar projection, matching `.map`;
#'    - Properties:
#'       * `receiver_start` should precede `receiver_end`;
#'       * Receiver deployment periods should (usually) at least partially overlap with the time period of acoustic detections;
#'       * Receiver coordinates must be valid on `.map` (not currently validated);
#'
#' * [`check_archival()`] checks archival depth time series data:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `timestamp`---an ordered, `POSIXct` vector of time stamps with a defined `tzone` attribute;
#'        * `depth`---a `numeric` vector of (absolute) depths (m);
#'    - Properties:
#'        * Functions assume accurate clock alignment between acoustic and archival tags (if applicable);
#'        * For most functions, a single time series (for one individual) is required;
#'
#' For all datasets, `NA`s may cause unexpected errors and produce a [`message`].
#'
#' `NULL` inputs are permitted in all `check_{data}` functions, in which case `NULL` is returned.
#'
#' @return Each function returns the input dataset, as described, formatted in line with [`patter`]'s requirements.
#'
#' @seealso
#' * See [`flapper::process_receiver_ids()`](https://edwardlavender.github.io/flapper/reference/process_receiver_id.html) to define unique receiver IDs;
#' * See [`datasets-mefs`] for example datasets included in [`patter`];
#' * See [`pat_setup_data()`] for the front-end function;
#'
#' @author Edward Lavender
#' @name check_dlist
NULL

#' @rdname check_dlist
#' @keywords internal

check_map <- function(.map) {
  if (is.null(.map)) {
    return(.map)
  }
  # Check class
  check_inherits(.map, "SpatRaster")
  message("`.map`: this is a reminder that a planar coordinate reference system (coordinate units: metres) is (currently) required. You can safely ignore this message if this is the case!")
  # Check names
  if (names(.map) != "map_value") {
    msg("`.map`: name updated from '{names(.map)}' to 'map_value'.",
        .envir = environment())
    names(.map) <- "map_value"
  }
  # Check resolution
  res <- terra::res(.map)
  if (!isTRUE(all.equal(res[1], res[2]))) {
    msg("`.map`: a square `.map` grid resolution is recommended.")
  }
  # Check values
  min_val <- terra::global(.map, "min", na.rm = TRUE)
  if (min_val < 0) {
    msg("`.map`: absolute values are recommended. Use NA to define inhospitable habitats (such as land).")
  }
  # Check source
  if (!terra::inMemory(.map)) {
    msg("`.map`: there is a speed penalty for grids that do not exist in memory in some functions.")
  }
  .map
}

#' @rdname check_dlist
#' @keywords internal

check_detections <- function(.detections, .moorings = NULL) {
  # Skip NULL
  if (is.null(.detections)) {
    return(NULL)
  }
  # Enforce class
  .detections <- as.data.table(.detections)
  # Check required columns
  check_names(.detections, req = c("timestamp", "receiver_id"))
  # Check for multiple individuals based on individual_id column
  if (rlang::has_name(.detections, "individual_id") &&
      fndistinct(.detections$individual_id) > 1L) {
    msg("`.detections`: multiple individuals detected in dataset.")
  }
  timestamp <- NULL
  .detections[, timestamp := check_POSIXct(timestamp)]
  if (is.unsorted(.detections$timestamp)) {
    msg("`.detections`: time stamps should be ordered chronologically.")
  }
  # Check receiver IDs
  if (inherits(.detections$receiver_id, "numeric")) {
    .detections$receiver_id <- as.integer(.detections$receiver_id)
  }
  check_inherits(.detections$receiver_id, "integer")
  if (!is.null(.moorings)) {
    if (!all(.detections$receiver_id %in% .moorings$receiver_id)) {
      warn("`.detections`: not all receivers in `.detections` are found in `.moorings`.")
    }
  }
  # Check for NAs
  if (any(is.na(.detections))) {
    msg("`.detections`: contains NAs.")
  }
  .detections
}

#' @rdname check_dlist
#' @keywords internal

check_moorings <- function(.moorings, .detections = NULL, .map) {

  #### Check class
  if (is.null(.moorings)) {
    return(.moorings)
  }
  .moorings <- as.data.table(.moorings)

  #### Check names
  check_names(
    input = .moorings,
    req = c("receiver_id",
            "receiver_start", "receiver_end",
            "receiver_x", "receiver_y"),
    extract_names = colnames,
    type = all
  )

  #### Check receiver IDs
  # * Numeric/integer
  # * Duplicates
  if (inherits(.moorings$receiver_id, "numeric")) {
    .moorings$receiver_id <- as.integer(.moorings$receiver_id)
  }
  check_inherits(.moorings$receiver_id, "integer")
  if (any(.moorings$receiver_id <= 0)) {
    abort("`.moorings$receiver_id` cannot contain receiver IDs <= 0.")
  }
  if (any(duplicated(.moorings$receiver_id))) {
    abort("`.moorings$receiver_id` contains duplicate elements.")
  }

  #### Check deployment periods
  # Check deployment times
  receiver_start <- receiver_end <- NULL
  .moorings[, receiver_start := check_POSIXct(receiver_start)]
  .moorings[, receiver_end := check_POSIXct(receiver_end)]
  if (any(.moorings$receiver_start >= .moorings$receiver_end)) {
    warn("`.moorings`: some `.moorings$receiver_start` entries are >= `.moorings$receiver_end` entries.")
  }
  # Check for receivers with deployment periods entirely outside the range of acoustic detections
  # * This requires a temporary data.frame (`deps`)
  if (!is.null(.detections)) {
    deps <- as.data.frame(.moorings)
    deps$interval <- lubridate::interval(deps$receiver_start, deps$receiver_end)
    deps$overlaps   <- lubridate::int_overlaps(
      deps$interval,
      lubridate::interval(min(.detections$timestamp), max(.detections$timestamp))
    )
    bool <- !deps$overlaps
    if (any(bool)) {
      warn("`.moorings`: the deployment period(s) of some receiver(s) ({str_items(deps$receiver_id[which(bool)])}) in `.moorings` are entirely outside the range of acoustic observations.", .envir = environment())
    }
  }

  #### Check detection probability parameters
  if (any(!rlang::has_name(.moorings, c("receiver_alpha", "receiver_beta", "receiver_gamma")))) {
    msg("`.moorings`: the detection probability parameter columns for acoustic observations (i.e., `receiver_alpha`, `receiver_beta` and `receiver_gamma`) expected by the built-in observation model structure for acoustic observations (`ModelObsAcousticLogisTrunc`) are missing from `.moorings`.")
  }

  #### Check NAs
  if (any(is.na(.moorings))) {
    msg("`.moorings`: contains NAs.")
  }

  #### Return outputs
  .moorings

}

#' @rdname check_dlist
#' @keywords internal

check_services <- function(.services, .moorings) {
  # Skip NULL
  if (is.null(.services)) {
    return(NULL)
  }
  # Check names
  check_names(
    input = .services, req = c("receiver_id", "service_start", "service_end"),
    extract_names = colnames, type = all)
  # Check times
  service_start <- service_end <- NULL
  .services[, service_start := check_POSIXct(service_start)]
  .services[, service_end := check_POSIXct(service_end)]
  # Check receiver IDs
  if (is.numeric(.services$receiver_id)) {
    .services$receiver_id <- as.integer(.services$receiver_id)
  }
  check_inherits(.services$receiver_id, "integer")
  if (!all(unique(.services$receiver_id) %in% unique(.moorings$receiver_id))) {
    warn("`.services`: not all receivers in `.services$receiver_id` are in `.moorings$receiver_id`.")
  }
  # Check for NAs
  if (any(is.na(.services))) {
    msg("`.services`: contains NAs.")
  }
  .services
}

#' @rdname check_dlist
#' @keywords internal

check_archival <- function(.archival) {
  # Skip NULL
  if (is.null(.archival)) {
    return(.archival)
  }
  # Enforce class
  .archival <- as.data.table(.archival)
  # Check required columns
  check_names(.archival, req = c("timestamp", "depth"))
  # Check for multiple individuals
  if (rlang::has_name(.archival, "individual_id") &&
      fndistinct(.archival$individual_id) > 1L) {
    msg("`.archival`: multiple individuals detected in dataset.")
  }
  # Check time stamps
  timestamp <- NULL
  .archival[, timestamp := check_POSIXct(timestamp)]
  if (is.unsorted(.archival$timestamp)) {
    msg("`.archival`: time stamps should be ordered chronologically.")
  }
  # Check depths
  check_inherits(.archival$depth, "numeric")
  if (any(.archival$depth < 0, na.rm = TRUE)) {
    msg("`.archival`: depths should be a positive-valued numeric vector and not negative.")
  }
  # Check for NAs
  if (any(is.na(.archival))) {
    msg("`.archival`: contains NAs.")
  }
  .archival
}
