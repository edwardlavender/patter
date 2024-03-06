#' @title Utilities: check data
#' @description These functions validate user datasets. See Details for [`patter`] requirements.
#'
#' @param .bathy A (bathymetry) [`SpatRaster`]. This is widely used as a generic grid in [`patter`] functions.
#' @param .lonlat A `logical` variable that defines whether spatial data is in longitude/latitude or planar format. If unsupplied, this is defined (if possible) based on the columns in `.moorings` (via [`is.lonlat()`]).
#' @param .acoustics,.moorings,.services,.archival Movement [`data.table`]s.
#' @param .dlist,.dataset,.spatial,.par,.algorithm Arguments for [`check_dlist()`].
#'
#' @details
#' These are internal functions that ensure that input dataset(s) meet [`patter`] requirements. Users should implement [`pat_setup_data()`] to prepare datasets for [`patter`] functions. Downstream functions (often) silently assume that input datasets meet all requirements, without subsequent checks. This simplifies internal code and documentation.
#'
#' * [`check_bathy()`] checks the bathymetry [`SpatRaster`]:
#'    - Class: [`SpatRaster`];
#'    - Name(s): `"bathy"`;
#'    - Properties:
#'        * A square grid is recommended;
#'        * Absolute values (m) are recommended;
#'        * At the time of writing (December 2023), planar grids are better tested;
#'
#' * [`check_acoustics()`] checks detection time series:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `timestamp`---an ordered, `POSIXct` vector of time stamps with a defined `tzone` attribute;
#'        * `receiver_id`---see `.moorings$receiver_id`;
#'    - Properties:
#'        * Functions assume accurate clock alignment between receivers;
#'        * For most functions, a single time series (for one individual) is required;
#'
#' * [`check_services()`] checks acoustic servicing records:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `receiver_id`---see `.moorings$receiver_id`;
#'        * `receiver_start`,`receiver_end`---`Date` vectors of service start and end dates. Before/after service events, receivers are assumed to have been deployed in the same locations; receiver deployments in different locations before/after servicing should be treated as distinct deployments in `.moorings`.
#'    - Properties:
#'        * Serviced receivers should be found in the `.moorings` dataset;
#'
#' * [`check_moorings()`] checks receiver deployment records:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `receiver_id`---an `integer` vector of unique receiver deployments (`1, ..., N`);
#'        * `receiver_easting` and `receiver_northing` and/or `receiver_lon` and `receiver_lat`---Receiver coordinates. `receiver_easting` and `receiver_northing` must be present if `.lonlat = FALSE` and `receiver_lon` and `receiver_lat` must be present otherwise. The selected coordinate columns are copied, renamed to `receiver_x` and `receiver_y` and coerced onto the `.bathy` grid. Note that `.bathy` must be in the specified projection.
#'    - Properties:
#'       * `receiver_start` should precede `receiver_end`;
#'       * Receiver deployment periods should (usually) at least partially overlap with the time period of acoustic observations;
#'       * Receiver coordinates must be valid (not currently validated);
#'
#' * [`check_archival()`] checks archival depth time series data:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `timestamp`---an ordered, `POSIXct` vector of time stamps with a defined `tzone` attribute;
#'        * `depth`---a `numeric` vector of (absolute) depths (m);
#'    - Properties:
#'        * Functions assume accurate clock alignment between acoustic and archival tags (if applicable);
#'        * For most functions, a single time series (for one individual) is required;
#'        * It is convenient if archival time stamps are regularly spaced;
#'
#' * [`check_dlist()`] is an internal function used to confirm that the data `list` passed to a [`patter`] function contains the elements required for that function.
#'
#' For all datasets, `NA`s may cause unexpected errors and produce a [`message`]. `NAs` in columns other than those described above are safe.
#'
#' `NULL` inputs are permitted in all `check_{data}` functions, in which case `NULL` is returned.
#'
#' @return Each function returns the input dataset, as described, formatted in line with [`patter`]'s requirements.
#'
#' @seealso
#' * See the [`flapper::process_receiver_ids()`](https://edwardlavender.github.io/flapper/reference/process_receiver_id.html) to define unique receiver IDs;
#' * See [`datasets-mefs`] for example datasets included in [`patter`];
#' * See [`pat_setup_data()`] for the front-end function;
#'
#' @author Edward Lavender
#' @name check_dlist
NULL

#' @rdname check_dlist
#' @keywords internal

check_bathy <- function(.bathy) {
  if (is.null(.bathy)) {
    return(.bathy)
  }
  check_inherits(.bathy, "SpatRaster")
  if (names(.bathy) != "bathy") {
    warn("`.bathy` name updated from '{names(.bathy)}' to 'bathy'.",
         .envir = environment())
    names(.bathy) <- "bathy"
  }

  res <- terra::res(.bathy)
  if (!isTRUE(all.equal(res[1], res[2]))) {
    msg("A square bathymetry grid is recommended.")
  }
  min_val <- terra::global(.bathy, "min", na.rm = TRUE)
  if (min_val < 0) {
    msg("A bathymetry grid with absolute values is recommended. Use NA to define inhospitable habitats (such as land).")
  }
  .bathy
}

#' @rdname check_dlist
#' @keywords internal

check_acoustics <- function(.acoustics, .moorings = NULL) {
  # Skip NULL
  if (is.null(.acoustics)) {
    return(NULL)
  }
  # Enforce class
  .acoustics <- as.data.table(.acoustics)
  # Check required columns
  check_names(.acoustics, req = c("timestamp", "receiver_id"))
  # Check for multiple individuals based on individual_id column
  if (rlang::has_name(.acoustics, "individual_id") &&
      fndistinct(.acoustics$individual_id) > 1L) {
    msg("Multiple individuals detected in acoustic data.")
  }
  # Check timestamps
  check_inherits(.acoustics$timestamp, "POSIXct")
  timestamp <- NULL
  .acoustics[, timestamp := check_tz(timestamp)]
  if (is.unsorted(.acoustics$timestamp)) {
    msg("Acoustic time stamps should be ordered chronologically.")
  }
  # Check receiver IDs
  if (inherits(.acoustics$receiver_id, "numeric")) {
    .acoustics$receiver_id <- as.integer(.acoustics$receiver_id)
  }
  check_inherits(.acoustics$receiver_id, "integer")
  if (!is.null(.moorings)) {
    if (!all(.acoustics$receiver_id %in% .moorings$receiver_id)) {
      warn("Not all receivers in `.acoustics` are found in `.moorings`.")
    }
  }
  # Check for NAs
  if (any(is.na(.acoustics))) {
    msg("The acoustic data contains NAs.")
  }
  .acoustics
}

#' @rdname check_dlist
#' @keywords internal

check_moorings <- function(.moorings, .acoustics, .lonlat, .bathy) {

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
            "receiver_range"),
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
  # Check deployment dates
  if (any(.moorings$receiver_start >= .moorings$receiver_end)) {
    warn("Some `.moorings$receiver_start` entries are >= `.moorings$receiver_end` entries.")
  }
  # Check for receivers with deployment periods entirely outside the range of acoustic observations
  # * This requires a temporary data.frame (`deps`)
  deps <- as.data.frame(.moorings)
  deps$interval <- lubridate::interval(deps$receiver_start, deps$receiver_end)
  deps$within   <- deps$interval %within% lubridate::interval(min(.acoustics$timestamp), max(.acoustics$timestamp))
  bool <- !deps$within
  if (any(bool)) {
    warn("The deployment period(s) of some receiver(s) ({str_items(deps$receiver_id[which(bool)])}) in `.moorings` are entirely outside the range of acoustic observations. Consider excluding these receivers for improved efficiency (e.g., in `acs_setup_detection_kernels()`, which pre-calculates the likelihood of non detection at all operational receivers _for each array design_.", .envir = environment())
  }

  #### Check receiver ranges
  if (fndistinct(.moorings$receiver_range) != 1L) {
    warn("Not all functions (yet) support receiver-specific detection ranges.")
  }

  #### (optional) Define coordinate columns (receiver_x, receiver_y)
  if (!is.null(.lonlat)) {

    # Define coordinate columns
    if (.lonlat) {
      coords <- c("receiver_lon", "receiver_lat")
      .moorings$receiver_x <- .moorings$receiver_lon
      .moorings$receiver_y <- .moorings$receiver_lat
    } else {
      coords <- c("receiver_easting", "receiver_northing")
      .moorings$receiver_x <- .moorings$receiver_easting
      .moorings$receiver_y <- .moorings$receiver_northing
    }
    check_names(.moorings, req = coords)

    # (optional) Coerce coordinates onto grid
    # * This assumes the grid is in the correct projection
    if (!is.null(.bathy)) {
      xy <-
        .moorings |>
        select(all_of(coords)) |>
        as.matrix()
      xy <- terra::xyFromCell(.bathy, terra::cellFromXY(.bathy, xy))
      .moorings$receiver_x <- xy[, 1]
      .moorings$receiver_y <- xy[, 2]
      if (!identical(.moorings[["receiver_x"]], .moorings[[coords[1]]]) |
          !identical(.moorings[["receiver_y"]], .moorings[[coords[2]]])) {
        msg("`.moorings` coordinates coerced onto `.bathy` grid.")
      }
    }
  }

  #### Check NAs
  if (any(is.na(.moorings))) {
    msg("`.moorings` contains NAs.")
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
  # Check dates
  check_inherits(.services$service_start, "Date")
  check_inherits(.services$service_end, "Date")
  # Check receiver IDs
  if (is.numeric(.services$receiver_id)) {
    .services$receiver_id <- as.integer(.services$receiver_id)
  }
  check_inherits(.services$receiver_id, "integer")
  if (!all(unique(.services$receiver_id) %in% unique(.moorings$receiver_id))) {
    warn("Not all receivers in `.services$receiver_id` are in `.moorings$receiver_id`.")
  }
  # Check for NAs
  if (any(is.na(.services))) {
    msg("The services data contains NAs.")
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
    msg("Multiple individuals detected in archival data.")
  }
  # Check time stamps
  check_inherits(.archival$timestamp, "POSIXct")
  timestamp <- NULL
  .archival[, timestamp := check_tz(timestamp)]
  if (is.unsorted(.archival$timestamp)) {
    msg("Archival time stamps should be ordered chronologically.")
  }
  if (nrow(.archival) > 1 && fndistinct(diff(.archival$timestamp)) != 1L) {
    msg("Archival time steps are not regularly spaced.")
  }
  # Check depths
  check_inherits(.archival$depth, "numeric")
  if (any(.archival$depth < 0, na.rm = TRUE)) {
    msg("Archival depths should be a positive-valued numeric vector and not negative.")
  }
  # Check for NAs
  if (any(is.na(.archival))) {
    msg("The archival data contains NAs.")
  }
  .archival
}

#' @rdname check_dlist
#' @keywords internal

check_dlist <- function(.dlist,
                        .dataset = NULL,
                        .spatial = NULL,
                        .par = NULL,
                        .algorithm = NULL) {
  check_named_list(.dlist)
  check_names(.dlist, c("data", "spatial", "pars", "algorithm"))
  check_not_null(input = .dlist$data, req = .dataset)
  check_not_null(input = .dlist$spatial, req = .spatial)
  check_not_null(input = .dlist$pars, req = .par)
  check_not_null(input = .dlist$algorithm, req = .algorithm)
  invisible(TRUE)
}
