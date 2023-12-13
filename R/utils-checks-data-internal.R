#' @title Utilities: check data
#' @description These functions validate user datasets.
#'
#' @param .bathy A bathymetry [`SpatRaster`]. This is widely used as a generic grid in [`patter`] functions.
#' @param .lonlat A `logical` variable that defines whether spatial data is in longitude/latitude format.
#'
#' @param .acoustics,.moorings,.services,.archival Movement [`data.table`]s.
#' * `.acoustics` contains the detection time series (see [`dat_acoustics`] for an example);
#' * `.moorings` contains the receiver deployments (see [`dat_moorings`]) for an example);
#' * `.services` contains receiver servicing information;
#' * `.archival` contains archival (depth) time series (see [`dat_archival`] for an example);
#'
#' @details
#' These are internal functions that ensure that input dataset(s) meet [`patter`] requirements. Users should implement [`pat_setup_data()`] to prepare datasets for [`patter`] functions. Downstream functions silently assume that input datasets meet all requirements, without subsequent checks. This simplifies internal code and documentation.
#'
#' * `check_bathy()` checks the bathymetry [`SpatRaster`]:
#'    - Class: [`SpatRaster`];
#'    - Properties:
#'        * A square grid is recommended;
#'        * Absolute values (m) are recommended;
#'        * At the time of writing (December 2023), planar grids are better tested;
#'
#' * `check_acoustics()` checks detection time series:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `timestamp`---an ordered, `POSIXct` vector of time stamps with a defined `tzone` attribute;
#'        * `receiver_id---see `.moorings$receiver_id`;
#'    - Properties:
#'        * Functions assume accurate clock alignment between receivers;
#'        * For most functions, a single time series (for one individual) is required;
#'
#' * `check_services()` checks acoustic servicing records:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `receiver_id`---a `integer` vector of unique receiver deployments;
#'        * `receiver_start`,`receiver_end`---`Date` vectors of service start and end dates;
#'    - Properties:
#'        * Serviced receivers should be found in the `.moorings` dataset;
#'
#' * `check_moorings()` checks receiver deployment records:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `receiver_id`---an `integer` vector of unique receiver deployments (`1, ..., N`);
#'        * `receiver_easting` and `receiver_northing` and/or `receiver_lon` and `receiver_lat`---Receiver coordinates. `receiver_easting` and `receiver_northing` must be present if `.lonlat = FALSE` and `receiver_lon` and `receiver_lat` must be present otherwise. The selected coordinate columns are copied, renamed to `receiver_x` and `receiver_y` and coerced onto the `.bathy` grid.
#'    - Properties:
#'       * `receiver_start` should precede `receiver_end` (not currently validated);
#'       * Receiver coordinates must be valid (not currently validated);
#'
#' * `check_archival()` checks archival depth time series data:
#'    - Class: [`data.table`];
#'    - Columns:
#'        * `timestamp`---an ordered, `POSIXct` vector of time stamps with a defined `tzone` attribute;
#'        * `depth`---a `numeric` vector of (absolute) depths (m);
#'    - Properties:
#'        * Functions assume accurate clock alignment between acoustic and archival tags (if applicable);
#'        * For most functions, a single time series (for one individual) is required;
#'        * It is convenient if archival time stamps are regularly spaced;
#'
#' For all datasets, `NA`s may cause unexpected errors and produce a [`warning`]. `NAs` in columns other than those described above are safe.
#'
#' `NULL` inputs are permitted in all `check_{data}` functions, in which case `NULL` is returned.
#'
#' @return Each function returns the input dataset, as described, formatted in line with [`patter`]'s requirements.
#'
#' @seealso
#' * See [`datasets-mefs`] for example datasets included in [`patter`];
#' * See [`pat_setup_data()`] for the front-end function;
#'
#' @author Edward Lavender
#' @name check_data
NULL

#' @rdname check_data
#' @keywords internal

check_bathy <- function(.bathy) {
  check_inherits(.bathy, "SpatRaster")
  res <- terra::res(.bathy)
  if (!all.equal(res[1], res[2])) {
    warn("A square bathymetry grid is recommended.")
  }
  min_val <- terra::global(.bathy, "min", na.rm = TRUE)
  if (min_val < 0) {
    warn("A bathymetry grid with absolute values is recommended. Use NA to define inhospitable habitats (such as land).")
  }
  .bathy
}

#' @rdname check_data
#' @keywords internal

check_acoustics <- function(.acoustics) {
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
      length(unique(.acoustics$individual_id)) > 1L) {
    warn("Multiple individuals detected in acoustic data.")
  }
  # Check timestamps
  check_inherits(.acoustics$timestamp, "POSIXct")
  timestamp <- NULL
  .acoustics[, timestamp := check_tz(timestamp)]
  if (is.unsorted(.acoustics$timestamp)) {
    warn("Acoustic time stamps should be ordered chronologically.")
  }
  # Check receiver IDs
  if (inherits(.acoustics$receiver_id, "numeric")) {
    .acoustics$receiver_id <- as.integer(.acoustics$receiver_id)
  }
  check_inherits(.acoustics$receiver_id, "integer")
  # Check for NAs
  if (any(is.na(.acoustics))) {
    warn("The acoustic data contains NAs.")
  }
  .acoustics
}

#' @rdname check_data
#' @keywords internal

check_moorings <- function(.moorings, .lonlat, .bathy) {

  #### Check class
  if (is.null(.moorings)) {
    return(.moorings)
  }
  .moorings <- as.data.table(.moorings)

  #### Check names
  check_names(
    input = .moorings,
    req = c("receiver_id", "receiver_start", "receiver_end"),
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

  #### (optional) Define coordinate columns (receiver_x, receiver_y)
  if (!is.null(.lonlat)) {

    # Define coordinate columns
    if (.lonlat) {
      coords <- c("receiver_lon", "receiver_lat")
      .moorings$receiver_x <- .moorings$receiver_lon
      .moorings$receiver_y <- .moorings$receiver_lat
    } else {
      .moorings$receiver_y <- .moorings$receiver_northing
      coords <- c("receiver_easting", "receiver_northing")
      .moorings$receiver_x <- .moorings$receiver_easting
    }
    check_names(.moorings, req = coords)

    # (optional) Coerce coordinates onto grid
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
        warn("`.moorings` coordinates coerced onto `.bathy` grid.")
      }
    }
  }

  #### Check NAs
  if (any(is.na(.moorings))) {
    warn("`.moorings` contains NAs.")
  }

  #### Return outputs
  .moorings

}

#' @rdname check_data
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
    warn("The services data contains NAs.")
  }
  .services
}

#' @rdname check_data
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
      length(unique(.archival$individual_id)) > 1L) {
    abort("Multiple individuals detected in archival data.")
  }
  # Check time stamps
  check_inherits(.archival$timestamp, "POSIXct")
  timestamp <- NULL
  .archival[, timestamp := check_tz(timestamp)]
  if (is.unsorted(.archival$timestamp)) {
    abort("Archival time stamps should be ordered chronologically.")
  }
  if (nrow(.archival) > 1 && length(unique(diff(.archival$timestamp))) != 1L) {
    warn("Archival time steps are not regularly spaced.")
  }
  # Check depths
  check_inherits(.archival$depth, "numeric")
  if (any(.archival$depth < 0, na.rm = TRUE)) {
    abort("Archival depths should be a positive-valued numeric vector and not negative.")
  }
  # Check for NAs
  if (any(is.na(.archival))) {
    warn("The archival data contains NAs.")
  }
  .archival
}
