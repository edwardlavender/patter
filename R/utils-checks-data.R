#' @title Utilities: check data
#' @description These functions check input datasets.
#' @param .moorings,.services,.acoustics,.archival User inputs. `NULL` inputs are allowed for `.services` and `.archival`.
#' @return The functions invisibly return the inputted object if all checks are passed, with the following adjustments:
#' * `.moorings$receiver_id` is silently coerced to an integer if numeric;
#' @author Edward Lavender
#' @name check_data
NULL

#' @rdname check_data
#' @keywords internal

check_moorings <- function(.moorings,
                           .class = "data.table",
                           .lonlat = NULL,
                           .bathy = NULL) {

  #### Check class
  check_inherits(.moorings, .class)

  #### Check names
  check_names(
    input = .moorings, req = c("receiver_id", "receiver_start", "receiver_end"),
    extract_names = colnames, type = all
  )

  #### Check receiver IDs
  # * Numeric/integer
  # * Duplicates
  if (inherits(.moorings$receiver_id, "numeric")) {
    .moorings$receiver_id <- as.integer(.moorings$receiver_id)
  }
  check_inherits(.moorings$receiver_id, "integer")
  if (any(.moorings$receiver_id <= 0)) {
    abort("Argument '.moorings$receiver_id' cannot contain receiver IDs <= 0.")
  }
  if (any(duplicated(.moorings$receiver_id))) {
    abort("Argument '.moorings$receiver_id' contains duplicate elements.")
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
    warn("`.moorings` contains NAs and some functions may fail unexpectedly.")
  }

  #### Return outputs
  invisible(.moorings)

}

#' @rdname check_data
#' @keywords internal

check_services <- function(.services, .moorings) {
  if (!is.null(.services)) {
    check_names(
      input = .services, req = c("receiver_id", "service_start", "service_end"),
      extract_names = colnames, type = all)
    if (is.numeric(.services$receiver_id)) {
      .services$receiver_id <- as.integer(.services$receiver_id)
    }
    check_inherits(.services$receiver_id, "integer")
    if (!all(unique(.services$receiver_id) %in% unique(.moorings$receiver_id))) {
      warn("Not all receivers in `.services$receiver_id` are in `.moorings$receiver_id`.")
    }
  }
  invisible(.services)
}

#' @rdname check_data
#' @keywords internal

check_acoustics <- function(.acoustics) {
  # Check class
  if (!inherits(.acoustics, "data.table")) {
    .acoustics <- as.data.table(.acoustics)
  }
  # check_inherits(.acoustics, "data.table")
  # Check required columns
  check_names(.acoustics, req = c("timestamp", "receiver_id"))
  check_inherits(.acoustics$timestamp, "POSIXct")
  if (inherits(.acoustics$receiver_id, "numeric")) {
    .acoustics$receiver_id <- as.integer(.acoustics$receiver_id)
  }
  check_inherits(.acoustics$receiver_id, "integer")
  # Check for NAs
  if (any(is.na(.acoustics))) {
    abort("The acoustic data contains NAs.")
  }
  # Check for multiple individuals
  if (rlang::has_name(.acoustics, "individual_id") && length(unique(.acoustics$individual_id)) > 1L) {
    abort("Multiple individuals detected in acoustic data.")
  }
  # Check time zone
  .acoustics$timestamp <- check_tz(.acoustics$timestamp)
  # Check sorted
  if (is.unsorted(.acoustics$timestamp)) {
    abort("Acoustic time stamps should be ordered chronologically.")
  }
  invisible(.acoustics)
}

#' @rdname check_data
#' @keywords internal

check_archival <- function(.archival) {
  if (!is.null(.archival)) {
    # Check class
    if (!inherits(.archival, "data.table")) {
      .archival <- as.data.table(.archival)
    }
    # check_inherits(.archival, "data.table")
    # Check required columns
    check_names(.archival, req = c("timestamp", "depth"))
    check_inherits(.archival$timestamp, "POSIXct")
    check_inherits(.archival$depth, "numeric")
    # Check for NAs
    if (any(is.na(.archival))) {
      abort("The archival data contains NAs.")
    }
    # Check for multiple individuals
    if (rlang::has_name(.archival, "individual_id") && length(unique(.archival$individual_id)) > 1L) {
      abort("Multiple individuals detected in archival data.")
    }
    # Check time zone
    .archival$timestamp <- check_tz(.archival$timestamp)
    # Check sorted
    if (is.unsorted(.archival$timestamp)) {
      abort("Archival time stamps should be ordered chronologically.")
    }
    # Check regularity
    if (nrow(.archival) > 1 && length(unique(diff(.archival$timestamp))) != 1L) {
      abort("Archival time steps are assumed to be regularly spaced.")
    }
    # Check depths
    if (any(.archival$depth < 0)) {
      abort("Archival depths should be a positive-valued numeric vector and not negative.")
    }
  }
  invisible(.archival)
}
