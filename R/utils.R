#' @title Signal messages, warnings or errors
#' @description These functions are wrappers for [`message()`], [`warning()`] and [`stop()`].
#' @param ... Arguments passed to [`glue::glue()`].
#' @details
#' * [`msg()`] is a [`message()`] wrapper;
#' * [`warn()`] is a [`warning()`] wrapper for immediate, clean warnings;
#' * [`abort()`] is a [`stop()`] wrapper for clean errors;
#'
#' @return Returned values follow parent functions.
#'
#' @author Edward Lavender
#'
#' @name signal

#' @rdname signal
#' @keywords internal

msg <- function(...) {
  message(glue::glue(...))
}

#' @rdname signal
#' @keywords internal

warn <- function(...) {
  warning(glue::glue(...), immediate. = TRUE, call. = FALSE)
}

#' @rdname signal
#' @keywords internal

abort <- function(...) {
  stop(glue::glue(...), call. = FALSE)
}


#' @title Create a log file
#' @description This function creates a .txt file (`.file`) via [`file.create`].
#' @param .file A character path to a file. `NULL` is permitted.
#' @return The function returns `invisible(TRUE)`.
#' @author Edward Lavender
#' @keywords internal

create_log <- function(.file) {
  if (!is.null(.file)) {
    if (tools::file_ext(.file) != "txt") {
      abort("`con` ('{.file}') should be the path to a text (.txt) file.",
            .envir = environment())
    }
    if (!dir.exists(dirname(.file))) {
      abort("`dirname(con)` ('{dirname(.file)}') does not exist.",
            .envir = environment())
    }
    if (!file.exists(.file)) {
      success <- file.create(.file)
      if (!success) {
        abort("Failed to create log file ('{.file}').",
              .envir = environment())
      }
    } else {
      if (length(readLines(.file)) > 0L) {
        warn("`con` ('{.file}`) already exists and is not empty!",
             .envir = environment())
      }
    }
  }
  invisible(TRUE)
}


#' @title Normalise a [`SpatRaster`]
#' @description This function normalises a [`SpatRaster`].
#' @param x A [`SpatRaster`].
#' @details
#' # Warning
#' * For speed in iterative applications (e.g., [`acs()`]), this function does not implement any internal checks.
#' * `NA`s are ignored.
#' @author Edward Lavender
#' @export

normalise <- function(x) {
  x / as.numeric(terra::global(x, "sum", na.rm = TRUE))
}


#' @title Compact a list
#' @description Remove all \code{NULL} entries from a list.
#' @param l A list.
#' @source This function is derived from the \code{plyr::compact()} function. The function is defined separately in \code{\link[flapper]{flapper}} to reduce reliance on non-default packages.
#' @keywords internal

compact <- function(l) l[which(!sapply(l, is.null))]
