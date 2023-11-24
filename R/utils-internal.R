#' @title Utilities: signal messages, warnings or errors
#' @description These functions are wrappers for [`message()`], [`warning()`] and [`stop()`].
#' @param ... Arguments passed to [`glue::glue()`].
#' @details
#' * [`msg()`] is a [`message()`] wrapper;
#' * [`warn()`] is a [`warning()`] wrapper for immediate, clean warnings;
#' * [`abort()`] is a [`stop()`] wrapper for clean errors;
#'
#' @return Returned values follow parent functions.
#' @author Edward Lavender
#' @name signal
NULL

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

#' @title Progress bar wrappers
#'
#' @examples
#' \dontrun{
#' # Define loop
#' loop <- function(.progress) {
#'   n <- 10L
#'   pb <- pb_init(n, .progress)
#'   for (i in seq_len(n)){
#'     Sys.sleep(0.1)
#'     pb_tick(pb, .progress)
#'   }
#' }
#'
#' # Show progress bar
#' loop(.progress = TRUE)
#'
#' # Hide no progress bar
#' loop(.progress = FALSE)
#' }
#' @name pb

#' @rdname pb
#' @keywords internal

pb_init <- function(.n, .progress) {
  pb <- NULL
  if (.progress) {
    pb <- progress::progress_bar$new(total = .n)
    pb$tick(0)
  }
  pb
}

#' @rdname pb
#' @keywords internal

pb_tick <- function(.pb, .progress) {
  if (.progress) .pb$tick()
}

#' @title Utilities: create a log file
#' @description This function creates a .txt file (`.file`) via [`file.create`].
#' @param .file A character path to a file. `NULL` and `""` are permitted.
#' @param .verbose A logical variable that defines whether or not to act on `.file`.
#' @return The function returns `invisible(TRUE)`.
#' @author Edward Lavender
#' @keywords internal

create_log <- function(.file, .verbose) {
  if (.verbose & !is.null(.file) & .file != "") {
    if (tools::file_ext(.file) != "txt") {
      abort("`.txt` ('{.file}') should be the path to a text (.txt) file.",
            .envir = environment())
    }
    if (!dir.exists(dirname(.file))) {
      abort("`dirname(.txt)` ('{dirname(.file)}') does not exist.",
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
        warn("`.txt` ('{.file}`) already exists and is not empty!",
             .envir = environment())
      }
    }
  }
  invisible(TRUE)
}

#' @title `cat_to_cf()` helper
#' @keywords internal

cat_helper <- function(.verbose, .txt) {
  # Define log file
  check_verbose_and_log(.verbose = .verbose, .txt = .txt)
  create_log(.file = .txt, .verbose = .verbose)
  # Define function to send messages to console or file
  append_messages <- ifelse(.txt == "", FALSE, TRUE)
  function(..., message = .verbose, file = .txt, append = append_messages) {
    if (message) cat(paste(..., "\n"), file = .txt, append = append)
  }
}

#' @title Utilities: compact a list
#' @description Remove all `NULL` entries from a list.
#' @param l A list.
#' @source This function is derived from the `plyr::compact()` function. The function is defined separately in [`patter`] to reduce reliance on non-default packages.
#' @keywords internal

compact <- function(l) l[which(!sapply(l, is.null))]

#' @title Utilities: calculate column products
#' @description This function calculates column products for each row in a [`matrix`].
#' @param .data A [`matrix`].
#' @param .cols A vector that defines the columns in `.data` to multiply.
#' @return The function returns a `numeric` vector.
#' @author Edward Lavender
#' @keywords internal

colProds.matrix <- function(.data, .cols = seq_len(ncol(.data))) {
  if (length(.cols) == 1L) {
    return(.data[, .cols])
  }
  eval(parse(text = paste0(".data[, ", .cols, "]", collapse = " * ")))
}

