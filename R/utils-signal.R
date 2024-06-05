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
#' @name utils-signal
NULL

#' @rdname utils-signal
#' @keywords internal

msg <- function(...) {
  message(glue::glue(...))
}

#' @rdname utils-signal
#' @keywords internal

warn <- function(...) {
  warning(glue::glue(...), immediate. = TRUE, call. = FALSE)
}

#' @rdname utils-signal
#' @keywords internal

abort <- function(...) {
  stop(glue::glue(...), call. = FALSE)
}
