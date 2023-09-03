#' @title Signal messages, warnings or errors
#' @description These functions are wrappers for [`message()`], [`warning()`] and [`stop()`].
#' @param ... Arguments passed to [`glue::glue()`].
#' @details
#' * [`msg()`] is a wrapper for [`message()`];
#' * [`warn()`] is a [`warning()`] wrapper for immediate, clean warnings;
#' * [`abort()`] is a [`stop()`] wrapper for clean errors;
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
