#' @title Utilities: function call properties
#' @description These functions collate information related to function calls.
#' @name call_

#' @rdname call_
#' @keywords internal

call_time <- function(.time, form = "%Y-%m-%d %H:%M:%S") {
  format(.time, "%Y-%m-%d %H:%M:%S")
}

#' @rdname call_
#' @keywords internal

call_start <- function(.fun = as.character(sys.call(-1L)), .start = Sys.time()) {
  .fun  <- .fun[1]
  .start <- call_time(.start)
  glue::glue("`patter::{.fun}()` called @ {.start}...",
             .envir = environment())
}

#' @rdname call_
#' @keywords internal

call_end <- function(.fun = as.character(sys.call(-1L)), .start, .end = Sys.time()) {
  .fun   <- .fun[1]
  .start <- call_time(.start)
  .end   <- call_time(.end)
  glue::glue("`patter::{.fun}()` call ended @ {.end} (duration: ~{call_duration(.start, .end)}).",
             .envir = environment())
}

call_duration <- function(.start, .end, ...) {
  dft      <- difftime(.end, .start, ...)
  duration <- round(as.numeric(dft), digits = 2)
  units    <- attr(dft, "units")
  units    <- sub("s$", "", units)
  units    <- paste0(units, "(s)")
  glue::glue("{duration} {units}",
             .envir = environment())
}

#' @rdname call_
#' @keywords internal

call_timings <- function(.start, .end = Sys.time(), ...) {
  rlang::check_dots_used()
  list(start = .start,
       end = .end,
       duration = difftime(.end, .start, ...))
}
