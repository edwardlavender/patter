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

call_start <- function(.fun = as.character(sys.call(-1L)), .time = Sys.time()) {
  .fun  <- as.character(.fun)
  .time <- call_time(.time)
  glue::glue("`patter::{.fun}()` called @ {.time}...",
             .envir = environment())
}

#' @rdname call_
#' @keywords internal

call_end <- function(.fun = as.character(sys.call(-1L)), .time = Sys.time()) {
  .fun  <- as.character(.fun)
  .time <- call_time(.time)
  glue::glue("`patter::{.fun}()` call ended @ {.time}...",
             .envir = environment())
}

#' @rdname call_
#' @keywords internal

call_duration <- function(.start, .end = Sys.time(), ...) {
  list(start = .start,
       end = .end,
       duration = difftime(.end, .start, ...))
}
