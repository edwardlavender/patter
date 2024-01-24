#' @title PF: backward pass internals
#' @description Internal functions for the backward pass.
#' @author Edward Lavender
#' @keywords internal

# Collate pf_backward_*() output
.pf_backward_output <- function(.start, .history, .path = NULL, .record) {
  if (!.record$save) {
    .history <- list()
    .path    <- NULL
  }
  out <- list(history = .history,
              path = .path,
              time = call_timings(.start = .start))
  class(out) <- c(class(out), pf_class)
  out
}
