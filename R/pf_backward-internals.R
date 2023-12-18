#' @title PF: backward pass internals
#' @description Internal functions for the backward pass.
#' @author Edward Lavender
#' @keywords internal

.pf_backward_killer_outputs <- function(.start, .history, .record) {
  if (!.record$save) {
    .history <- list()
  }
  list(history = .history,
       time = call_timings(.start = .start))
}
