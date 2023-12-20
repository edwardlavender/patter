#' @title PF: backward pass internals
#' @description Internal functions for the backward pass.
#' @author Edward Lavender
#' @keywords internal

# Collate outputs for pf_backward_killer()
.pf_backward_killer_outputs <- function(.start, .history, .record) {
  if (!.record$save) {
    .history <- list()
  }
  out <- list(history = .history,
              time = call_timings(.start = .start))
  class(out) <- c(class(out), pf_class)
  out
}
