#' @title Deprecated functions
#' @description These functions are deprecated in [`patter`].
#' @param ... Function arguments.
#' @details
#' * [`pf_plot_xy()`] has been renamed [`plot_xyt()`];
#' @name patter-deprecated

#' @rdname patter-deprecated
#' @export

pf_plot_xy <- function(...) {
  .Deprecated(old = "pf_plot_xy()", new = "plot_xyt()", package = "patter")
  plot_xyt(...)
}
