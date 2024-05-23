#' @title `Julia`: naming functions
#' @description A set of internal naming functions.
#' @author Edward Lavender
#' @keywords internal

# Name the output object from a pf_*() function
name_particles <- function(.fun = c("pf_filter", "pf_smoother_two_filter"),
                           .direction = c("forward", "backward")) {
  .fun       <- match.arg(.fun)
  .direction <- match.arg(.direction)
  if (.fun == "pf_filter" && .direction == "forward") {
    # particles from forward run
    "pfwd"
  } else if (.fun == "pf_filter" && .direction == "backward") {
    # particles from backward run
    "pbwd"
  } else if (.fun == "pf_smoother_two_filter") {
    # particles from two-filter smoother
    "ptf"
  }
}
