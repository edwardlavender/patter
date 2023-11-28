#' @title PF: sampling functions
#' @name pf_sample

#' @rdname pf_sample
#' @export

.pf_sample_multinomial <- function(.particles, .n) {
  stopifnot(rlang::has_name(.particles, "weight"))
  if (fnrow(.particles) > 0L) {
    .particles[sample.int(.N, size = .n, replace = TRUE, prob = .particles$weight), ]
  } else {
    .particles
  }
}

#' @rdname pf_sample
#' @export

.pf_sample_systematic <- function(.particles, .n) {
  if (fnrow(.particles) > 0L) {
    # Cumulative sum of weights
    stopifnot(rlang::has_name(.particles, "weight"))
    cwt <- cumsum(.particles$weight)
    # Simulate starting point
    u1 <- stats::runif(n = 1, 0, 1 / .n)
    # Define grid
    u <- seq(u1, 1, by = 1 / .n)
    # Select particles
    .particles[findInterval(u, cwt) + 1, ]
  } else {
    .particles
  }
}
