#' @title PF: sampling functions
#'
#' @description These are particle sampling functions for [`pf_forward()`].
#'
#' @param .particles A [`data.table`] that defines particle samples. This includes a `weight` column that should be used for (re)sampling. `.particles$weights` are normalised to sum to one.
#' @param .n An `integer` that defines the number of particle samples at each time step.
#'
#' @details
#' [`pf_forward()`] expects a sampling function in the `.sample` argument. You can write your own function or use an exported one:
#'  * [`pf_sample_multinomial`] implements multinomial (re)sampling.
#'  * [`pf_sample_systematic()`] implements systematic (re)sampling.
#'
#' Sampling functions must accept `.particles` and `.n` arguments and return a [`data.table`] of `.n` particle samples. The `weight` column must be reset after sampling.
#'
#' Systematic particle sampling ([`pf_sample_systematic()`]) is generally recommended.
#'
#' @return The functions return a [`data.table`], as inputted, but containing `.n` rows, one for each sampled particle.
#'
#' @examples
#' require(data.table)
#' p <- data.table(cell_now = c(1, 2, 3), weight = c(0.5, 0.25, 0.25))
#' pf_sample_multinomial(p, .n = 10L)
#' pf_sample_systematic(p, .n = 10L)
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @name pf_sample

#' @rdname pf_sample
#' @export

pf_sample_multinomial <- function(.particles, .n) {
  if (fnrow(.particles) > 0L) {
    stopifnot(rlang::has_name(.particles, "weight"))
    stopifnot(all.equal(sum(.particles$weight), 1))
    .particles <- .particles[sample.int(.N,
                          size = .n,
                          replace = TRUE,
                          prob = .particles$weight), ]
    # Reset the weights
    weight <- NULL
    .particles[, weight := 1 / fnrow(.particles)]
  }
  .particles
}

#' @rdname pf_sample
#' @export

pf_sample_systematic <- function(.particles, .n) {
  if (fnrow(.particles) > 0L) {
    # Cumulative sum of weights
    stopifnot(rlang::has_name(.particles, "weight"))
    stopifnot(isTRUE(all.equal(sum(.particles$weight), 1)))
    cwt <- cumsum(.particles$weight)
    # Simulate starting point
    u1 <- stats::runif(n = 1, 0, 1 / .n)
    # Define grid
    u <- seq(u1, 1, by = 1 / .n)
    # Select particles
    .particles <- .particles[findInterval(u, cwt) + 1, ]
    # Reset the weights
    weight <- NULL
    .particles[, weight := 1 / fnrow(.particles)]
  }
  .particles
}
