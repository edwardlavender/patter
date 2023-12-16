#' @title PF: Likelihood function internals
#' @description This function evaluates the likelihood of the data given location proposals in [`pf_forward()`], wrapping specified likelihood functions (see [`pf_lik`]).
#' @param .particles,.obs,.t,.dlist Arguments passed to likelihood functions (see [`pf_lik`]).
#' @param .stack A named `list` of likelihood functions (see [`pf_lik`]).
#' @param .diagnostics An empty `list`, used to store likelihood diagnostics, or `NULL`.
#' @param .trial An `integer` that distinguishes trials.
#' @return A [`data.table`] that defines valid proposal locations, likelihoods and weights. A `diagnostics` attribute contains proposal diagnostics.
#' @author Edward Lavender
#' @keywords internal

.pf_lik <- function(.particles, .obs, .t, .dlist,
                   .stack,
                   .diagnostics = list(), .trial = NA_integer_) {

  #### Set up
  # Define global variables
  lik <- weight <- NULL
  .particles[, lik := 1]
  # Define baseline diagnostics
  if (!is.null(.diagnostics)) {
    .diagnostics[["base"]] <-
      .pf_diag(.particles = .particles, .t = .t,
               .trial = .trial, .label = "base")
  }

  #### Calculate likelihoods
  for (i in seq_len(length(stack))) {
    if (.pf_diag_any(.particles)) {
      .particles <- stack[[i]](.particles = .particles,
                               .obs = .obs,
                               .t = .t,
                               .dlist = .dlist)
      if (!is.null(.diagnostics)) {
        .diagnostics[[names(stack)[i]]] <-
          .pf_diag(.particles = .particles, .t = .t,
                   .trial = .trial, .label = names(stack)[i])
      }
    }
  }

  #### Return outputs
  .particles[, weight := lik]
  attr(.particles, "diagnostics") <- .diagnostics
  .particles

}
