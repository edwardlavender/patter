#' @title PF: particle diagnostics
#' @description These are internal functions that calculate diagnostic statistics from selected particle samples.
#'
#' @details
#' # Particle diagnostics
#'
#' Particle diagnostics measure how the diversity of particle samples evolves through time. Current diagnostics include the number of unique particle samples at each time step and the effective sample size. Particle diagnostics have multiple uses:
#' * **Convergence.** Diagnostics from the forward run can indicate the causes of convergence failures. This is possible because [`pf_forward()`] keeps track of diagnostics at every stage of the algorithm, recording at each time step the diversity of particle proposals, the diversity remaining after each likelihood function evaluation and the diversity following (re)-sampling. See `vignette("d-demos", package = "patter")` for an illustration.
#' * **Sampling.** Diagnostics indicate sampling sufficiency. In general, a low ratio of the number of unique particles to the total number of particles indicates effective coverage of the possible locations of an individual.
#' * **Particle degeneracy.** Diagnostics measure particle degeneracy, i.e., the decay in the number of unique particles through time as trajectories are sooner-or-later rendered invalid by observations. Comparison of particle diagnostics between [`pf_backward_killer()`] and [`pf_backward_sampler()`] is particular valuable in this context. Both functions perform a backward refinement of particles from the forward sampler, but the former is crude but fast while the latter is sophisticated but expensive. Tracking particle diagnostics can indicate whether [`pf_backward_sampler()`] is worth the cost.
#'
#' # Internal routines
#'
#' * [`.pf_diag_any()`] identifies whether or not any particle samples remain;
#' * [`.pf_diag_ess()`] calculates effective sample size;
#' * [`.pf_diag_nu()`] counts the number of unique particle samples;
#' * [`.pf_diag()`] is a wrapper function that collates diagnostics;
#'
#' # Exported wrappers
#'
#' In [`pf_forward`], diagnostics are necessarily calculated on the fly by [`.pf_diag`]`_()` functions and simply require extraction from outputs via [`pf_forward_diagnostics()`]. However, note that the forward simulation necessitates the calculation of multiple diagnostics at each time step and [`pf_forward_diagnostics()`] attempts to collate all diagnostics in memory, which is not memory safe.
#'
#' [`pf_backward_killer()`] only tracks particle samples and it is therefore necessary to calculate diagnostics from particle samples post-hoc. A single set of diagnostics is calculated for each time step, so this function is (effectively) memory safe.
#'
#' @seealso
#' * [`pf_forward()`], [`pf_backward_killer()`] and [`pf_backward_sampler()`] implement the forward simulation and the backward pass;
#' * [`pf_forward_diagnostics()`] and [`pf_backward_killer_diagnostics()`] collect diagnostics;
#' * [`.pf_diag`]`_()` functions are internal routines that calculate diagnostic statistics;
#'
#' @author Edward Lavender
#' @name pf_diag

#' @rdname pf_diag
#' @keywords internal

.pf_diag_any <- function(.particles) {
  fnrow(.particles) != 0L
}

#' @rdname pf_diag
#' @keywords internal

.pf_diag_ess <- function(.likelihood) {
  1 / sum(.likelihood ^ 2)
}

#' @rdname pf_diag
#' @keywords internal

.pf_diag_nu <- function(.cells) {
  length(collapse::funique(.cells))
}

#' @rdname pf_diag
#' @keywords internal

.pf_diag <- function(.particles, .t, .trial = NA_integer_, .label) {
  out <- data.table(timestep = .t,
                    component = .label,
                    trial = .trial,
                    n = nrow(.particles),
                    n_u = 0L,
                    ess = 0L)
  if (out$n > 0) {
    n_u <- ess <- NULL
    out[, n_u := .pf_diag_nu(.particles$cell_now)]
    out[, ess := .pf_diag_ess(.particles$lik)]
  }
  out
}

#' @rdname pf_diag
#' @keywords internal

.pf_diag_bind <- function(.diag) {
  if (length(.diag) == 0L) {
    .diag <- NULL
  } else {
    collapse::unlist2d(.diag, idcols = FALSE, DT = TRUE)
  }
}

#' @rdname pf_diag
#' @keywords internal

.pf_diag_collect <- function(.diagnostics, .iter_m, .iter_i) {
  # Collect diagnostics from particle attributes
  diagnostics <- .pf_diag_bind(.diagnostics)
  # Define number of manual iterations
  iter_m <- NULL
  diagnostics[, iter_m := .iter_m]
  # Define number of internal iterations
  iter_i <- NULL
  diagnostics[, iter_i := .iter_i]
  setcolorder(diagnostics, c("iter_m", "iter_i", "timestep", "component", "trial", "n", "n_u", "ess"))
  diagnostics
}
