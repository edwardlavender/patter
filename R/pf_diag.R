#' @title PF: particle diagnostics
#' @description These functions collate particle diagnostics.
#'
#' @param .history Particle samples, provided in any format accepted by [`.pf_history_dt()`].
#'
#' @param ... Additional arguments passed to [`.pf_history_dt()`]. `.collect`, if used, is necessarily `TRUE` and should not be specified.
#'
#' @details Particle diagnostics are fully described in [`pf_diag-internal`].
#'
#' [`pf_diag_summary()`] summarises particle diagnostics.
#'
#' @example man/examples/pf_diag-examples.R
#'
#' @return `pf_diag_*()` functions return [`data.table`]s.
#'
#' [`pf_diag_summary`] returns a summary [`data.table`] with the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * `n`---an `integer` that defines the number of particles;
#' * `nu`---an `integer` that defines the number of unique location samples (see [`.pf_diag_nu()`]);
#' * `ess`---a `double` that defines the effective sample size or `NA_real_` if the `logwt` column is unavailable (see [`.pf_diag_ess()`]);
#'
#' @inherit pf_diag-internal seealso
#' @name pf_diag

#' @rdname pf_diag
#' @export

pf_diag_summary <- function(.history, ...) {
  .history |>
    .pf_history_dt(..., .collect = TRUE) |>
    set_missing(.col = "logwt") |>
    lazy_dt(immutable = TRUE) |>
    # Normalise weights by time step
    # (This should be enforced by pf_forward())
    group_by(.data$timestep) |>
    mutate(logwt = lognormalise(.data$logwt)) |>
    ungroup() |>
    # Aggregate weights by time step and grid cell
    group_by(.data$timestep, .data$cell_now) |>
    summarise(logwt = logSumExp(.data$logwt)) |>
    ungroup() |>
    # Summarise diagnostics by time step
    group_by(.data$timestep) |>
    summarise(timestep = .data$timestep[1],
              n = n(),
              nu = fndistinct(.data$cell_now),
              ess = exp(-logSumExp(2 * .data$logwt))) |>
    as.data.table()
}

#' @title PF: particle diagnostics
#' @description These are internal functions that calculate diagnostic statistics from selected particle samples.
#'
#' @details
#' # Particle diagnostics
#'
#' Particle diagnostics measure how the diversity of particle samples evolves through time. Current diagnostic metrics include the number of unique particle samples at each time step and the effective sample size. Particle diagnostics have multiple uses:
#' * **Convergence.** We formerly tracked particle diagnostics in [`pf_forward()`] to facilitate identification of the causes of convergence failures, but this is not currently implemented.
#' * **Sampling.** Diagnostics indicate sampling sufficiency. In general, a low ratio of the number of unique particles to the total number of particles indicates effective coverage of the possible locations of an individual.
#' * **Particle degeneracy.** Diagnostics measure particle degeneracy, i.e., the decay in the number of unique particles through time as trajectories are rendered invalid by observations.
#'
#' # Internal routines
#'
#' * [`.pf_diag_any()`] identifies whether or not any particle samples remain;
#' * [`.pf_diag_nu()`] counts the number of unique particle samples (grid cells);
#' * [`.pf_diag_ess()`] calculates effective sample size from normalised particle (log) weights;
#'
#' @seealso
#' * [`pf_forward()`] and associates implement particle filtering algorithms;
#' * [`pf_diag_summary()`] summarises diagnostics;
#' * [`.pf_diag`]`_()` functions are internal routines that calculate diagnostic statistics;
#'
#' @author Edward Lavender
#' @name pf_diag-internal

#' @rdname pf_diag-internal
#' @keywords internal

.pf_diag_any <- function(.particles) {
  fnrow(.particles) != 0L
}

#' @rdname pf_diag-internal
#' @keywords internal

.pf_diag_nu <- function(.particles) {
  fndistinct(.particles$cell_now)
}

#' @rdname pf_diag-internal
#' @keywords internal

.pf_diag_ess <- function(.logwt) {
  # Handle missing weights
  if (all(is.na(.logwt))) {
    return(NA_real_)
  }
  # Validate weights are normalised
  stopifnot(isTRUE(all.equal(exp(logSumExp(.logwt)), 1)))
  # Calculate ESS
  exp(-logSumExp(2 * .logwt))
}
