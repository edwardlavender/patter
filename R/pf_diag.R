#' @title PF: particle diagnostics
#' @description These functions collate particle diagnostics from [`pf_forward()`] and [`pf_backward_*()`].
#' @param .sink For [`pf_diag_convergence()`], `.sink` specifies particle diagnostics. The following inputs are accepted:
#' * A [`pf_particles-class`] object;
#' * A `character` string that defines the directory containing `parquet` files, specified in one of the following formats:
#'    * `{.record$sink}` (as specified in [`pf_forward()`]);
#'    * `{.record$sink}/diagnostics/`;
#'
#' @param .history For [`pf_diag_summary()`], `.history` expects particle samples, provided in any format accepted by [`.pf_history_dt()`].
#'
#' @param ... Additional arguments.
#' * In [`pf_diag_convergence()`], `...` is passed to [`arrow::open_dataset()`].
#' * In [`pf_diag_summary()`], `...` is passed to [`.pf_history_dt()`]. `.collect`, if used, is necessarily `TRUE` and should not be specified.
#'
#' @details Particle diagnostics are fully described in [`pf_diag-internal`].
#'
#' [`pf_diag_convergence()`] collates convergence diagnostic outputs from [`pf_forward()`]. If [`pf_forward()`] is implemented with `.record$save = TRUE`, the outputted [`pf_particles-class`] object from [`pf_forward()`] includes a pre-compiled [`data.table`] of diagnostics (see [`pf_opt_record()`]). [`pf_diag_convergence()`] accepts a [`pf_particles-class`] object, but there is really no need for this, as a `diagnostics` element is already present in this object. Instead, [`pf_diag_convergence()`] is primarily designed to collate particle diagnostics on file. [`pf_forward()`] writes particle diagnostics to `{.record$sink}/diagnostics` (see [`pf_opt_record()`]). You can supply `{.record$sink}` or `{.record$sink}/diagnostics/` to this function. The individual [`data.table`]s are collated to match the form in which they are provided by [`pf_forward()`] when `.record$save = TRUE`.
#'
#' [`pf_diag_summary()`] summarises particle diagnostics directly from (accepted) particle samples. This can be used for [`pf_forward()`] or [`pf_backward_*()`].
#'
#' @example man/examples/pf_diag-examples.R
#'
#' @return `pf_diag_*()` functions return [`data.table`]s.
#'
#' [`pf_diag_convergence()`] returns the `diagnostics` [`data.table`] of a `pf_particles-class` object.
#'
#' [`pf_diag_summary`] returns a summary [`data.table`] with the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * `n`---an `integer` that defines the number of particles;
#' * `n_u`---an `integer` that defines the number of unique location samples (see [`.pf_diag_nu()`]);
#' * `ess`---a `double` that defines the effective sample size or `NA_real_` if the `lik` column is unavailable (see [`.pf_diag_ess()`]);
#'
#' @inherit pf_diag-internal seealso
#' @name pf_diag

#' @rdname pf_diag
#' @export

pf_diag_convergence <- function(.sink, ...) {
  if (inherits(.sink, pf_class)) {
    return(.sink$diagnostics)
  }
  check_dir_exists(.sink)
  if (basename(.sink) != "diagnostics") {
    .sink <- file.path(.sink, "diagnostics")
  }
  check_dir_exists(.sink)
  .sink |>
    arrow::open_dataset(...) |>
    arrange(.data$iter_m, .data$iter_i, .data$timestep) |>
    collect() |>
    as.data.table()
}

#' @rdname pf_diag
#' @export

pf_diag_summary <- function(.history, ...) {
  .history |>
    .pf_history_dt(..., .collect = TRUE) |>
    add_col_real(.col = "weight") |>
    lazy_dt() |>
    group_by(.data$timestep) |>
    summarise(timestep = .data$timestep[1],
              n = n(),
              n_u = .pf_diag_nu(.data$cell_now),
              ess = .pf_diag_ess(.data$weight)
    ) |>
    as.data.table()
}

#' @title PF: particle diagnostics (internal)
#' @description These are internal functions that calculate diagnostic statistics from selected particle samples.
#'
#' @details
#' # Particle diagnostics
#'
#' Particle diagnostics measure how the diversity of particle samples evolves through time. Current diagnostic metrics include the number of unique particle samples at each time step and the effective sample size. Particle diagnostics have multiple uses:
#' * **Convergence.** Diagnostics from the forward run can indicate the causes of convergence failures. This is possible because [`pf_forward()`] keeps track of diagnostics at every stage of the algorithm, recording at each time step the diversity of particle proposals, the diversity remaining after each likelihood function evaluation and the diversity following (re)-sampling. See [`pf_particles-class`] and `vignette("d-demos", package = "patter")` for an illustration.
#' * **Sampling.** Diagnostics indicate sampling sufficiency. In general, a low ratio of the number of unique particles to the total number of particles indicates effective coverage of the possible locations of an individual.
#' * **Particle degeneracy.** Diagnostics measure particle degeneracy, i.e., the decay in the number of unique particles through time as trajectories are rendered invalid by observations. Comparison of particle diagnostics between [`pf_backward_killer()`] and [`pf_backward_sampler`]`_*()` is particularly valuable in this context. Both functions perform a backward refinement of particles from [`pf_forward()`]; however, the former is simple but fast, while the latter is sophisticated but expensive. Tracking particle diagnostics can indicate whether [`pf_backward_sampler`]`_*()` is worth the cost.
#'
#' # Internal routines
#'
#' * [`.pf_diag_any()`] identifies whether or not any particle samples remain;
#' * [`.pf_diag_nu()`] counts the number of unique particle samples (grid cells);
#' * [`.pf_diag_ess()`] calculates effective sample size from normalised particle weights;
#' * [`.pf_diag()`] is a wrapper function that calculates diagnostics;
#' * [`.pf_diag_bind()`] binds `list`s of diagnostics together;
#' * [`.pf_diag_collect()`] wraps [`.pf_diag_bind()`] and assigns required columns;
#'
#' # Exported wrappers
#'
#' In [`pf_forward()`], convergence diagnostics are necessarily calculated on the fly by [`.pf_diag`]`_()` functions. To collate convergence diagnostics from [`pf_forward()`], use [`pf_diag_convergence()`]. However, note that the forward simulation necessitates the calculation of multiple diagnostics at each time step and [`pf_diag_convergence()`] attempts to collate all diagnostics in memory, which is not memory safe.
#'
#' To collate summary diagnostics from [`pf_forward()`] or [`pf_backward_*()`], use [`pf_diag_summary()`]. This function calculates a single set of diagnostics is calculated for each time step, so this function is (effectively) memory safe.
#'
#' @seealso
#' * [`pf_forward()`] and [`pf_backward_*()`] implement the forward simulation and the backward pass;
#' * [`pf_diag_convergence()`] and [`pf_diag_summary()`] collect diagnostics;
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

.pf_diag_nu <- function(.cells) {
  length(collapse::funique(.cells))
}

#' @rdname pf_diag-internal
#' @keywords internal

.pf_diag_ess <- function(.weight) {
  if (all(is.na(.weight))) {
    return(NA_real_)
  }
  stopifnot(isTRUE(all.equal(sum(.weight), 1)))
  1 / sum(.weight ^ 2)
}

#' @rdname pf_diag-internal
#' @keywords internal

.pf_diag <- function(.particles, .weight, .t, .label) {
  out <- data.table(timestep = .t,
                    component = .label,
                    n = fnrow(.particles),
                    n_u = NA_integer_,
                    ess = NA_real_)
  if (out$n > 0) {
    n_u <- ess <- NULL
    out[, n_u := .pf_diag_nu(.particles$cell_now)]
    out[, ess := .pf_diag_ess(.particles[[.weight]])]
  }
  out
}

#' @rdname pf_diag-internal
#' @keywords internal

.pf_diag_bind <- function(.diagnostics) {
  if (length(.diagnostics) == 0L) {
    .diagnostics <- NULL
  } else {
    collapse::unlist2d(.diagnostics, idcols = FALSE, DT = TRUE)
  }
}

#' @rdname pf_diag-internal
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
  setcolorder(diagnostics, c("iter_m", "iter_i",
                             "timestep", "component",
                             "n", "n_u", "ess"))
  diagnostics
}
