#' @title PF: calculate particle diagnostics
#' @name pf_diag

#' @rdname pf_diag
#' @keywords internal

.pf_diag_any <- function(.particles) {
  nrow(.particles) != 0L
}

#' @rdname pf_diag
#' @keywords internal

.pf_diag_ess <- function(.likelihood) {
  1 / sum(.likelihood ^ 2)
}

#' @rdname pf_diag
#' @keywords internal

.pf_diag_unique <- function(.cells) {
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
    out[, n_u := .pf_diag_unique(.particles$cell_now)]
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
