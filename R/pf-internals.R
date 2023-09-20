#' @title PF helper: internal checks
#' @description These are internal check functions.
#' @author Edward Lavender
#' @name pf_check
NULL


#' @rdname pf_check
#' @keywords internal

.pf_check_obs <- function(.o) {
  if (inherits(.o, "data.frame") & !inherits(.o, "data.table")) {
    .o <- as.data.table(.o)
  }
  check_inherits(.o, "data.table")
  if (!rlang::has_name(.o, "timestep")) {
    abort("`.obs` should be a data.table with a `timestep` column. ")
  }
  .o
}

#' @rdname pf_check
#' @keywords internal

.pf_check_write_history <- function(.w) {
  .acs_check_write_record(.w, .con = "sink")
}
