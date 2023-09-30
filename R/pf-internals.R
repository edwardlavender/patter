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
  if (is.unsorted(.o$timestep)) {
    abort("`.obs$timestep` is not sorted.")
  }
  .o
}

#' @rdname pf_check
#' @keywords internal

.pf_check_write_history <- function(.w) {
  .acs_check_write_record(.w, .con = "sink")
}

#' @rdname pf_check
#' @keywords internal

.pf_path_pivot_checks <- function(.obs, .cols) {
  if (is.null(.obs) & !is.null(.cols)) {
    .cols <- NULL
    warn("`.obs = NULL` so input to `.cols` is ignored.")
  }
  if (!is.null(.obs)) {
    if (!rlang::has_name(.obs, "timestep")) {
      abort("`.obs` must have a `timestep` column.")
    }
    if (is.null(.cols)) {
      abort("You must specify the columns in `.obs` required in the output (via `.cols`).")
    }
    check_inherits(.cols, "character")
    if (!all(.cols %in% colnames(.obs))) {
      abort("All elements in `.cols` must be column names in `.obs`.")
    }
  }
}

#' @title PF: path reconstruction helpers
#' @description These functions facilitate the reconstruction of movement paths.
#' @param .data,.current.,t.,pb Arguments for `.pf_path_join`.
#' @param .history,.read Arguments for `.pf_path_chain`.
#' @author Edward Lavender
#' @name pf_path_

#' @rdname pf_path_
#' @keywords internal

.pf_path_join <-
  function(.data, .current, .t, .pb = NULL) {
    # print(.t)
    if (!is.null(.pb)) {
      .pb$tick()
    }
    collapse::join(.data,
                   .current |>
                     select("x{.t - 1}" := "cell_past", "x{.t}" := "cell_now"),
                   how = "right",
                   validate = "m:m",
                   on = paste0("x", .t - 1),
                   verbose = FALSE)
  }

#' @rdname pf_path_
#' @keywords internal

.pf_path_chain <-
  function(.history, .read){
    if (length(.history) <= 2L) {
      abort("There are <= 2 steps in the time series.")
    }
    index <- 2:length(.history)
    if (.read) {
      history_for_index <- paste0("arrow::read_parquet(.history[[", index, "]])")
    } else {
      history_for_index <- paste0(".history[[", index, "]]")
    }
    paste0(
      ".history[[1]] |> \n",
      paste(paste0("  .pf_path_join(", history_for_index, ", .t = ", index, ", .pb)"), collapse = " |> \n")
    )
  }
