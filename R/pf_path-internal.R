#' @title PF: path reconstruction helpers
#' @description These functions facilitate the reconstruction of movement paths.
#' @param .obs,.cols Arguments for [`.pf_path_pivot_checks()`]
#' @param .data,.current.,t.,pb Arguments for [`.pf_path_join()`].
#' @param .history,.read Arguments for [`.pf_path_chain()`].
#' @author Edward Lavender
#' @name pf_path_

#' @rdname pf_path_
#' @keywords internal

# Check `.obs` and `.cols` inputs
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

#' @rdname pf_path_
#' @keywords internal

# Join current particle samples to a data.table
.pf_path_join <-
  function(.data, .current, .t, .pb = NULL, .pb_step) {
    # Monitor progress
    # print(.t)
    # Use Sys.sleep() for testing (e.g., visualise progress bar)
    # Sys.sleep(0.1)
    # print(.t)
    pb_tick(.pb = .pb, .t = .pb_step)
    collapse::join(.data,
                   .current |>
                     select("x{.t - 1}" := "cell_past", "x{.t}" := "cell_now") |>
                     as.data.table(),
                   how = "right",
                   validate = "m:m",
                   on = paste0("x", .t - 1),
                   verbose = FALSE)
  }

#' @rdname pf_path_
#' @keywords internal

# Define text to chain particle samples together via `.pf_path_join()`
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
      paste(paste0("  .pf_path_join(", history_for_index, ", .t = ", index,
                   ",.pb = .pb, .pb_step = ", rev(index), ")"),
            collapse = " |> \n")
      )
  }
