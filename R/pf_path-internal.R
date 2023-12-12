#' @title PF: path reconstruction helpers
#' @description These functions facilitate the reconstruction of movement paths.
#' @param .data,.current.,t.,pb Arguments for `.pf_path_join`.
#' @param .history,.read Arguments for `.pf_path_chain`.
#' @author Edward Lavender
#' @name pf_path_

#' @rdname pf_path_
#' @keywords internal

.pf_path_join <-
  function(.data, .current, .t, .pb = NULL, .pb_step) {
    # Monitor progress
    # print(.t)
    # Use Sys.sleep() for testing (e.g., visualise progress bar)
    # Sys.sleep(0.1)
    # print(.t)
    pb_tick(.pb = .pb, .t = .pb_step, .progress = !is.null(.pb))
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
