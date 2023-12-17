#' @title PF: Compile particle histories into a [`data.table`]
#' @description This function compiles particle histories into a [`data.table`].
#' @param .history The particle samples, supplied as:
#' * A [`pf-class`] object;
#' * The `history` element of a [`pf-class`] object;
#' * A string that defines the directory containing parquet files;
#' * A list of strings that defines parquet files;
#' @author Edward Lavender
#' @keywords internal

.pf_history_dt <- function(.history, ..., .collect = TRUE) {

  # Handle .pf objects
  if (inherits(.history, "pf")) {
    .history <- .history$history
  }

  # Handle list of data.table inputs
  if (inherits(.history, "list") && inherits(.history[[1]], "data.table")) {
    check_names(.history[[1]], "cell_now")
    out <- .history |> rbindlist()
    return(out)
  }

  # Handle list of file paths
  if (inherits(.history, "list") &&
      inherits(.history[[1]], "character") &&
      length(.history[[1]] == 1L) &&
      file.exists(.history[[1]])) {

    # Option 1: Read individual files
    out <-
      .history |>
      lapply(arrow::read_parquet) |>
      rbindlist()
    return(out)

    # Option 2: Open dataset
    # For speed, assume that all files are located in the same directory
    # .history <- dirname(.history[[1]])
  }

  # Handle folder input
  if (length(.history) == 1L) {
    check_dir(.history)
    check_contents_ext(.history, "parquet")
    out <-
      .history |>
      arrow::open_dataset(...)
    if (.collect) {
      out <-
        out |>
        arrange(.data$timestep) |>
        collect() |>
        as.data.table()
    }
    return(out)
  }

  # Return error on failure
  abort("`.history` should be the path to parquet files or a list of data.tables.")
}
