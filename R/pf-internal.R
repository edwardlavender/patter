#' @title PF: collate particle histories
#' @description This function collates particle histories into a single [`data.table`].
#' @param .history The particle samples, supplied as:
#' * A [`pf_particles-class`] object;
#' * The `history` element of a [`pf_particles-class`] object;
#' * A `character` string that defines the directory containing parquet files;
#' * A `list` of file paths (e.g., from [`pf_files()`]);
#' @param ... If `.history` is a directory, additional arguments can be passed to [`arrow::open_dataset()`] via `...`.
#' @param .collect If `.history` is a directory, `.collect` is a `logical` variable that defines whether or not to collect the dataset in memory.
#'
#' @details
#'
#' 1. If `.history` is a [`pf_particles-class`] object, or the `history` element of such an object, `data.table::rbindlist(..., fill = TRUE)` is used to combine particle samples.
#'
#' 2. If `.history` is a folder, [`arrow::open_dataset()`] plus (optionally) [`dplyr::collect()`] is used. Additional arguments, such as `schema` can be passed to [`arrow::open_dataset()`] via `...`.
#'
#' 3. If `.history` is a `list` of parquet files, files are iteratively read into memory (via [`arrow::read_parquet()`]) and combined (via `data.table::rbindlist(..., fill = TRUE)`. This option is much slower than option 2.
#'
#' @return The function returns a [`data.table`] or an [`arrow::FileSystemDataset`] (if `.history` is a directory and `.collect = FALSE`).
#'
#' @examples
#' \dontrun{
#' require(arrow)
#'
#' # Use pf_particles-class object
#' .pf_history_dt(dat_pfbk())
#'
#' # Use history element
#' .pf_history_dt(dat_pfbk()$history)
#'
#' # Use directory (and optionally select a subset of columns)
#' pfbk_folder <- dat_pfbk_
#' sch <- schema(timestep = int32(),
#'               cell_now = int32(),
#'               x_now = double(),
#'               y_now = double())
#' .pf_history_dt(pfbk_folder, schema = sch)
#'
#' # Use directory with `.collect = FALSE`
#' .pf_history_dt(pfbk_folder, schema = sch, .collect = FALSE)
#'
#' # Use a file list
#' .pf_history_dt(pf_files(pfbk_folder))
#' }
#' @author Edward Lavender
#' @keywords internal

.pf_history_dt <- function(.history, ..., .collect = TRUE) {

  # Handle .pf objects
  if (inherits(.history, pf_class)) {
    .history <- .history$history
  }

  # Handle list of data.table inputs
  if (inherits(.history, "list") && inherits(.history[[1]], "data.table")) {
    check_names(.history[[1]], "cell_now")
    out <- .history |> rbindlist(fill = TRUE)
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
      rbindlist(fill = TRUE)
    return(out)

    # Option 2: Open dataset
    # For speed, assume that all files are located in the same directory
    # .history <- dirname(.history[[1]])
  }

  # Handle folder input
  if (length(.history) == 1L) {
    check_dir_exists(.history)
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
  abort("`.history` should be a pf_particles-class object, the `history` element of a pf_particles-class object, a list of file paths or a directory containing history (.parquet) files.")
}
