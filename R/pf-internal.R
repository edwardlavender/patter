#' @title PF: access particle histories
#' @description These functions function access or collate particle histories.
#'
#' * [`.pf_history_list()`] `list`s particle histories or file pointers;
#' * [`.pf_history_read()`] distinguishes particle histories in memory versus on file;
#' * [`.pf_history_elm()`] accesses particle histories for a specific `list` element;
#' * [`.pf_history_dt()`] collates particle histories into a single [`data.table`];
#'
#' @param .history The particle samples. In [`.pf_history_list()`] and [`.pf_history_dt()`], these can be supplied as:
#' * A [`pf_particles-class`] object;
#' * The `history` element of a [`pf_particles-class`] object;
#' * A `character` string that defines the directory containing parquet files;
#' * A `list` of file paths (e.g., from [`pf_files()`]);
#'
#' For [`.pf_history_read()`] and [`.pf_history_elm()`], `.history` should be a `list`.
#'
#' All functions also silently accept (and return) `NULL`.
#
#' @param ... Additional arguments if `.history` is a directory;
#' * In [`.pf_history_list()`], `...` is passed to [`pf_files()`];
#' * In [`.pf_history_dt()`]: `...`  is passed to [`arrow::open_dataset()`].
#'
#' @param .elm,.read For [`.pf_history_read()`] and [`.pf_history_elm()`]:
#' * `.elm` is an `integer` that defines the `list` index.
#' * `.read` is a `logical` variable that defines whether or not to read particle samples from file.
#'
#' @param .collect For [`.pf_history_dt()`], if `.history` is a directory, `.collect` is a `logical` variable that defines whether or not to collect the dataset in memory.
#'
#' @details
#'
#' # List particle samples
#'
#' [`.pf_history_list()`] lists particle samples or file pointers. There are four implementation options:
#'
#' 1. If `.history` is a  [`pf_particles-class`] object, the function returns the `history` element.
#' 2. If `.history` is the `history` element of a [`pf_particles-class`] object, it is returned unchanged.
#' 3. If `.history` is a folder, [`pf_files()`] is used to return a `list` of file paths.
#' 4. If `.history` is a list of file paths, they are returned unchanged.
#'
#' This function is used in iterative routines, supported by [`.pf_history_elm()`].
#'
#' # Collate particle samples
#'
#' [`.pf_history_dt()`] collates all particle samples. There are three implementation options:
#'
#' 1. If `.history` is a [`pf_particles-class`] object, or the `history` element of such an object, `data.table::rbindlist(..., fill = TRUE)` is used to combine particle samples.
#' 2. If `.history` is a folder, [`arrow::open_dataset()`] plus (optionally) [`dplyr::collect()`] is used. Additional arguments, such as `schema` can be passed to [`arrow::open_dataset()`] via `...`.
#' 3. If `.history` is a `list` of parquet files, files are iteratively read into memory (via [`arrow::read_parquet()`]) and combined (via `data.table::rbindlist(..., fill = TRUE)`. This option is much slower than option 2.
#'
#' @return
#' If `.history = NULL`, `NULL` is returned. Otherwise:
#'
#' * [`.pf_history_list()`] returns a list of [`data.table`]s (options 1 and 2) or file paths (options 3 and 4);
#' * [`.pf_history_read()`] return a `logical` variable;
#' * [`.pf_history_elm()`] returns a [`data.table`] for particle samples for a single time step;
#' * [`.pf_history_dt()`] returns a [`data.table`] or an [`arrow::FileSystemDataset`] (if `.history` is a directory and `.collect = FALSE`).
#'
#' @examples
#' \dontrun{
#'
#' # .pf_history_dt() examples
#'
#' require(arrow)
#'
#' # Use pf_particles-class object
#' .pf_history_dt(dat_pfbk())
#'
#' # Use `history` element
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
#'
#' @author Edward Lavender
#' @name pf_history

#' @rdname pf_history
#' @keywords internal

# Define a list of particle samples or file pointers
.pf_history_list <- function(.history, ...) {

  # Handle NULL
  if (is.null(.history)) {
    return(NULL)
  }

  # Handle .pf objects
  if (inherits(.history, pf_class)) {
    return(.history$history)
  }

  # Handle list of data.table inputs
  if (inherits(.history, "list") && inherits(.history[[1]], "data.table")) {
    return(.history)
  }

  # Handle list of file paths
  if (inherits(.history, "list") &&
      inherits(.history[[1]], "character") &&
      length(.history[[1]] == 1L) &&
      file.exists(.history[[1]])) {
    return(.history)
  }

  # Handle folder input
  if (length(.history) == 1L && inherits(.history[[1]], "character")) {
    return(pf_files(.history, ...))
  }

  abort("Input to `history` is invalid.")

}

#' @rdname pf_history
#' @keywords internal

# Define whether or not to read particle samples
# * This function expects a list and should follow .pf_history_list()
.pf_history_read <- function(.history) {
  if (is.null(.history)) {
    return(NULL)
  }
  check_inherits(.history, "list")
  if (inherits(.history[[1]], "data.table")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @rdname pf_history
#' @keywords internal

# Define particle samples for a specific .history element e.g., .history[[1]]
# * This function expects a list and should follow .pf_history_list()
.pf_history_elm <- function(.history,
                            .elm,
                            .read = .pf_history_read(.history)) {
  if (is.null(.history)) {
    return(NULL)
  }
  if (.read) {
    return(arrow::read_parquet(.history[[.elm]]))
  } else {
    return(.history[[.elm]])
  }
}

#' @rdname pf_history
#' @keywords internal

# Collate particle histories into a single data.table
.pf_history_dt <- function(.history, ..., .collect = TRUE) {

  # Handle NULL
  if (is.null(.history)) {
    return(NULL)
  }

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
    check_dir_contents_ext(.history, "parquet")
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

#' @title PF: internal utilities
#' @description These functions are internal utilities that support particle filtering routines.
#' @author Edward Lavender
#' @name pf_utils_internal

#' @rdname pf_utils_internal
#' @keywords internal

.pf_sink_folder <- function(.sink, .folder) {
  check_dir_exists(.sink)
  if (!is.null(.folder)) {
    check_inherits(.folder, "character")
    stopifnot(length(.folder) == 1L)
    .sink <- file.path(.sink, .folder)
    check_dir_exists(.sink)
  }
  .sink
}
