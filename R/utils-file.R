#' @title Utilities: `file` helpers
#' @description These are simple file system utilities.
#'
#' @param .sink A `character` string that defines the directory in which files are located.
#' @param .folder (optional) A `character` string that defines the name of a sub-folder for which to list files (via [`file_list()`]) or summarise file sizes (via [`file_size()`]).
#' @param ... In [`file_path()`], `...` is passed to [`file.path()`]. Otherwise, `...` is a placeholder for additional arguments passed to [`list.files()`], such as `pattern`, excluding `full.names`.
#' @param .unit For [`file_size()`], `.unit` is a `character` string that defines the units of the output (`MB`, `GB`, `TB`).
#'
#' @details
#' * [`file_path()`] is a simple wrapper for [`file.path()`] constructs a file path and verifies that it exists.
#' *  [`file_list()`] creates an ordered `list` of numbered files. This function expects files to be named `1.{.ext}`, `2.{.ext}`, ..., `N.{.ext}`. All listed files must share the same file extension.
#' * [`file_size()`] calculates the total size of files in a directory.
#' * [`file_cleanup()`] deletes temporary files and or directories recursively;
#'
#' @return
#' * [`file_path()`] returns a `character` string that defines the file path.
#' * [`file_list()`] returns an ordered vector of file paths.
#' * [`file_size()`] returns a number.
#' * [`file_cleanup()`] returns `invisible(NULL)`.
#'
#' @example man/examples/example-file_path.R
#' @author Edward Lavender
#' @name file_path

#' @rdname file_path
#' @export

file_path <- function(...) {
  .sink <- do.call(file.path, list_compact(list(...)))
  if (!dir.exists(.sink) | !file.exists(.sink)) {
    abort("Path doesn't exist.")
  }
  .sink
}

#' @rdname file_path
#' @export

file_list <- function(.sink, .folder = NULL, ...) {

  #### List files & validate
  # check_dots_used:  list.files() used
  check_dots_allowed("full.names", ...)
  check_dots_for_missing_period(formals(), list(...))
  .sink <- file_path(.sink, .folder)
  files <- list.files(.sink, full.names = TRUE, ...)
  if (length(files) == 0L) {
    abort("No files identified in `.sink`.")
  }
  exts  <- tools::file_ext(files)
  ext   <- exts[1]
  if (fndistinct(exts) != 1L) {
    abort("Multiple file types (extensions) identified in `.sink`. Do you need to pass `pattern` to `list.files()`?")
  }

  #### Define ordered vector of files
  # Define file names
  names   <- as.integer(tools::file_path_sans_ext(basename(files)))
  # Check file names are integers (1, 2, etc.) by coercion
  if (any(is.na(names))) {
    abort("File names should be '1.{{extension}}', '2.{{extension}}', ..., 'N.{{extension}}'.")
  }
  # Order file paths by number
  out <-
    data.table(file = files,
               name = names,
               ext = exts) |>
    lazy_dt(immutable = TRUE) |>
    mutate(name = as.integer(.data$name)) |>
    arrange(.data$name) |>
    as.data.table()
  # Validate ordered list (e.g., to check there are no gaps)
  if (!isTRUE(all.equal(paste0(seq_row(out), ".", ext),
                        basename(out$file)))) {
    abort("File names should be '1.{ext}', '2.{ext}', etc.",
          .envir = environment())
  }
  out$file
}

#' @rdname file_path
#' @export

file_size <- function(.sink,
                      .folder = NULL, ...,
                      .unit = c("MB", "GB", "TB")) {
  # Check inputs
  # check_dots_used:  list.files() used
  check_dots_allowed("full.names", ...)
  check_dots_for_missing_period(formals(), list(...))
  # Get units
  .unit <- match.arg(.unit)
  # Define size in MB
  .sink <- file_path(.sink, .folder)
  size <-
    .sink |>
    list.files(..., full.names = TRUE) |>
    unlist() |>
    file.size() |>
    sum() / 1e6
  # Convert size as requested
  if (.unit == "GB") {
    size <- size / 1e3
  }
  if (.unit == "TB") {
    size <- size / 1e6
  }
  size
}

#' @rdname file_path
#' @export

file_cleanup <- function(.sink) {
  unlink(.sink, recursive = TRUE)
  nothing()
}
