#' @title PF: list files
#' @description [`pf_files()`] creates an ordered `list` of numbered files.
#'
#' [`pf_files_size()`] calculates the total size of all files.
#'
#' @param .sink A `character` string that defines the directory in which files are located.
#' @param .folder (optional) A `character` string that defines the name of a sub-folder for which to list files (via [`pf_files()`]) or summarise file sizes (via [`pf_files_size()`]).
#' @param ... A placeholder for additional arguments passed to [`list.files()`], such as `pattern`, excluding `full.names`.
#' @param .unit For [`pf_files_size()`], `.unit` is a `character` string that defines the units of the output (`MB`, `GB`, `TB`).
#'
#' @details
#' [`pf_files()`] expects listed files to be named `1.{.ext}`, `2.{.ext}`, ..., `N.{.ext}`. All listed files must share the same file extension.
#'
#' The function is normally used to to create an ordered `list` of the `parquet` files that contain particle samples (e.g., from [`pf_forward()`] or [`pf_backward_killer()`]), but it can be used in any situation with files named as described above (e.g., to `list` `png` files from [`pf_plot_history()`]).
#'
#' At the time of writing, [`pf_files()`] cannot be used to list particle-diagnostic files from [`pf_forward()`] and should only be used for particle samples. However, [`pf_files_size()`] can be used to estimate the total file size in any directory.
#'
#' @return
#' * [`pf_files()`] returns an ordered `list` of file paths.
#' * [`pf_files_size()`] returns a number.
#'
#' @examples
#' # Use `pf_files()` to list files from `pf_forward()`
#' pff_folder <- dat_pff_src(.folder = NULL)
#' files      <- pf_files(.sink = pff_folder, .folder = "history")
#'
#' # Use `pf_files()` to list files from `pf_backward_killer()`
#' pfbk_folder <- dat_pfbk_src()
#' files       <- pf_files(pfbk_folder)
#'
#' # Use `pf_files_size()` to calculate file size
#' pf_files_size(pff_folder, recursive = TRUE)
#' pf_files_size(pff_folder, .folder = "history")
#' pf_files_size(pff_folder, .folder = "diagnostics")
#' pf_files_size(pfbk_folder)
#' pf_files_size(pfbk_folder, .unit = "GB")
#' pf_files_size(pfbk_folder, .unit = "TB")
#'
#' @inherit pf_forward seealso
#'
#' @author Edward Lavender
#' @name pf_files

#' @rdname pf_files
#' @export

pf_files <- function(.sink, .folder = NULL, ...) {

  #### List files & validate
  # check_dots_used:  list.files() used
  check_dots_allowed("full.names", ...)
  check_dots_for_missing_period(formals(), list(...))
  .sink <- .pf_sink_folder(.sink = .sink, .folder = .folder)
  files <- list.files(.sink, full.names = TRUE, ...)
  if (length(files) == 0L) {
    abort("No files identified in `.sink`.")
  }
  exts  <- tools::file_ext(files)
  if (length(unique(exts)) != 1L) {
    abort("Multiple file types (extensions) identified in `.sink`. Do you need to pass `pattern` to `list.files()`?")
  }
  .ext <- exts[1]
  if (!all(exts == .ext)) {
    abort("The extensions of listed files vary.")
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
  if (!isTRUE(all.equal(paste0(seq_row(out), ".", .ext),
                        basename(out$file)))) {
    abort("File names should be '1.{.ext}', '2.{.ext}', etc.",
          .envir = environment())
  }
  as.list(out$file)
}

#' @rdname pf_files
#' @export

pf_files_size <- function(.sink,
                          .folder = NULL, ...,
                          .unit = c("MB", "GB", "TB")) {
  # Check inputs
  # check_dots_used:  list.files() used
  check_dots_allowed("full.names", ...)
  check_dots_for_missing_period(formals(), list(...))
  # Get units
  .unit <- match.arg(.unit)
  # Define size in MB
  .sink <- .pf_sink_folder(.sink = .sink, .folder = .folder)
  size <-
    .sink |>
    list.files(..., full.names = TRUE) |>
    unlist() |>
    file.size() |>
    sum() / 1e6L
  # Convert size as requested
  if (.unit == "GB") {
    size <- size / 1e3L
  }
  if (.unit == "TB") {
    size <- size / 1e6L
  }
  size
}
