#' @title PF: list files
#' @description [`pf_files()`] creates an ordered `list` of the parquet files that contain particle samples (e.g., from [`pf_forward()`] or [`pf_backward_killer()`]).
#'
#' [`pf_files_size()`] calculates the total size of all files.
#'
#' @param .sink A `character` string that defines the directory in which files are located.
#' @param ... For [`pf_files()`], `...` is a placeholder for additional arguments passed to [`list.files()`], such as `pattern`, excluding `full.names`.
#' @param .unit For [`pf_files_size()`], `.unit` is a `character` string that defines the units of the output (`MB`, `GB`, `TB`).
#'
#' @return
#' * [`pf_files()`] returns an ordered `list` of file paths.
#' * [`pf_files_size()`] returns a number.
#'
#' @examples
#' # Use `pf_files()` to list files from `pf_forward()`
#' pff_folder <- dat_pff_src()
#' files <- pf_files(pff_folder)
#'
#' # Use `pf_files()` to list files from `pf_backward_killer()`
#' pfbk_folder <- dat_pfbk_src()
#' files <- pf_files(pfbk_folder)
#'
#' # Use `pf_files_size()` to calculate file size
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

pf_files <- function(.sink, ...) {
  # Check inputs
  check_dir(input = .sink)
  check_dots_allowed("full.names", ...)
  check_dots_for_missing_period(formals(), list(...))
  rlang::check_dots_used()
  files <- list.files(.sink, full.names = TRUE, ...)
  if (length(files) == 0L) {
    abort("No files identified in `.sink`.")
  }
  exts  <- tools::file_ext(files)
  if (length(unique(exts)) != 1L) {
    abort("Multiple file types (extensions) identified in `.sink`. Do you need to pass `pattern` to `list.files()`?")
  }
  if (!all(exts == "parquet")) {
    abort(".parquet files are expected.")
  }
  bsname <- basename(files)
  # Define ordered vector of files
  data.table(file = files,
             name = tools::file_path_sans_ext(bsname),
             ext = exts) |>
    lazy_dt(immutable = TRUE) |>
    mutate(name = as.integer(.data$name)) |>
    arrange(.data$name) |>
    pull(.data$file) |>
    as.list()
}

#' @rdname pf_files
#' @export

pf_files_size <- function(.sink, .unit = c("MB", "GB", "TB")) {
  # Get units
  .unit <- match.arg(.unit)
  # Define size in MB
  size <-
    .sink |>
    pf_files() |>
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
