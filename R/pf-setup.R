#' @title PF set up: list files for PF
#' @description This function creates an ordered `list` of files for [`pf_backward_*()`].
#'
#' @param .root A string that defines the directory in which files are located.
#' @param ... Additional arguments passed to [`list.files()`], such as `pattern`, excluding `full.names`.
#'
#' @return The function returns an ordered `list` of file paths.
#'
#' @examples
#' # List files from pf_forward()
#' pff_folder <- system.file("extdata", "acpf", "forward", "history",
#'                           package = "patter", mustWork = TRUE)
#' files <- pf_setup_files(pff_folder)
#'
#'
#' # List files from pf_backward_killer()
#' pfbk_folder <- system.file("extdata", "acpf", "backward", "killer",
#'                            package = "patter", mustWork = TRUE)
#' files <- pf_setup_files(pfbk_folder)
#'
#' @seealso
#'
#' @author Edward Lavender
#' @export

pf_setup_files <- function(.root, ...) {
  # Check inputs
  check_dir(input = .root)
  check_dots_allowed("full.names", ...)
  check_dots_for_missing_period(formals(), list(...))
  files <- list.files(.root, full.names = TRUE, ...)
  if (length(files) == 0L) {
    abort("No files identified in `.root`.")
  }
  exts  <- tools::file_ext(files)
  if (length(unique(exts)) != 1L) {
    abort("Multiple file types (extensions) identified in `.root`. Do you need to pass `pattern` to `list.files()`?")
  }
  if (!all(exts == "parquet")) {
    abort(".parquet files (for `pf_backward_*()`) are expected.")
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
