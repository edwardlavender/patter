#' @title PF set up: list files for PF
#' @description This function creates an ordered `list` of files for [`pf_backward_*()`].
#'
#' @param .root A string that defines the directory in which files are located.
#' @param ... Additional arguments passed to [`list.files()`], such as `pattern`, excluding `full.names`.
#'
#' @return The function returns an ordered `list` of file paths.
#'
#' @examples
#' # Quick implementation of pf_forward()
#' con <- tempdir()
#' pff_folder <- file.path(con, "patter", "pf", "forward")
#' dir.create(pff_folder, recursive = TRUE)
#' out_pff <- pf_forward(.obs = dat_obs(),
#'                       .bathy = dat_gebco(),
#'                       .moorings = dat_moorings, .detection_overlaps = dat_overlaps(),
#'                       .detection_kernels = dat_kernels(),
#'                       .write_opts = list(sink = pff_folder))
#'
#' # List files for pf_backward_*()
#' files <- pf_setup_files(file.path(pff_folder, "history"))
#'
#' # Quick implementation of pf_backward_killer()
#' pfb_folder <- file.path(con, "patter", "pf", "backward")
#' dir.create(pfb_folder, recursive = TRUE)
#' out_pfb <- pf_backward_killer(files,
#'                               .write_history = list(sink = pfb_folder))
#'
#' # List files from pf_backward_*()
#' pf_setup_files(pfb_folder)
#'
#' # Clean up
#' unlink(file.path(con, "patter"), recursive = TRUE)
#'
#' @seealso
#' TO DO
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
  ext <- exts[1]
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

#' @title PF: rerun
#' @export

pf_setup_rerun <- function(.rerun, .revert = 25L) {
  # default `.revert` is bigger than `.trial_revert_steps`
  max(c(1L, length(.rerun[["history"]]) - .revert))
}
