#' @title PF set up: List files from AC* for PF
#' @description This function creates an ordered list of 'record' files derived from an AC* algorithm (e.g., [`acs()`]) for particle filtering (via [`pf()`]).
#'
#' @param .root A string that defines the directory in which files are located.
#' @param ... Additional arguments passed to [`list.files()`], such as `pattern`, excluding `full.names`.
#'
#' @return The function returns an ordered character vector of files.
#'
#' @examples
#' # Quick implementation of AC algorithm
#' acoustics <- dat_acoustics[individual_id == 25, ]
#' archival <- dat_archival[individual_id == 25, ]
#' obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
#' obs <- obs[1:10, ]
#' gebco <- dat_gebco()
#' dat_moorings$receiver_range <- 500
#' containers <- acs_setup_detection_containers(gebco, dat_moorings)
#' # (For speed only, ignore overlaps)
#' # overlaps <- acs_setup_detection_overlaps(containers, dat_moorings)
#' kernels <-
#'   acs_setup_detection_kernels(dat_moorings,
#'                               .calc_detection_pr = acs_setup_detection_pr,
#'                               .bathy = gebco)
#' folder <- file.path(tempdir(), "ac")
#' dir.create(folder)
#' out_ac <-
#'   acs(obs,
#'       .bathy = gebco,
#'       .detection_kernels = kernels,
#'       .write_record = list(filename = folder, overwrite = TRUE))
#'
#' # List files
#' pf_setup_record(folder)
#'
#' # Clean up
#' unlink(folder, recursive = TRUE)
#'
#'
#' @seealso This function is designed to list outputs from [`acs()`] (see the `.write_record` argument) for \code{\link[flapper]{pf}} (see the \code{.record} argument).
#'
#' @author Edward Lavender
#' @export

pf_setup_record <- function(.root, ...) {
  # Check inputs
  check_dir(input = .root)
  check_dots_allowed("full.names", ...)
  check_dots_for_missing_period(formals(), list(...))
  files <- list.files(.root, full.names = TRUE, ...)
  if (length(unique(tools::file_ext(files))) != 1L) {
    warn("Multiple file types (extensions) identified in `.root`. Do you need to pass `pattern` to `list.files()`?")
  }
  bsname <- basename(files)
  if (!isTRUE(all.equal(sort(paste0(seq_len(length(files)), ".tif")), bsname))) {
    warn("Files do not match expected naming convention ('1.tif', '2.tif', ..., 'N.tif' where N is the number of files.)")
  }
  # Define ordered vector of files
  data.table(file = files,
             name = tools::file_path_sans_ext(bsname),
             ext = tools::file_ext(bsname)) |>
    lazy_dt(immutable = TRUE) |>
    mutate(name = as.integer(.data$name)) |>
    arrange(.data$name) |>
    dplyr::pull(.data$file)
}
