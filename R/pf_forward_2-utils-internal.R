#' @title PF helper: internal utilities
#' @keywords internal

.pf_util_write <- function(.particles, .diagnostics,
                           .psink, .dsink, .write) {
  if (.write) {
    file <- paste0(t, ".parquet")
    arrow::write_parquet(.particles,
                         sink = file.path(.psink, file))
    arrow::write_parquet(.diagnostics,
                         sink = file.path(.dsink, file))
  }
}
