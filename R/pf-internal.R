#' @title PF: internal utilities
#' @description These functions are shared internal utilities that support particle filtering routines, especially [`pf_forward()`] and [`pf_backward_*()`].
#' @author Edward Lavender
#' @name pf_utils-internal

#' @rdname pf_utils-internal
#' @keywords internal

# Define .sink folder for pf_files()
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

#' @rdname pf_utils-internal
#' @keywords internal

# Snapshot data.tables for saving in memory or to file
.pf_snapshot <- function(.dt, .save, .select, .cols) {
  # Copy data
  # * If saved in memory
  # * If .select = FALSE
  # * (With .select = FALSE, we copy the data via .select)
  if (.save && !.select) {
    .dt <- copy(.dt)
  }
  # Subset columns (to reduce file size)
  # * Use immutable = TRUE
  # * This is necessary b/c the snapshot data may not contain all columns
  # * ... required for the function to run (e.g., `weight`) & we need to
  # * ... keep these columns in the input object
  if (.select) {
    .dt <-
      .dt |>
      lazy_dt(immutable = TRUE) |>
      select(any_of(.cols)) |>
      as.data.table()
  }
  .dt
}

#' @rdname pf_utils-internal
#' @keywords internal

# Write files (particles) to output directory
.pf_write_particles <- function(.particles, .sink, .filename, .write) {
  if (.write) {
    arrow::write_parquet(.particles, sink = file.path(.sink, paste0(.filename, ".parquet")))
  }
}
