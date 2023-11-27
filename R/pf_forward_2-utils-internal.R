#' @title PF helper: internal utilities
#' @keywords internal
#' @name pf_forward_2_utils

#' @rdname pf_forward_2_utils
#' @keywords internal

.pf_utils_dirs <- function(.write_opts) {
  if (is.null(.write_opts)) {
    return(NULL)
  }
  check_named_list(.write_opts)
  check_names(.write_opts, "sink")
  check_dir(.write_opts$sink)
  folder_history <- file.path(.write_opts$sink, "history")
  folder_diagnostics <- file.path(.write_opts$sink, "diagnostics")
  lapply(c(folder_history, folder_diagnostics), \(.folder) {
    if (!dir.exists(.folder)) {
      dir.create(.folder)
    }
    check_dir_is_empty(.folder)
  })
  list(history = folder_history,
       diagnostics = folder_diagnostics)
}

#' @rdname pf_forward_2_utils
#' @keywords internal

.pf_util_startup <- function(.rerun, .obs, .lonlat, .bathy, .moorings,
                             .detection_overlaps, .detection_kernels, .update_ac,
                             .write_opts) {

  #### Use .rerun, if specified
  if (!is.null(.rerun)) {
    startup <- .rerun$internal$startup
    startup$history     <- .rerun$history
    startup$diagnostics <- .rerun$diagnostics
    return(startup)
  }

  #### Define output containers
  # Lists to hold outputs
  history     <- list()
  diagnostics <- list()
  # directories to write outputs (may be NULL)
  folders            <- .pf_utils_dirs(.write_opts)
  folder_history     <- folders[[".history"]]
  folder_diagnostics <- folders[["diagnostics"]]

  #### Prepare algorithm inputs
  # Prepare land filter
  is_land <- spatContainsNA(.bathy)
  # Prepare AC filter
  # * Check moorings & coerce onto grid
  if (!is.null(.moorings)) {
    .moorings <- check_moorings(.moorings, .lonlat = .lonlat, .bathy = .bathy)
  }

  #### Define wrapper functions
  pf_lik_w <- function(.particles, .t, .trial = NA_integer_) {
    pf_lik(.particles = .particles, .obs = .obs, .t = .t, .bathy = .bathy,
           .is_land = is_land,
           .moorings = .moorings,
           .detection_overlaps = .detection_overlaps, .detection_kernels = .detection_kernels,
           .update_ac = .update_ac,
           .trial = .trial)
  }

  #### Collate outputs
  list(
    output = list(
      history = history,
      diagnostics = diagnostics,
      folder_history = folder_history,
      folder_diagnostics = folder_diagnostics
    ),
    data = list(
      .moorings = .moorings
    ),
    control = list(
      is_land = is_land
    ),
    wrapper = list(pf_lik = pf_lik_w)
  )
}

#' @rdname pf_forward_2_utils
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

#' @rdname pf_forward_2_utils
#' @keywords internal

pf_util_next <- function(.particles) {
  # Current cells become past cells for next time step
  .particles |>
    mutate(timestep = .data$timestep + 1L) |>
    select("timestep",
           cell_past = "cell_now",
           x_past = "x_now",
           y_past = "y_now") |>
    as.data.table()
}

