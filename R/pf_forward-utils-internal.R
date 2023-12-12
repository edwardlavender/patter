#' @title PF helper: internal utilities
#' @keywords internal
#' @name pf_forward_2_utils


#' @rdname pf_forward_2_utils
#' @keywords internal

# Check .record_opts
.pf_record_opts <- function(.record_opts) {
  out <- list_args(.args = .record_opts, .defaults = list(save = FALSE,
                                                          cols = NULL,
                                                          sink = NULL))
  if (!out$save && is.null(out$sink)) {
    abort("`.record_opts$save = FALSE` and `.record_opts$sink = NULL`. There is nothing to do.")
  }
  out
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Define directories for output files
.pf_dirs <- function(.write_opts) {
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
    check_dir_is_empty(.folder, action = warn)
  })
  list(history = folder_history,
       diagnostics = folder_diagnostics)
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Write files (particles) to output directory
.pf_write_particles <- function(.particles, .sink, .write) {
  if (.write) {
    file <- paste0(.particles$timestep[1], ".parquet")
    arrow::write_parquet(.particles, sink = file.path(.sink, file))
  }
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Write files (diagnostics) to output directory
.pf_write_diagnostics <- function(.diagnostics, .sink, .write) {
  if (.write) {
    file <- paste0(
      paste0(.diagnostics$iter_m[1],
             .diagnostics$iter_i[1],
             .diagnostics$timestep[1], collapse = "-"),
      ".parquet")
    arrow::write_parquet(.diagnostics, sink = file.path(.sink, file))
  }
}

#' @rdname pf_forward_2_utils
#' @keywords internal

.pf_startup <- function(.rerun, .obs, .lonlat, .bathy, .moorings,
                        .detection_overlaps, .detection_kernels, .update_ac,
                        .record_opts) {

  #### Use .rerun, if specified
  # Currently, we assume that input arguments (e.g., .record_opts) are the same on reruns
  if (length(.rerun) > 0L) {
    # Pull startup values
    startup <- .rerun$internal$startup
    # Increment manual iteration counter
    startup$control$iter_m <- startup$control$iter_m + 1L
    # Update history & diagnostics elements
    startup$output$history     <- .rerun$history
    startup$output$diagnostics <- list(.rerun$diagnostics)
    return(startup)
  }

  #### Validate inputs
  .record_opts <- .pf_record_opts(.record_opts)

  #### Define output containers
  # Lists to hold outputs
  history     <- list()
  diagnostics <- list()
  # directories to write outputs (may be NULL)
  folders            <- .pf_dirs(.record_opts)
  folder_history     <- folders[["history"]]
  folder_diagnostics <- folders[["diagnostics"]]

  #### Prepare controls
  # Number of manual iterations
  iter_m <- 1L
  # Number of internal iterations
  iter_i <- 1L
  # Prepare land filter
  is_land <- spatContainsNA(.bathy)

  #### Prepare data
  # Check moorings & coerce onto grid
  if (!is.null(.moorings)) {
    .moorings <- check_moorings(.moorings, .lonlat = .lonlat, .bathy = .bathy)
  }

  #### Define wrapper functions
  .pf_lik_abbr <- function(.particles, .t, .trial = NA_integer_) {
    pf_lik(.particles = .particles, .obs = .obs, .t = .t, .bathy = .bathy,
           .is_land = is_land,
           .moorings = .moorings,
           .detection_overlaps = .detection_overlaps, .detection_kernels = .detection_kernels,
           .update_ac = .update_ac,
           .trial = .trial)
  }
  .pf_write_particles_abbr <- function(.particles) {
    .pf_write_particles(.particles = .particles, .sink = folder_history, .write = !is.null(.record_opts$sink))
  }
  .pf_write_diagnostics_abbr <- function(.diagnostics) {
    .pf_write_diagnostics(.diagnostics = .diagnostics, .sink = folder_diagnostics, !is.null(.record_opts$sink))
  }

  #### Collate outputs
  list(
    output = list(
      .record_opts = .record_opts,
      select_cols = !is.null(.record_opts$cols),
      history = history,
      diagnostics = diagnostics,
      folder_history = folder_history,
      folder_diagnostics = folder_diagnostics
    ),
    data = list(
      .moorings = .moorings
    ),
    control = list(
      iter_m = iter_m,
      iter_i = iter_i,
      is_land = is_land
    ),
    wrapper = list(.pf_lik_abbr = .pf_lik_abbr,
                   .pf_write_particles_abbr = .pf_write_particles_abbr,
                   .pf_write_diagnostics_abbr = .pf_write_diagnostics_abbr)
  )
}


#' @rdname pf_forward_2_utils
#' @keywords internal

# Snapshot data.tables for saving in memory or to file
.pf_snapshot <- function(.dt, .select, .cols) {
  # Copy & drop attributes
  dt <- copy(data.table(.dt))
  # Subset columns (to reduce file size)
  if (.select) {
    dt <-
      dt |>
      select(any_of(.cols)) |>
      as.data.table()
  }
  dt
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Define the starting time step for the loop
.pf_start_t <- function(.rerun, .rerun_from) {
  if (length(.rerun) == 0L) {
    t <- 2L
  } else {
    t <- max(c(2L, .rerun_from))
  }
  t
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Increment loop: current cells become past cells @ next time step
.pf_increment <- function(.particles) {
  .particles |>
    lazy_dt() |>
    mutate(timestep = .data$timestep + 1L) |>
    select("timestep",
           cell_past = "cell_now",
           x_past = "x_now",
           y_past = "y_now") |>
    as.data.table()
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Define particles for the previous time step
.pf_ppast <- function(.particles, .history, .sink, .t) {
  # Define previous time step
  tp <- .t - 1L
  # Define particles
  if (is.null(.particles)) {
    # Extract .particles from .history list, if available
    # (if `.save_opts` = TRUE)
    if (length(.history) > 0L) {
      .particles <- .history[[tp]]
    } else {
      .particles <- arrow::read_parquet(file.path(.sink, paste0(tp, ".parquet")))
    }
  }
  # Modify particles (cell_now at t - 1 becomes cell_past at t)
  .pf_increment(.particles)
}

#' @rdname pf_forward_2_utils
#' @keywords internal

.pf_trial_sampler <- function(.diagnostics, .trial_crit) {
  opt_1 <- length(.diagnostics) == 0L
  if (opt_1) {
    return(TRUE)
  }
  pos  <- fnrow(.diagnostics[["kick"]])
  crit <- .diagnostics[["kick"]]$n_u[pos]
  crit < .trial_crit
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Revert to an earlier time step
.pf_revert <- function(.t, .trial_revert_steps) {
  max(c(2L, .t - .trial_revert_steps))
}

#' @rdname pf_forward_2_utils
#' @keywords internal

.pf_continue <- function(.particles, .t, .crit, .trial_revert_crit) {

  # Outcome (A): convergence failure
  # * If there are no particles, return warning + FALSE
  if (nrow(.particles) == 0L) {
    warn("Convergence error: there are no particles with positive weights at time step {.t}. Returning outputs up to time step {.t}.",
         .envir = environment())
    return(FALSE)
  }

  # Outcome (B): convergence warning
  # * If the number of particles is less than the required threshold (but above zero)
  # * ... we throw a warning, but continue
  if (.crit < .trial_revert_crit) {
    warn("Convergence warning: insufficient particles ({.crit} < {.trial_revert_crit} [`.trial_revert_crit`] particles) at timestep {.t}.",
         .envir = environment())
  }

  # Option (C): success (continue)
  TRUE

}

#' @rdname pf_forward_2_utils
#' @keywords internal

.pf_outputs <- function(.rerun, .start, .startup, .history, .diagnostics, .convergence) {
  .rerun$time[[.startup$control$iter_m]] <- call_timings(.start = .start)
  out  <- list(history = .history,
               diagnostics = .pf_diag_bind(.diagnostics),
               internal = list(startup = .startup),
               convergence = .convergence,
               time = .rerun$time)
  class(out) <- c(class(out), "pf")
  out
}
