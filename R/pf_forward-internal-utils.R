#' @title PF helper: internal check functions
#' @description These are internal check functions for [`pf_forward()`].
#' @author Edward Lavender
#' @name pf_check
NULL

#' @rdname pf_check
#' @keywords internal

# Collate .pf_checks() for pf_forward()
.pf_checks <- function(inputs = match.call()[-1L], defaults = formals(), dots) {
  check_dots_for_missing_period(formals, dots)
}

#' @rdname pf_check
#' @keywords internal

# Check `.obs` data.table
.pf_check_obs <- function(.obs) {
  if (inherits(.obs, "data.frame") & !inherits(.obs, "data.table")) {
    .obs <- as.data.table(.obs)
  }
  check_inherits(.obs, "data.table")
  if (!rlang::has_name(.obs, "timestep")) {
    abort("`.obs` should be a data.table with a `timestep` column. ")
  }
  if (is.unsorted(.obs$timestep)) {
    abort("`.obs$timestep` is not sorted.")
  }
  .obs
}

#' @title PF helper: internal utilities
#' @description These functions are internal utilities for [`pf_forward()`].
#' @author Edward Lavender
#' @keywords internal
#' @name pf_forward-utils

#' @rdname pf_forward-utils
#' @keywords internal

# Define directories for output files
.pf_dirs <- function(.record) {
  check_named_list(.record)
  check_names(.record, "sink")
  if (is.null(.record$sink)) {
    return(NULL)
  }
  check_dir_exists(.record$sink)
  folder_history <- file.path(.record$sink, "history")
  folder_diagnostics <- file.path(.record$sink, "diagnostics")
  lapply(c(folder_history, folder_diagnostics), \(.folder) {
    if (!dir.exists(.folder)) {
      dir.create(.folder)
    }
    check_dir_is_empty(.folder, action = warn)
  })
  list(history = folder_history,
       diagnostics = folder_diagnostics)
}

#' @rdname pf_forward-utils
#' @keywords internal

# Write files (particles) to output directory
.pf_write_particles <- function(.particles, .sink, .write) {
  if (.write) {
    file <- paste0(.particles$timestep[1], ".parquet")
    arrow::write_parquet(.particles, sink = file.path(.sink, file))
  }
}

#' @rdname pf_forward-utils
#' @keywords internal

# Write files (diagnostics) to output directory
.pf_write_diagnostics <- function(.diagnostics, .sink, .write) {
  if (.write) {
    file <- paste0(
      paste(.diagnostics$iter_m[1],
             .diagnostics$iter_i[1],
             .diagnostics$timestep[1], sep = "-"),
      ".parquet")
    arrow::write_parquet(.diagnostics, sink = file.path(.sink, file))
  }
}

#' @rdname pf_forward-utils
#' @keywords internal

# Implement startup checks and operations
.pf_startup <- function(.obs, .dlist, .rargs, .dargs, .rerun, .record) {

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
  # TO DO, use .pf_check()

  #### Prepare controls
  # Number of manual iterations
  iter_m <- 1L
  # Number of internal iterations
  iter_i <- 1L

  #### Define output containers
  # Lists to hold outputs
  history     <- list()
  diagnostics <- list()
  # directories to write outputs (may be NULL)
  folders            <- .pf_dirs(.record)
  folder_history     <- folders[["history"]]
  folder_diagnostics <- folders[["diagnostics"]]

  #### Define wrapper functions
  .pf_write_particles_abbr <- function(.particles) {
    .pf_write_particles(.particles = .particles, .sink = folder_history, .write = !is.null(.record$sink))
  }
  .pf_write_diagnostics_abbr <- function(.diagnostics) {
    .pf_write_diagnostics(.diagnostics = .diagnostics, .sink = folder_diagnostics, !is.null(.record$sink))
  }

  #### Collate outputs
  list(
    control = list(
      iter_m = iter_m,
      iter_i = iter_i
    ),
    output = list(
      .record = .record,
      select_cols = !is.null(.record$cols),
      history = history,
      diagnostics = diagnostics,
      folder_history = folder_history,
      folder_diagnostics = folder_diagnostics
    ),
    wrapper = list(.pf_write_particles_abbr = .pf_write_particles_abbr,
                   .pf_write_diagnostics_abbr = .pf_write_diagnostics_abbr)
  )
}

#' @rdname pf_forward-utils
#' @keywords internal

# Snapshot data.tables for saving in memory or to file
.pf_snapshot <- function(.dt,.save, .select, .cols) {
  # Copy & drop attributes
  # * This is necessary if we save objects in memory only
  if (.save) {
    .dt <- copy(data.table(.dt))
  }
  # Subset columns (to reduce file size)
  if (.select) {
    .dt <-
      .dt |>
      select(any_of(.cols)) |>
      as.data.table()
  }
  .dt
}

#' @rdname pf_forward-utils
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

#' @rdname pf_forward-utils
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

#' @rdname pf_forward-utils
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

#' @rdname pf_forward-utils
#' @keywords internal

# Choose whether or not to implement directed sampling
.pf_trial_sampler <- function(.diagnostics, .trial_crit) {
 if (!rlang::has_name(.diagnostics, "kick")) {
   return(TRUE)
 } else {
   pos  <- fnrow(.diagnostics[["kick"]])
   crit <- .diagnostics[["kick"]]$n_u[pos]
   return(crit < .trial_crit)
 }
}

#' @rdname pf_forward-utils
#' @keywords internal

# Revert to an earlier time step
.pf_revert <- function(.t, .trial_revert_steps) {
  max(c(2L, .t - .trial_revert_steps))
}

#' @rdname pf_forward-utils
#' @keywords internal

# Continue the simulation to the next time step
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

#' @rdname pf_forward-utils
#' @keywords internal

# Collate `pf_forward()` outputs
.pf_outputs <- function(.rerun, .start, .startup, .history, .diagnostics, .convergence) {
  .rerun$time[[.startup$control$iter_m]] <- call_timings(.start = .start)
  out  <- list(history = .history,
               diagnostics = .pf_diag_bind(.diagnostics),
               internal = list(startup = .startup),
               convergence = .convergence,
               time = .rerun$time)
  class(out) <- c(class(out), pf_class)
  out
}
