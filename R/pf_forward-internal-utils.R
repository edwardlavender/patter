#' @title PF helper: internal check functions
#' @description These are internal check functions for [`pf_forward()`].
#' @author Edward Lavender
#' @name pf_check
NULL

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

# Define directories for output files in pf_forward()
.pf_forward_dirs <- function(.record) {
  check_named_list(.record)
  check_names(.record, "sink")
  if (is.null(.record$sink)) {
    return(NULL)
  }
  check_dir_exists(.record$sink)
  folder_history <- file.path(.record$sink, "history")
  lapply(folder_history, \(.folder) {
    if (!dir.exists(.folder)) {
      dir.create(.folder)
    }
    check_dir_empty(.folder, action = warn)
  })
  list(history = folder_history)
}

#' @rdname pf_forward-utils
#' @keywords internal

# Implement startup checks and operations
.pf_forward_startup <- function(.rerun, .record) {

  #### Use .rerun, if specified
  # Currently, we assume that input arguments (e.g., .record_opts) are the same on reruns
  if (length(.rerun) > 0L) {
    # Pull startup values
    startup <- .rerun$internal$startup
    # Increment manual iteration counter
    startup$control$iter_m <- startup$control$iter_m + 1L
    # Update history & diagnostics elements
    startup$output$history     <- .rerun$history
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
  # directories to write outputs (may be NULL)
  folders            <- .pf_forward_dirs(.record)
  folder_history     <- folders[["history"]]

  #### Define wrapper functions
  .pf_write_particles_abbr <- function(.particles) {
    # Note this requires .particles$timestep[1] to be available
    .pf_write_particles(.particles = .particles,
                        .sink = folder_history,
                        .filename = .particles$timestep[1],
                        .write = !is.null(.record$sink))
  }

  #### Collate outputs
  list(
    control = list(
      iter_m = iter_m,
      iter_i = iter_i
    ),
    output = list(
      select_cols = !is.null(.record$cols),
      history = history,
      folder_history = folder_history
    ),
    wrapper = list(.pf_write_particles_abbr = .pf_write_particles_abbr)
  )
}

#' @rdname pf_forward-utils
#' @keywords internal

# Define the starting time step for the loop
.pf_forward_start_t <- function(.rerun, .rerun_from) {
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
.pf_forward_increment <- function(.particles, .obs, .t) {
  .particles |>
    lazy_dt(immutable = FALSE) |>
    mutate(timestep = .obs$timestep[.t + 1L]) |>
    select("timestep",
           cell_past = "cell_now",
           x_past = "x_now",
           y_past = "y_now",
           "weight") |>
    as.data.table()
}

#' @rdname pf_forward-utils
#' @keywords internal

# Define particles for the previous time step
.pf_forward_ppast <- function(.particles, .history, .sink, .t, .obs) {
  # Define previous time step
  tp <- .t - 1L
  # Define particles
  if (is.null(.particles)) {
    # Extract .particles from .history list, if available
    # (if `.save = TRUE`)
    if (length(.history) > 0L) {
      .particles <- .history[[tp]]
    } else {
      .particles <- arrow::read_parquet(file.path(.sink, paste0(.obs$timestep[tp], ".parquet")))
    }
  }
  # Modify particles (cell_now at t - 1 becomes cell_past at t)
  .pf_forward_increment(.particles = .particles, .obs = .obs, .t = tp)
}

#' @rdname pf_forward-utils
#' @keywords internal

# Choose whether or not to implement directed sampling
.pf_forward_trial_sampler <- function(.particles, .trial) {
  if (.trial$trial_kick == 0L) {
    return(TRUE)
  } else {
    # Implement sampling if (a) there are zero likelihood cells & (b) the ESS is too low
    contains_zero_lik <- anyv(.particles$lik, 0)
    if (contains_zero_lik) {
      crit <- .pf_diag_ess(.particles, .weight = normalise(.particles$weight * .particles$lik))
      return(crit < .trial$trial_sampler_crit)
    }
  }
  return(FALSE)
}

#' @rdname pf_forward-utils
#' @keywords internal

# Revert to an earlier time step
.pf_forward_revert <- function(.t, .trial_revert_steps) {
  max(c(2L, .t - .trial_revert_steps))
}

#' @rdname pf_forward-utils
#' @keywords internal

# Continue the simulation to the next time step
.pf_forward_continue <- function(.particles, .t, .crit, .trial_revert_crit) {

  # Outcome (A): convergence failure
  # * If there are no particles, return warning + FALSE
  if (fnrow(.particles) == 0L) {
    warn("Convergence error: there are no particles with positive weights at time step {.t}. Returning outputs up to time step {.t}.",
         .envir = environment())
    return(FALSE)
  }

  # Outcome (B): convergence warning
  # * If the ESS is less than the required threshold (but above zero)
  # * ... we throw a warning, but continue
  if (.crit < .trial_revert_crit) {
    warn("Convergence warning: insufficient ESS ({.crit} < {.trial_revert_crit} [`.trial_revert_crit`] ESS) at timestep {.t}.",
         .envir = environment())
  }

  # Option (C): success (continue)
  TRUE

}

#' @rdname pf_forward-utils
#' @keywords internal

# Collate `pf_forward()` outputs
.pf_forward_output <- function(.rerun, .start, .startup, .history, .convergence) {
  .rerun$time[[.startup$control$iter_m]] <- call_timings(.start = .start)
  out  <- list(history = .history,
               path = NULL,
               diagnostics = NULL,
               internal = list(startup = .startup),
               convergence = .convergence,
               time = .rerun$time)
  class(out) <- c(class(out), pf_class)
  out
}
