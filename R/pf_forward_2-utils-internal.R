#' @title PF helper: internal utilities
#' @keywords internal
#' @name pf_forward_2_utils

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
    check_dir_is_empty(.folder)
  })
  list(history = folder_history,
       diagnostics = folder_diagnostics)
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Write files (particles, diagnostics) to output directories
.pf_write <- function(.particles, .diagnostics,
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

.pf_startup <- function(.rerun, .obs, .lonlat, .bathy, .moorings,
                        .detection_overlaps, .detection_kernels, .update_ac,
                        .write_opts) {

  #### Use .rerun, if specified
  if (!is.null(.rerun)) {
    startup <- .rerun$internal$startup
    startup$output$history     <- .rerun$history
    startup$output$diagnostics <- .rerun$diagnostics
    return(startup)
  }

  #### Define output containers
  # Lists to hold outputs
  history     <- list()
  diagnostics <- list()
  # directories to write outputs (may be NULL)
  folders            <- .pf_dirs(.write_opts)
  folder_history     <- folders[[".history"]]
  folder_diagnostics <- folders[["diagnostics"]]

  #### Prepare controls
  # Prepare land filter
  is_land <- spatContainsNA(.bathy)
  # Prepare revert count
  revert_count <- 0L

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
  .pf_write_abbr <- function(.particles, .diagnostics) {
    .pf_write(.particles = .particles, .diagnostics = diagnostics,
                   .psink = folder_history, .dsink = folder_diagnostics,
                   .write = !is.null(.write_opts))
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
      is_land = is_land,
      revert_count = revert_count
    ),
    wrapper = list(.pf_lik_abbr = .pf_lik_abbr,
                   .pf_write_abbr = .pf_write_abbr)
  )
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Define the starting time step for the loop
.pf_start_t <- function(.rerun, .rerun_from) {
  if (is.null(.rerun)) {
    t <- 2L
  } else {
    t <- max(c(2L, .rerun_from))
  }
  t
}

#' @rdname pf_forward_2_utils
#' @keywords internal

# Move loop on so that current cells become past cells @ next time step
.pf_next <- function(.particles) {
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
# * This is necessary for the first step in pf_forward_2()
.pf_ppast <- function(.particles, .history, .t) {
  if (.t == 2L) {
    ppast <- .pf_next(.particles)
  } else {
    # TO DO: read .history[[.t - 1L]] here
    ppast <- copy(.history[[.t - 1L]])
  }
}

#' @rdname pf_forward_2_utils
#' @keywords internal

.pf_trial_sampler <- function(.diagnostics, .trial_crit) {
  opt_1 <- length(.diagnostics) == 0L
  if (opt_1) {
    return(TRUE)
  }
  pos <- length(.diagnostics[["kick"]])
  crit <- .diagnostics[["kick"]][[pos]]$n_u
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

.pf_outputs <- function(.start, .startup, .history, .diagnostics, .convergence) {
  time <- call_timings(.start)
  out  <- list(history = .history,
               diagnostics = .diagnostics,
               internal = list(startup = .startup),
               convergence = .convergence,
               time = time)
  class(out) <- c(class(out), "pf")
  out
}
