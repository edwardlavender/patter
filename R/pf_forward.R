#' @title PF: forward simulation
#' @export

# notes
# * particles replaced
# * diagnostics retained always, even for failure timesteps
# * other parameters should not change on rerun

pf_forward <- function(.obs,
                         .origin = NULL,
                         .bathy, .lonlat = FALSE,
                         .moorings = NULL,
                         .detection_overlaps = NULL,
                         .detection_kernels = NULL,
                         .update_ac = NULL,
                         .rpropose = pf_rpropose_kick,
                         .dpropose = pf_dpropose,
                         .n = 100L,
                         .sample_origin = pf_sample_multinomial,
                         .sample = .sample_origin,
                         .trial_origin_crit = 1L,
                         .trial_origin = 1L,
                         .trial_kick_crit = 1L,
                         .trial_kick = 2L,
                         .trial_sampler_crit = 2L,
                         .trial_sampler = 2L,
                         .trial_revert_crit = 2L,
                         .trial_revert_steps = 10L,
                         .trial_revert = 2L,
                         .rerun = list(), .rerun_from = pf_setup_rerun(.rerun),
                         .save_opts = FALSE, .write_opts = NULL,
                         .progress = TRUE, .verbose = TRUE, .txt = "") {

  #### Check user inputs
  t_onset <- Sys.time()
  # .pf_checks()

  #### Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(call_start(.fun = as.character(sys.call(0L)), .start = t_onset))
  on.exit(cat_to_cf(call_end(.fun = as.character(sys.call(0L)),
                             .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Define startup objects (e.g., empty output lists)
  cat_to_cf("... Setting up simulation...")
  startup <- .pf_startup(.rerun = .rerun,
                         .obs = .obs,
                         .lonlat = .lonlat,
                         .bathy = .bathy,
                         .moorings = .moorings,
                         .detection_overlaps = .detection_overlaps,
                         .detection_kernels = .detection_kernels,
                         .update_ac = .update_ac,
                         .write_opts = .write_opts)
  # Output objects
  history        <- startup$output$history
  diagnostics    <- startup$output$diagnostics
  # Controls
  iter_i         <- startup$control$iter_i
  iter_m         <- startup$control$iter_m
  # (Processed) data
  .moorings      <- startup$data$.moorings
  # Wrappers
  .pf_lik_abbr   <- startup$wrapper$.pf_lik_abbr
  .pf_write_particles_abbr <- startup$wrapper$.pf_write_particles_abbr
  .pf_write_diagnostics_abbr <- startup$wrapper$.pf_write_diagnostics_abbr

  #### Define origin
  pnow <- NULL
  if (length(history) == 0L | .rerun_from == 1L) {
    cat_to_cf("... Defining origin...")
    pnow <- .pf_particles_origin(.obs = .obs,
                                 .origin = .origin,
                                 .grid = FALSE,
                                 .detection_kernels = .detection_kernels,
                                 .moorings = .moorings,
                                 .bathy = .bathy,
                                 .pf_lik = .pf_lik_abbr,
                                 .sample = .sample, .n = .n,
                                 .trial_crit = .trial_origin_crit,
                                 .trial_count = .trial_origin)
    diagnostics_1 <- .pf_diag_collect(.diagnostics = attr(pnow, "diagnostics"),
                                      .iter_m = iter_m, .iter_i = iter_i)
    # Record accepted cells
    if (.save_opts) {
      # Drop attributes via data.table()
      history[[1L]]     <- copy(data.table(pnow))
      diagnostics[[1L]] <- diagnostics_1
    }
    .pf_write_particles_abbr(data.table(pnow))
    .pf_write_diagnostics_abbr(diagnostics_1)
  }

  #### Initiate loop
  # Define starting time step
  t          <- .pf_start_t(.rerun, .rerun_from)
  index_diag <- length(diagnostics) + 1L
  # Define previous particles
  ppast <- .pf_ppast(.particles = pnow, .history = history,
                     .sink = startup$output$folder_history, .t = t)
  # Define progress bar
  pb <- pb_init(.n = max(.obs$timestep), .init = 2L, .progress = .progress)

  #### Run simulation
  cat_to_cf("... Initiating simulation...")
  while (t %in% 2L:max(.obs$timestep)) {

    #### Initiate time step
    cat_to_cf(paste0("... ... Time step ", t, ":"))
    pb_tick(.pb = pb, .t = t, .progress = .progress)
    diagnostics_t <- list()

    #### (1) Propose new particles (using kicks)
    if (.trial_kick > 0L) {
      cat_to_cf("... ... ... Kicking particles...")
      pnow <- .pf_particles_kick(.particles = copy(ppast),
                                 .rpropose = .rpropose, .obs = .obs, .t = t, .bathy = .bathy,
                                 .pf_lik = .pf_lik_abbr,
                                 .sample = .sample, .n = .n,
                                 .trial_crit = .trial_kick_crit,
                                 .trial_count = .trial_kick)
      diagnostics_t[["kick"]] <- .pf_diag_bind(attr(pnow, "diagnostics"))
    }

    #### (2) Propose new particles (using direct sampling)
    if (.trial_sampler > 0L) {
      use_sampler <- .pf_trial_sampler(.diagnostics = diagnostics_t,
                                       .trial_crit = .trial_sampler_crit)
      if (use_sampler) {
        cat_to_cf("... ... ... Using directed sampling...")
        pnow <- .pf_particles_sampler(.particles = copy(ppast),
                                      .obs = .obs, .t = t, .bathy = .bathy, .lonlat = .lonlat,
                                      .pf_lik = .pf_lik_abbr,
                                      .dpropose = .dpropose,
                                      .sample, .n,
                                      .trial_crit = .trial_sampler_crit,
                                      .trial_count = .trial_sampler)
        diagnostics_t[["sampler"]] <- .pf_diag_bind(attr(pnow, "diagnostics"))
      }
    }

    #### (3) Collect diagnostics
    diagnostics_t <- .pf_diag_collect(diagnostics_t, .iter_m = iter_m, .iter_i = iter_i)
    if (.save_opts) {
      diagnostics[[index_diag]] <- diagnostics_t
    }
    .pf_write_diagnostics_abbr(diagnostics_t)
    index_diag <- index_diag + 1L

    #### (4) (A) Revert to an earlier time step
    crit <- diagnostics_t$n_u[nrow(diagnostics_t)]
    if (crit < .trial_revert_crit & iter_i <= .trial_revert) {
      # Define time step
      t <- .pf_revert(.t = t, .trial_revert_steps = .trial_revert_steps)
      cat_to_cf(paste0("... ... ... Reverting to time ", t, " (on ", iter_i, "/", .trial_revert, " revert attempt(s)...)"))
      iter_i <- iter_i + 1L
      # Define ppast for relevant time step
      ppast <- .pf_ppast(.particles = NULL, .history = history,
                         .sink = startup$output$folder_history, .t = t)

    } else {

      #### (3) (B) Continue or finish simulation
      # Check convergence
      continue <- .pf_continue(.particles = pnow, .t = t,
                               .crit = crit, .trial_revert_crit = .trial_revert_crit)
      # Save particle samples (if possible)
      if (continue) {
        if (.save_opts) {
          history[[t]] <- copy(data.table(pnow))
        }
        .pf_write_particles_abbr(.particles = data.table(pnow))
      } else {
        # For convergence failures, collate outputs & return up to current time step
        out <- .pf_outputs(.rerun = .rerun,
                           .start = t_onset,
                           .startup = startup,
                           .history = history,
                           .diagnostics = diagnostics,
                           .convergence = FALSE)
        return(out)
      }

      #### Move on
      t <- t + 1L
      ppast <- .pf_increment(.particles = pnow)
    }

  }
  pb_close(.pb = pb, .progress = .progress)

  #### Return outputs
  .pf_outputs(.rerun = .rerun,
              .start = t_onset,
              .startup = startup,
              .history = history,
              .diagnostics = diagnostics,
              .convergence = TRUE)

}

#' @title PF: forward run diagnostics
#' @description This function collates diagnostics from the forward run.
#' @param .sink A character string that defines the directory in which [`pf_forward()`] outputs are saved. `.sink` should contain the `diagnostics/` directory that is created by [`pf_forward()`].
#' @details This function is not memory safe.
#' @return The function returns the diagnostics [`data.table`].
#' @author Edward Lavender
#' @export

pf_forward_diagnostics <- function(.sink) {
  # TO DO
  # * Consider use of .pf_history_dt() here
  # * But note that file ordering with arrow::open_dataset() may be problematic
  check_dir(.sink)
  if (basename(.sink) != "diagnostics") {
    .sink <- file.path(.sink, "diagnostics")
    check_dir(.sink)
  }
  .sink |>
    pf_setup_files() |>
    pbapply::pblapply(arrow::read_parquet) |>
    rbindlist()
}
