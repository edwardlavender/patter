#' @title PF: forward simulation
#' @description This function runs the forward simulation, generating samples of the set of possible locations of an animal at each time point given the data up to that time point and a movement model.
#'
#' @param .obs A [`data.table`] defining the timeline and associated observations, typically from [`acs_setup_obs()`].
#' @param .dlist A `named` list of data and parameters required propose samples and calculate likelihoods (see [`pf_setup_data()`], [`pf_lik`] and [`pf_propose`]. At a minimum, this function requires `.dlist$spatial$bathy` and additional elements required by `.likelihood`,`.rpropose` and `.dpropose` functions (see XXX).
#' @param .origin A [`SpatRaster`] that defines starting location(s). `NA`s/non `NA`s distinguish possible/impossible locations. By default, `.origin` is defined from the bathymetry grid (`.dlist$spatial$bathy`). For AC*PF algorithm implementations, grid cells beyond acoustic containers are masked. For *DCPF implementations, it is also desirable if you can, at least approximately, mask grid cells that are incompatible with the first depth observation. At the first time step, up to `1e6` locations ('quadrature points') are sampled from `.origin`. The likelihood of the data at each quadrature point is evaluated and `.n` starting locations are sampled with replacement (via `.sample`).
#' @param .rpropose,.dpropose Proposal functions.
#' * `.rpropose` is a function that proposes new locations for the individual, given previous locations. By default, this is a 'stochastic kick' function that simulates new locations by randomly kicking previous particles (see [`pf_rpropose_kick()`] and Details).
#' * `.dpropose` is a function that evaluates the probability density of movements between location pairs (see [`pf_dpropose`]). This is required for directed sampling (see Details).
#'
#' See [`pf_propose`] for required arguments.
#'
#' @param .likelihood A named `list` of likelihood functions. These are used to calculate the likelihood of each dataset at proposal locations. See [`pf_lik`] for required arguments, convenience functions and advice.
#' @param .n,.sample Sampling arguments.
#' * `.n` is an `integer` that defines the number of particle samples at each time step.
#' * `.sample` is a function used to (re)-sample proposal locations (see [`pf_sample`]).
#'
#' @param .trial_origin_crit,.trial_origin,.trial_kick_crit,.trial_kick,.trial_sampler_crit,.trial_sampler,.trial_revert_crit,.trial_revert_steps,.trial_revert Trial arguments used to tune convergence properties. ALl arguments expect `integer`s.
#' * `.trial_{step}` arguments define the number of times to trial a stochastic process at each time step (before giving up).
#' * `.trial_{step}_crit` arguments define the number of valid, unique proposal locations (grid cells) required to trigger a repeated trial.
#'
#' For example, if after kicking previous particles into proposal locations and evaluating the likelihood of each location only 10 unique particles remain, the stochastic process is repeated up to `.trial_kick` times or until the number of valid proposals exceeds `.trial_kick_crit` (see Details). In full:
#'
#' * `.trial_origin_crit` is the critical threshold for the starting samples. If the number of unique, valid starting locations is <= `.trial_origin_crit`, sampling is repeated up to `.trial_origin` times.
#' * `trial_kick_crit` is the critical threshold for stochastic kicks. If the number of unique, valid proposals is <= `trial_kick_crit`, the process of kicking and sampling particles (via `.rpropose` and `.sample`) is repeated up to `.trial_kick` times. Use `.trial_kick = 0L` to suppress stochastic kicks.
#' * `.trial_sampler_crit` is the critical threshold for directed sampling. Following stochastic kicks, if the number of unique, valid proposals remains <= `.trial_sampler_crit`, directed sampling is implemented. Samples are redrawn up to `.trial_sampler` times. Use `.trial_sampler_crit = 0L` to suppress directed sampling.
#' * `.trial_revert_crit` is the critical threshold for a reversion. If the number of unique, valid proposal locations is <= `.trial_revert_crit`, the algorithm reverts by `.trial_revert_steps` time steps to an earlier time step (time step two or greater). `.trial_revert` is the total number of reversions permitted. This is reset on algorithm reruns (see `.rerun`).
#'
#' @param .control A named `list` of control options. See [`pf_control()`] for supported options.
#' @param .rerun,.rerun_from Rerun options. These options are used to restart the algorithm from an earlier time step in the case of a convergence failure.
#' * `.rerun` is the named `list` of algorithm outputs from a previous rerun.
#' * `.rerun_from` is an `integer` that defines the time step from which to rerun the algorithm.
#'
#' Algorithm parameters should remain consistent on algorithm reruns.
#'
#' @param .record A named `list` that controls function outputs. This may include the following elements:
#' * `save`---a `logical` variable that defines whether or not to save particle samples and diagnostics in memory. Use with caution.
#' * `sink`---a `character` string that defines a (usually) empty directory in which to write particle samples and diagnostics (see Value).
#' * `cols`---a `character` vector that defines the names of the columns in particle-sample [`data.table`]s to save and/or write to file at each time step. This reduces the space occupied by outputs. At a minimum, you should retain `timestep`, `cell_now`, `x_now` and `y_now` for the backward sampler (see [`pf_backward_sampler()`]). `NULL` retains all columns.
#'
#' At least one of `.save` and `.sink` must be provided.
#'
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#' @param ... Additional arguments.
#'
#' @details
#'
#' # Overview
#'
#' [`pf_forward()`] iterates over time steps, simulating location samples (termed 'particles') that are consistent with the preceding data and a movement model at each time step. At each time step, this process comprises four steps:
#' 1. A proposal step, in which we propose possible locations for the individual.
#' 2. A likelihood step, in which we calculate the likelihood of the data given each proposal.
#' 3. A weights step, in which we translate likelihoods into sampling weights.
#' 4. A sampling step, in which we (re)sample valid proposal locations using the weights.
#'
#' At the first time step, proposal locations are defined from a large number of 'quadrature points' across `.origin`. At each quadrature point, we evaluate likelihoods and calculate weights. `.n` starting locations (particles) are sampled from the set of quadrature points using the weights.
#'
#' At subsequent time steps, proposal locations are generated via `.rpropose` which, by default, is a 'stochastic kick' function that 'kicks' previous particles at random into new locations (in line the restrictions imposed by a movement model). The benefit of this approach is that it is extremely fast, but in situations in which there are relatively few possible locations for an individual, the approach can work poorly because few kicked particles end up in valid locations. A `list` of likelihood functions is used to evaluate the likelihood of the data, given each proposal, and filter invalid proposals. During this time, we track how the number and diversity of proposal locations declines, as the data are revealed to be incompatible with selected proposals by successive likelihood functions (see Diagnostics). Likelihoods are translated into weights and `.n` valid proposals (particles) are resampled, with replacement, using the weights. If the number of unique, valid locations is <= `.trial_kick_crit`, this process is repeated up to `.trial_kick` times.
#'
#' Following the stochastic-kick methodology, if the number of unique, valid locations is <= `.trial_sampler_crit`, directed sampling is initiated. For each unique, previous location, this methodology identifies the set of reachable cells, given a mobility parameter, and evaluates likelihoods and the probability density of movements into each reachable location (which are used to define sampling weights). `.n` locations are then directly sampled from the set of valid locations. This approach is expensive in terms of time (since it requires iteration over particles) and memory (since the complete set of valid locations is used for sampling). Particles can be processed in batches for improved speed, up to the limits imposed by available memory (see `.control`). While this approach is expensive, sampling from the set of valid locations facilitates convergence.
#'
#' You can opt to use either `.rpropose` or directed sampling via the `.trial_` arguments. However, in general, it is advisable to permit the algorithm to chop-and-change between methods, depending on the number of valid proposals. This approach benefits from the speed benefits of stochastic kicks, where possible, as well as the improved convergence properties of directed sampling, where required.
#'
#' Particle rejuvenation is another strategy that is sometimes used to facilitate convergence but this is not currently implemented.
#'
#' At the end of each time step, if the number of unique, valid locations remains <= `.trial_revert_crit`, the algorithm can step backwards in time by `.trial_revert_steps` and try again. This reversion will be attempted up to `.trial_revert` times. After `.trial_revert` times, if the algorithm reaches a time step when there are fewer than `.trial_revert_crit` unique samples, it will produce a [`warning`], but attempt to continue the simulation if possible. In the case of convergence failures, you can rerun the simulation from existing outputs, starting from an earlier time step, via `.rerun`. However, algorithm arguments should remain constant. On algorithm reversions and reruns, particle samplers are replaced but particle diagnostics are always retained.
#'
#' The algorithm iterates over the time series, proposing and sampling particles as described above. Particle samples can be saved in memory or written to file, alongside particle diagnostics. If the function fails to convergence, a [`warning`] is returned alongside the outputs up to that time step. Otherwise, the function will continue to the end of the time series.
#'
#' # Algorithms
#'
#' This is highly flexible routine for the reconstruction of possible locations of an individual through time, given the data up to that time point. By modifying the likelihood functions, it is straightforward to implement the ACPF, DCPF and ACDCPF algorithms introduced by Lavender et al. (2023) for reconstructing movements using (a) acoustic time series, (b) archival time series and (c) acoustic and archival time series.
#'
#' # Convergence and diagnostics
#'
#' While [`pf_forward()`] tries hard to reconstruct a complete time series of location samples, algorithm convergence is not guaranteed. The algorithm may reach a dead-end---a time step at which there are no valid locations into which the algorithm can step. This may be due to data errors, incorrect assumptions, insufficient sampling effort or poor tuning parameter settings. To facilitate diagnosis of the immediate cause of convergence failures, during likelihood evaluations we keep track of 'particle diagnostics', i.e., the number of unique, valid locations before/after each likelihood evaluation alongside other statistics.
#'
#'
#' @return The function returns a [`pff-class`] object. If `.return$sink` is specified, two directories, {.return$sink}/history/ and {.return$sink}/diagnostics, are also created that contain particle samples and diagnostics. Particle samples are labelled `1.parquet, 2.parquet, ..., T.parquet`, where `T` is the number of time steps. Diagnostics are labelled `1.parquet, 2.parquet, ..., Z.parquet`, where Z is the number of diagnostic files. There may be multiple diagnostic files (e.g.., `1.parquet, 2.parquet, 3.parquet` for each time step. Use [`pf_forward_diagnostics()`] to collate diagnostics.
#'
#' @seealso
#'
#' @author Edward Lavender
#' @export

pf_forward <- function(.obs,
                       .dlist,
                       .origin = .dlist$spatial$bathy,
                       .rpropose = pf_rpropose_kick, .dpropose = pf_dpropose,
                       .likelihood = list(),
                       .n = 100L,
                       .sample = pf_sample_multinomial,
                       .trial_origin_crit = 1L,
                       .trial_origin = 1L,
                       .trial_kick_crit = 1L,
                       .trial_kick = 2L,
                       .trial_sampler_crit = 2L,
                       .trial_sampler = 2L,
                       .trial_revert_crit = 2L,
                       .trial_revert_steps = 10L,
                       .trial_revert = 2L,
                       .control = pf_control(),
                       .rerun = list(), .rerun_from = pf_rerun_from(.rerun),
                       .record = pf_record(),
                       .verbose = TRUE) {

  #### Check user inputs
  t_onset <- Sys.time()
  # .pf_checks()

  #### Set up messages
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_forward", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_forward", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Define startup objects (e.g., empty output lists)
  cat_log("... Setting up simulation...")
  startup <- .pf_startup(.obs = .obs,
                         .dlist = dlist,
                         .rerun = .rerun,
                         .record = .record)
  # Output objects
  .record        <- startup$output$.record
  select_cols    <- startup$output$select_cols
  history        <- startup$output$history
  diagnostics    <- startup$output$diagnostics
  # Controls
  iter_i         <- startup$control$iter_i
  iter_m         <- startup$control$iter_m
  # Wrappers
  .pf_write_particles_abbr   <- startup$wrapper$.pf_write_particles_abbr
  .pf_write_diagnostics_abbr <- startup$wrapper$.pf_write_diagnostics_abbr

  #### Define origin
  pnow <- NULL
  if (length(history) == 0L | .rerun_from == 1L) {
    cat_log("... Defining origin...")
    pnow <- .pf_particles_origin(.particles = NULL,
                                 .obs = .obs, .t = 1L, .dlist = .dlist,
                                 .origin = .origin,
                                 .rpropose = NULL, .dpropose = NULL,
                                 .likelihood = .likelihood,
                                 .sample = .sample, .n = .n,
                                 .trial_crit = .trial_origin_crit,
                                 .trial_count = .trial_origin,
                                 .control = .control)
    diagnostics_1 <- .pf_diag_collect(.diagnostics = attr(pnow, "diagnostics"),
                                      .iter_m = iter_m, .iter_i = iter_i)
    # Record accepted cells
    snapshot <- .pf_snapshot(.dt = pnow, .select = select_cols, .cols = .record$cols)
    if (.record$save) {
      history[[1L]]     <- snapshot
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
  pb <- pb_init(.min = 2L, .max = max(.obs$timestep))

  #### Run simulation
  cat_log("... Initiating simulation...")
  while (t %in% 2L:max(.obs$timestep)) {

    #### Initiate time step
    cat_log(paste0("... ... Time step ", t, ":"))
    pb_tick(.pb = pb, .t = t)
    diagnostics_t <- list()

    #### (1) Propose new particles (using kicks)
    if (.trial_kick > 0L) {
      cat_log("... ... ... Kicking particles...")
      pnow <- .pf_particles_kick(.particles = copy(ppast),
                                 .obs = .obs, .t = t, .dlist = .dlist,
                                 .rpropose = .rpropose, .dpropose = NULL,
                                 .likelihood = .likelihood,
                                 .sample = .sample, .n = .n,
                                 .trial_crit = .trial_kick_crit,
                                 .trial_count = .trial_kick,
                                 .control = .control
                                 )
      diagnostics_t[["kick"]] <- .pf_diag_bind(attr(pnow, "diagnostics"))
    }

    #### (2) Propose new particles (using direct sampling)
    if (.trial_sampler > 0L) {
      use_sampler <- .pf_trial_sampler(.diagnostics = diagnostics_t,
                                       .trial_crit = .trial_sampler_crit)
      if (use_sampler) {
        cat_log("... ... ... Using directed sampling...")
        pnow <- .pf_particles_sampler(.particles = copy(ppast),
                                      .obs = .obs, .t = t, .dlist = .dlist,
                                      .rpropose = NULL, .dpropose = .dpropose,
                                      .likelihood = .likelihood,
                                      .sample = .sample, .n = .n,
                                      .trial_crit = .trial_sampler_crit,
                                      .trial_count = .trial_sampler,
                                      .control = .control)
        diagnostics_t[["sampler"]] <- .pf_diag_bind(attr(pnow, "diagnostics"))
      }
    }

    #### (3) Collect diagnostics
    diagnostics_t <- .pf_diag_collect(diagnostics_t, .iter_m = iter_m, .iter_i = iter_i)
    if (.record$save) {
      diagnostics[[index_diag]] <- diagnostics_t
    }
    .pf_write_diagnostics_abbr(diagnostics_t)
    index_diag <- index_diag + 1L

    #### (4) (A) Revert to an earlier time step
    crit <- diagnostics_t$n_u[nrow(diagnostics_t)]
    if (crit < .trial_revert_crit & iter_i <= .trial_revert) {
      # Define time step
      t <- .pf_revert(.t = t, .trial_revert_steps = .trial_revert_steps)
      cat_log(paste0("... ... ... Reverting to time ", t, " (on ", iter_i, "/", .trial_revert, " revert attempt(s)...)"))
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
        snapshot <- .pf_snapshot(.dt = pnow, .select = select_cols, .cols = .record$cols)
        if (.record$save) {
          history[[t]] <- snapshot
        }
        .pf_write_particles_abbr(.particles = snapshot)
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
  pb_close(.pb = pb)

  #### Return outputs
  .pf_outputs(.rerun = .rerun,
              .start = t_onset,
              .startup = startup,
              .history = history,
              .diagnostics = diagnostics,
              .convergence = TRUE)

}

#' @rdname pf_forward
#' @export

pf_record <- function(..., .record = list()) {
  args <- list(...)
  args <- list_merge(args, .record)
  defaults <- list(save = FALSE,
                   cols = NULL,
                   sink = NULL)
  out <- list_args(.args = args, .defaults = defaults)
  if (!out$save && is.null(out$sink)) {
    abort("`.record$save = FALSE` and `.record$sink = NULL`. There is nothing to do.")
  }
  out
}

#' @rdname pf_forward
#' @export

pf_control <- function(..., .control = list()) {
  args <- list(...)
  args <- list_merge(args, .control)
  defaults <- list(sampler_batch_size = 2L)
  list_args(.args = args, .defaults = defaults)
}

#' @rdname pf_forward
#' @export

pf_rerun_from <- function(.rerun, .trial_revert_steps = 25L) {
  # default `.trial_revert_steps` is bigger than pf_forward `.trial_revert_steps`
  max(c(1L, length(.rerun[["history"]]) - .trial_revert_steps))
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
