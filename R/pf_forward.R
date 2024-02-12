#' @title PF: simulation options
#' @description These functions define selected function arguments for [`pf_forward()`] and/or [`pf_backward_*()`].
#'
#' @param .trial_origin_crit,.trial_kick,.trial_sampler,.trial_sampler_crit,.trial_resample_crit,.trial_revert_crit,.trial_revert_steps,.trial_revert [`pf_opt_trial()`] arguments, passed to `.trial` in [`pf_forward()`] and used to tune convergence properties. All arguments expect `integer` inputs.
#' * `.trial_origin_crit` is an `integer` that specifies the critical effective sample size (ESS) for the starting samples. If the initial ESS is < `.trial_origin_crit`, a [`warning`] is given.
#' * `.trial_kick` is a `integer` that defines the number of stochastic kicks at each time step.
#' * `.trial_sampler` is an `integer` (0, 1) that defines whether or not to trial the directed sampling methodology.
#' * `.trial_sampler_crit` is an `integer` that specifies the critical threshold for directed sampling. Following stochastic kicks, if the ESS is < `.trial_sampler_crit`, directed sampling is implemented.
#' * `.trial_resample_crit` is an `integer` that defines the ESS for (re)sampling. Particles are resampled when the ESS is < `.trial_resample_crit`.
#' * `.trial_revert_crit` is an `integer` that specifies the critical threshold for a reversion. If the ESS < `.trial_revert_crit`, the algorithm reverts by `.trial_revert_steps` time steps to an earlier time step (time step two or greater).
#' * `.trial_revert` is an `integer` that specifies the total number of reversions permitted. This is reset on algorithm reruns (see `.rerun`).
#'
#' @param .save,.sink,.cols [`pf_opt_record()`] arguments, passed to `.record` in [`pf_forward()`] and [`pf_backward_*()`].
#' * `.save`---a `logical` variable that defines whether or not to save particle samples and diagnostics in memory. Use `.save = TRUE` with caution.
#' * `.sink`---a `character` string that defines a (usually) empty directory in which to write particle samples and diagnostics. `{.sink}/history/`and `{.sink}/diagnostics` directories are created (if necessary) to store particle samples and diagnostics respectively.
#' * `.cols`---a `character` vector that defines the names of the columns in particle-sample [`data.table`]s to save and/or write to file at each time step. This reduces the space occupied by outputs. For [`pf_backward_killer()`], you need to retain `timestep`, `cell_past` and `cell_now`. For [`pf_backward_sampler`]`_*()`, you need `timestep`, `cell_now`, `x_now` and `y_now` for the backward sampler. For calculation of effective sample size, `weight` is required. `NULL` retains all columns.
#'
#' At least one of `.save = TRUE` and `.sink` must be set.
#'
#' @param .drop,.sampler_batch_size [`pf_opt_control()`] arguments, passed to `.control` in [`pf_forward()`] and [`pf_backward_*()`].
#' * `.drop` A `logical` variable that defines whether or not to drop particles with zero likelihood or density.
#' * `.sampler_batch_size`---for [`pf_forward()`] (specifically, [`.pf_particles_sampler()`]), `.sampler_batch_size` is an `integer` that controls the batch size (the number of particles processed simultaneously) in directed sampling. Increase the batch size to improve speed; decrease the batch size to avoid memory constraints. The appropriate batch size depends on grid resolution and memory availability.
#'
#' @param .rerun,.revert [`pf_opt_rerun_from()`] arguments.
#' * `.rerun` is a named `list` of algorithm outputs from a previous run.
#' * `.revert` is an `integer` that defines the number of steps to revert.
#'
#' @details These functions are defined separately for convenience of documentation. Note that they do not define global options and must be passed to [`pf_forward()`] arguments.
#'
#' @return
#' * [`pf_opt_trial()`] returns a named `list`;
#' * [`pf_opt_record()`] returns a named `list`;
#' * [`pf_opt_control()`] returns a named `list`;
#' * [`pf_opt_rerun_from()`] returns an `integer`;
#'
#' @examples
#' pf_opt_trial()
#' pf_opt_trial(.trial_resample_crit = 200L)
#' pf_opt_record(.save = TRUE)
#' pf_opt_control()
#' pf_opt_rerun_from(dat_pff(), .revert = 10L)
#'
#' @inherit pf_forward seealso
#' @author Edward lavender
#' @name pf_opt

#' @rdname pf_opt
#' @export

pf_opt_trial <- function(.trial_origin_crit = 1L,
                         .trial_kick = 1L,
                         .trial_sampler = 1L,
                         .trial_sampler_crit = 10L,
                         .trial_resample_crit = 500L,
                         .trial_revert_crit = 1L,
                         .trial_revert_steps = 10L,
                         .trial_revert = 0L) {
  list(trial_origin_crit = .trial_origin_crit,
       trial_kick = .trial_kick,
       trial_sampler = .trial_sampler,
       trial_sampler_crit = .trial_sampler_crit,
       trial_resample_crit = .trial_resample_crit,
       trial_revert_crit = .trial_revert_crit,
       trial_revert_steps = .trial_revert_steps,
       trial_revert = .trial_revert)
}

#' @rdname pf_opt
#' @export

pf_opt_record <- function(.save = FALSE, .cols = NULL, .sink = NULL) {
  # Checks
  if (!.save && is.null(.sink)) {
    abort("`.save = FALSE` and `.sink = NULL`. There is nothing to do.")
  }
  # Output list
  list(save = .save,
       cols = .cols,
       sink = .sink)
}

#' @rdname pf_opt
#' @export

pf_opt_control <- function(.sampler_batch_size = 2L, .drop = TRUE) {
  list(drop = .drop,
       sampler_batch_size = .sampler_batch_size)
}

#' @rdname pf_opt
#' @export

pf_opt_rerun_from <- function(.rerun, .revert = 25L) {
  # default `.revert` is bigger than `.trial_revert_steps`
  max(c(1L, length(.rerun[["history"]]) - .revert))
}

#' @title PF: forward simulation
#' @description This function runs the forward simulation of the forward filtering--backward smoothing algorithm. The forward simulation samples possible locations (particles) of an animal at each time point given the data up to (and including) that time point and a movement model.
#'
#' @param .obs A [`data.table`] defining the timeline and associated observations, typically from [`pf_setup_obs()`].
#' @param .dlist A `named` list of data and parameters required propose locations and calculate likelihoods (see [`pat_setup_data()`], [`pf_lik`] and [`pf_propose`]). This function requires:
#' * `.dlist$spatial$bathy`
#' * (optional) `.dlist$spatial$origin`, a [`SpatRaster`] used to define the origin.
#' * `.dlist$pars$lonlat` (required by the default `.rpropose` and `.dpropose` arguments).
#' * Any additional elements required by `.likelihood`,`.rpropose` and `.dpropose` functions(see below).
#'
#' @param .rpropose,.dpropose,.rargs,.dargs Proposal functions and associated argument lists (see [`pf_propose`]).
#' * `.rpropose` is a function that proposes new locations for the individual, given previous locations. By default, this is a 'stochastic kick' function that simulates new locations by randomly kicking previous particles (see [`pf_rpropose_kick()`] and Details).
#' * `.dpropose` is a function that evaluates the probability density of movements between location pairs (see [`pf_dpropose()`]). This is required for directed sampling (see Details).
#' * `.rargs` and `.dargs` are named `list`s of arguments passed to `.rpropose` and `.dpropose` respectively.
#'
#' @param .likelihood A named `list` of likelihood functions. These are used to calculate the likelihood of each dataset at proposal locations. See [`pf_lik`] for required arguments, convenience functions and advice.
#' @param .n,.sample Sampling arguments.
#' * `.n` is an `integer` that defines the number of particle samples at each time step.
#' * `.sample` is a function used to (re)-sample proposal locations (see [`pf_sample`]).
#'
#' @param .trial A named `list` of tuning parameters for convergence, from [`pf_opt_trial()`].
#'
#' @param .control A named `list` of control options, from [`pf_opt_control()`].
#' @param .rerun,.rerun_from Rerun options. These options are used to restart the algorithm from an earlier time step in the case of a convergence failure.
#' * `.rerun` is the named `list` of algorithm outputs from a previous rerun.
#' * `.rerun_from` is an `integer` that defines the time step from which to rerun the algorithm.
#'
#' Algorithm parameters should remain consistent on algorithm reruns.
#'
#' @param .record A named `list` of output options, from [`pf_opt_record()`].
#'
#' @param .verbose User output control (see [`patter-progress`] for supported options).
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
#' At the first time step, proposal locations are defined from a large number of 'quadrature points' across a [`SpatRaster`] that defines starting location(s), defined in `.dlist$spatial$origin` (`.dlist$spatial$bathy` is used if `.dlist$spatial$origin` is undefined). `NA`s/non `NA`s distinguish possible/impossible locations. For AC*PF algorithm implementations (i.e., implementations that incorporate acoustic data, see Algorithms, below), grid cells beyond acoustic containers are masked. For *DCPF implementations (i.e., implementations that incorporate depth data, see Algorithms, below), it is also desirable if you can, at least approximately, mask grid cells that are incompatible with the first depth observation. At the first time step, up to `1e6` locations ('quadrature points') are sampled from the [`SpatRaster`]. At each quadrature point, we evaluate likelihoods and calculate weights. `.n` starting locations (particles) are sampled, via `.sample`, from the set of quadrature points using the weights. If the effective sample size (ESS) of sampled locations is less than `.trial$trial_origin_crit`, a [`warning`] is given.
#'
#' At subsequent time steps, proposal locations are generated via `.rpropose` which, by default, is a 'stochastic kick' function that 'kicks' previous particles at random into new locations (in line with the restrictions imposed by a movement model). The benefit of this approach is that it is extremely fast, but in situations in which there are relatively few possible locations for an individual, the approach can work poorly because few kicked particles end up in valid locations. A `list` of likelihood functions is used to evaluate the likelihood of the data, given each proposal, and filter invalid proposals. During this time, we track how the number and diversity of proposal locations declines, as the data are revealed to be incompatible with selected proposals by successive likelihood functions (see Convergence and diagnostics, below). Likelihoods are translated into weights for resampling (see below).
#'
#' Following the stochastic-kick methodology, if the effective sample size is <= `.trial$trial_sampler_crit`, directed sampling is initiated. For each unique, previous location, this methodology identifies the set of reachable cells, given a mobility parameter, and evaluates likelihoods and the probability density of movements into each reachable location (which are used to define sampling weights). This approach is expensive in terms of time (since it requires iteration over particles) and memory (since the complete set of valid locations is used for sampling). Particles can be processed in batches for improved speed, up to the limits imposed by available memory (see `.control`). While this approach is expensive, sampling from the set of valid locations facilitates convergence.
#'
#' You can opt to use either `.rpropose` or directed sampling via the `.trial_` arguments to [`pf_opt_trial()`]. However, in general, it is advisable to permit the algorithm to chop-and-change between methods, depending on the number of valid proposals. This approach benefits from the speed of stochastic kicks, where possible, as well as the improved convergence properties of directed sampling, where required.
#'
#' Particle rejuvenation is another strategy that is sometimes used to facilitate convergence but this is not currently implemented.
#'
#' At the end of each time step, the effective sample size of weighted particles is evaluated. If the ESS is below the critical value specified in `.trial$trial_resample_crit` and/or directed sampling is implemented, particles are (re)-sampled, via `.sample`. If the ESS is less than `.trial$trial_revert_crit`, the algorithm can step backwards in time by `.trial$trial_revert_steps` and try again. This reversion will be attempted up to `.trial$trial_revert` times. After `.trial$trial_revert` times, if the algorithm reaches a time step when the ESS is less than `.trial$trial_revert_crit`, it will produce a [`warning`], but attempt to continue the simulation if possible. In the case of convergence failures, you can rerun the simulation from existing outputs, starting from an earlier time step, via `.rerun`. However, algorithm arguments should remain constant. On algorithm reversions and reruns, particle samplers are replaced but particle diagnostics are always retained.
#'
#' The algorithm iterates over the time series, proposing, weighting and (re)sampling particles as described above. Particle samples can be saved in memory or written to file, alongside particle diagnostics. If the function fails to convergence, a [`warning`] is returned alongside the outputs up to that time step. Otherwise, the function will continue to the end of the time series.
#'
#' # Algorithms
#'
#' This is highly flexible routine for the reconstruction of the possible locations of an individual through time, given the data up to that time point. By modifying the likelihood functions, it is straightforward to implement the ACPF, DCPF and ACDCPF algorithms introduced by Lavender et al. (2023) for reconstructing movements using (a) acoustic time series, (b) archival time series and (c) acoustic and archival time series. [`pf_forward()`] thus replaces (and enhances) the [`flapper::ac()`](https://edwardlavender.github.io/flapper/reference/ac.html), [`flapper::dc()`](https://edwardlavender.github.io/flapper/reference/dc.html), [`flapper::acdc()`](https://edwardlavender.github.io/flapper/reference/acdc.html) and [`flapper::pf()`](https://edwardlavender.github.io/flapper/reference/pf.html) functions.
#'
#' # Convergence and diagnostics
#'
#' While [`pf_forward()`] tries hard to reconstruct a complete time series of location samples, algorithm convergence is not guaranteed. The algorithm may reach a dead-end---a time step at which there are no valid locations into which the algorithm can step. This may be due to data errors, incorrect assumptions, insufficient sampling effort or poor tuning-parameter settings. To facilitate diagnosis of the immediate cause of convergence failures, during likelihood evaluations we keep track of 'particle diagnostics', i.e., the number of unique, valid locations before/after each likelihood evaluation alongside other statistics (see [`pf_diag-internal`] and [`pf_particles-class`]).
#'
#' @return The function returns a [`pf_particles-class`] object. If `.record$sink` is specified, two directories, `{.record$sink}/history/` and `{.record$sink}/diagnostics`, are also created that contain particle samples and diagnostics. Particle samples are labelled `1.parquet, 2.parquet, ..., T.parquet`, where `T` is the number of time steps. Diagnostics are labelled `A-B-C`, where `A`, `B` and `C` are the number of manual restarts, internal reversions and time steps. Use [`pf_diag_convergence()`] to collate convergence diagnostics and [`pf_diag_summary()`] for a summary.
#'
#' @example man/examples/pf_forward-examples.R
#'
#' @seealso The forward filtering--backward sampling algorithm samples locations (particles) that represent the possible locations of an individual through time, accounting for all data and the individual's movement.
#'
#' * To set up data, use [`pat_setup_data()`].
#'
#' * [`pf_forward()`] implements the forward filter:
#'    * To set up an observations timeline, use [`pf_setup_obs()`].
#'    * For proposal (movement) models, see [`pf_propose`].
#'    * For likelihood functions, to evaluate the likelihood of the data at proposal locations, see [`pf_lik`].
#'    * For sampling functions, to (re)sample plausible proposal locations, see [`pf_sample`].
#'    * For tuning parameters, see [`pf_opt`].
#'
#' * [`pf_backward_*()`] refines outputs from the forward filter:
#'    * [`pf_backward_killer()`] removes dead-ends;
#'    * [`pf_backward_sampler`]`_*()` implements the backward sampler;
#'
#' * To reconstruct movement paths from particle samples, use [`pf_path()`].
#'
#' * To map emergent patterns of space use, use [`pf_coord()`] plus a `map_*()` function, such as [`map_pou()`], [`map_dens()`] and/or [`map_hr`]`_()`.
#'
#' * For additional utilities, see supporting `pf_*()` functions, such as [`pf_files()`], [`pf_files_size()`], [`pf_plot_history()`], [`pf_diag_convergence()`] and [`pf_diag_summary()`].
#'
#' @author Edward Lavender
#' @export

pf_forward <- function(.obs,
                       .dlist,
                       .rpropose = pf_rpropose_kick, .rargs = list(),
                       .dpropose = pf_dpropose, .dargs = list(),
                       .likelihood = list(),
                       .n = 100L,
                       .sample = pf_sample_systematic,
                       .trial = pf_opt_trial(),
                       .control = pf_opt_control(),
                       .rerun = list(),
                       .rerun_from = pf_opt_rerun_from(.rerun),
                       .record = pf_opt_record(),
                       .verbose = getOption("patter.verbose")) {

  #### Check user inputs
  t_onset <- Sys.time()
  # .pf_checks()

  #### Set up messages
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_forward", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_forward", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Define startup objects (e.g., empty output lists)
  cat_log("... Setting up simulation...")
  startup <- .pf_forward_startup(.rerun = .rerun,
                                 .record = .record)
  # Arguments
  # * Define these options outside of .pf_forward_startup() so that
  # * .rerun works even if the output list is written to file.
  # * (Otherwise, SpatRasters in startup aren't saved/reloaded properly.)
  .rargs$.obs   <- .obs
  .rargs$.dlist <- .dlist
  .dargs$.obs   <- .obs
  .dargs$.dlist <- .dlist
  .dargs$.drop  <- .control$drop
  # Controls
  iter_i         <- startup$control$iter_i
  iter_m         <- startup$control$iter_m
  # Wrappers
  .pf_write_particles_abbr   <- startup$wrapper$.pf_write_particles_abbr
  .pf_write_diagnostics_abbr <- startup$wrapper$.pf_write_diagnostics_abbr
  # Output objects
  .record        <- startup$output$.record
  select_cols    <- startup$output$select_cols
  history        <- startup$output$history
  diagnostics    <- startup$output$diagnostics
  # Global variables
  weight <- lik <- wt <- NULL

  #### Define origin
  pnow <- NULL
  if (.rerun_from == 1L) {
    cat_log("... Defining origin...")
    pnow <- .pf_particles_origin(.particles = NULL,
                                 .obs = .obs, .t = 1L, .dlist = .dlist,
                                 .rpropose = NULL, .dpropose = NULL,
                                 .rargs = NULL, .dargs = NULL,
                                 .likelihood = .likelihood,
                                 .sample = .sample, .n = .n,
                                 .trial_crit = .trial$trial_origin_crit,
                                 .control = .control)
    diagnostics_1 <- .pf_diag_collect(.diagnostics = attr(pnow, "diagnostics"),
                                      .iter_m = iter_m, .iter_i = iter_i)
    # Record accepted cells
    snapshot <- .pf_snapshot(.dt = pnow, .save = .record$save,
                             .select = select_cols, .cols = .record$cols)
    if (.record$save) {
      history[[1L]]     <- snapshot
      diagnostics[[1L]] <- diagnostics_1
    }
    .pf_write_particles_abbr(snapshot)
    .pf_write_diagnostics_abbr(diagnostics_1)
  }

  #### Initiate loop
  # Define starting index
  t1         <- .pf_forward_start_t(.rerun, .rerun_from)
  index_diag <- length(diagnostics) + 1L
  # Define previous particles
  ppast <- .pf_forward_ppast(.particles = pnow, .history = history,
                             .sink = startup$output$folder_history, .t = t1,
                             .obs = .obs)
  # Define progress bar
  pb <- pb_init(.min = t1, .max = fnrow(.obs))

  #### Run simulation
  cat_log("... Initiating simulation...")
  t <- t1
  while (t %in% t1:fnrow(.obs)) {

    #### Initiate time step
    cat_log(paste0("... ... Index ", t, " (timestep ", .obs$timestep[t], "):"))
    pb_tick(.pb = pb, .t = t)
    diagnostics_t <- list()
    # Argument lists
    .rargs$.t <- t
    .dargs$.t <- t

    #### (1) Propose new particles (using kicks)
    if (.trial$trial_kick > 0L) {
      cat_log("... ... ... Kicking particles...")
      pnow <- .pf_particles_kick(.particles = copy(ppast),
                                 .obs = .obs, .t = t, .dlist = .dlist,
                                 .rpropose = .rpropose,
                                 .rargs = .rargs,
                                 .likelihood = .likelihood,
                                 .control = .control, .trial = .trial$trial_kick
                                 )
      diagnostics_t[["kick"]] <- .pf_diag_bind(attr(pnow, "diagnostics"))
    }

    #### (2) Propose new particles (using direct sampling)
    use_sampler <- FALSE
    if (.trial$trial_sampler > 0L) {
      use_sampler <- .pf_forward_trial_sampler(.diagnostics = diagnostics_t,
                                               .trial_crit = .trial$trial_sampler_crit)
      if (use_sampler) {
        cat_log("... ... ... Using directed sampling...")
        pnow <- .pf_particles_sampler(.particles = copy(ppast),
                                      .obs = .obs, .t = t, .dlist = .dlist,
                                      .dpropose = .dpropose,
                                      .dargs = .dargs,
                                      .likelihood = .likelihood,
                                      .control = .control)
        diagnostics_t[["grid"]] <- .pf_diag_bind(attr(pnow, "diagnostics"))
      }
    }

    #### (3) (optional) Resampling
    if (fnrow(pnow) > 0L) {
      # Update weights
      pnow[, weight := normalise(weight * lik)]
      # Optionally implement re-sampling, if:
      # * ESS < .trial$trial_resample_crit
      # * use_sampler = TRUE
      if (isTRUE((.pf_diag_ess(pnow$weight) < .trial$trial_resample_crit)) || use_sampler) {
        cat_log("... ... ... (Re)-sampling...")
        pnow <- .sample(.particles = pnow, .n = .n)
        diagnostics_t[["resample"]] <- .pf_diag(.particles = pnow,
                                                .weight = "weight",
                                                .t = t,
                                                .label = "sample")
      }
    }
    # Drop wt column to avoid user confusion
    pnow[, wt := NULL]

    #### (4) Collect diagnostics
    diagnostics_t <- .pf_diag_collect(diagnostics_t, .iter_m = iter_m, .iter_i = iter_i)
    if (.record$save) {
      diagnostics[[index_diag]] <- diagnostics_t
    }
    .pf_write_diagnostics_abbr(diagnostics_t)
    index_diag <- index_diag + 1L

    #### (5) (A) Revert to an earlier time step
    crit <- diagnostics_t$ess[nrow(diagnostics_t)]
    if (crit < .trial$trial_revert_crit & iter_i <= .trial$trial_revert) {
      # Define time step
      t <- .pf_forward_revert(.t = t, .trial_revert_steps = .trial$trial_revert_steps)
      cat_log(paste0("... ... ... Reverting to time ", t, " (on ", iter_i, "/", .trial$trial_revert, " revert attempt(s)...)"))
      iter_i <- iter_i + 1L
      # Define ppast for relevant time step
      ppast <- .pf_forward_ppast(.particles = NULL, .history = history,
                                 .sink = startup$output$folder_history,
                                 .t = t, .obs = .obs)

    } else {

      #### (5) (B) Continue or finish simulation
      # Check convergence
      continue <- .pf_forward_continue(.particles = pnow,
                                       .t = t,
                                       .crit = crit,
                                       .trial_revert_crit = .trial$trial_revert_crit)
      # Save particle samples (if possible)
      if (continue) {
        snapshot <- .pf_snapshot(.dt = pnow, .save = .record$save,
                                 .select = select_cols, .cols = .record$cols)
        if (.record$save) {
          history[[t]] <- snapshot
        }
        .pf_write_particles_abbr(.particles = snapshot)
      } else {
        # For convergence failures, collate outputs & return up to current time step
        out <- .pf_forward_output(.rerun = .rerun,
                                  .start = t_onset,
                                  .startup = startup,
                                  .history = history,
                                  .diagnostics = diagnostics,
                                  .convergence = FALSE)
        return(out)
      }

      #### Move on
      ppast <- .pf_forward_increment(.particles = pnow, .obs = .obs, .t = t)
      t <- t + 1L
    }

  }
  pb_close(.pb = pb)

  #### Return outputs
  .pf_forward_output(.rerun = .rerun,
                     .start = t_onset,
                     .startup = startup,
                     .history = history,
                     .diagnostics = diagnostics,
                     .convergence = TRUE)

}
