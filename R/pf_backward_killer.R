#' @title PF: backward killer
#' @description This function implements backward pruning (killing) of particle samples.
#'
#' @param .history Particle samples from the forward simulation, provided in any format accepted by [`.pf_history_list()`]. Particle samples must contain `cell_past` and `cell_now` columns.
#' @param .record A named `list` of output options, from [`pf_opt_record()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details This function removes 'dead ends' from particle samples. If you imagine the forward simulation, as implemented by [`pf_forward()`], as a branching process, like the growth of a fungal network between two points in space (representing the start and end of the time series), dead ends are the side branches that emerge during this process that do not reach the destination (because sooner-or-later they are rendered incompatible with the data). This function prunes dead-ends from the time series by running a fast `match`ing process backwards in time and retaining the subset of particle samples that lead to the destination particle samples. This process is very fast, and you can use the results to reconstruct movement paths (via [`pf_path()`]) and maps of space use (via `map_*()` functions), but crude.
#'
#' There are two, related limitations with the 'prune' methodology. The first is that the removal of dead ends tends to bias particle samples, because early samples (which invariably sooner-or-later end up on a dead-end) are more likely to get killed than later samples. This is known as particle degeneracy. Use the [`pf_diag_summary()`] function to evaluate trends in the effective sample size through time and examine whether this is an issue. The second is that while particles from the forward simulation are contingent upon the past (a marginal distribution), they do not embody information from the future (the joint distribution).
#'
#' To reconstruct the joint distribution of particle samples given all data (i.e., 'proper' movement trajectories), the backward sampler is required instead (see [`pf_backward_sampler`]). However, this is much more expensive.
#'
#' This function replaces functionality in [`flapper::pf_simplify()`](https://edwardlavender.github.io/flapper/reference/pf_simplify.html).
#'
#' @example man/examples/pf_backward_killer-examples.R
#'
#' @return The function returns a [`pf_particles-class`] object.
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @export

pf_backward_killer <- function(.history,
                               .record = pf_opt_record(),
                               .verbose = getOption("patter.verbose")) {

  #### Check user inputs
  # TO DO

  #### Set up messages
  t_onset <- Sys.time()
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_backward_killer", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_backward_killer", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Set up loop
  .history  <- .pf_history_list(.history)
  read      <- .pf_history_read(.history)
  inout     <- .pf_history_cols(.history = .history, .record = .record,
                                .input_cols = c("cell_past", "cell_now"))
  .record   <- inout$.record
  read_cols <- inout$read_cols
  write     <- .pf_history_write(.record)
  # Global variables
  logwt <- NULL

  # Define progress bar
  timestep_final <- length(.history)
  pb <- pb_init(.min = 0L, .max = timestep_final)

  #### Implement loop
  for (t in rev(seq_len(timestep_final))) {

    #### Read particle samples for t and t - 1
    # For speed, we only use .pf_history_elm() if read = TRUE
    tp <- t - 1L
    pb_tick(.pb = pb, .t = (timestep_final - t) + 1L)
    cat_log(paste0("... Time step ", t, ":"))
    if (read) {
      if (t == timestep_final) {
        cat_log(paste0("... ... Reading `.history[[", timestep_final, "]]`..."))
        .history[[t]] <- .pf_history_elm(.history = .history, .elm = t, .read = TRUE, .cols = read_cols)
      }
      if (t > 1) {
        cat_log(paste0("... ... Reading `.history[[", tp, "]]`..."))
        .history[[tp]] <- .pf_history_elm(.history = .history, .elm = tp, .read = TRUE, .cols = read_cols)
      }
    }

    #### Filter particle samples
    # cell_past for current time step should match cell_now for previous one
    if (t < timestep_final && t > 1) {
      cat_log(paste0("... ... Cleaning `.history[[", tp, "]]`..."))
      cat_log(paste0("... ... Identifying `cell_now` (for the previous step) that match `cell_past` (for the current step)..."))
      cat_log(paste0("... ... ... Input: ", nrow(.history[[t]]), " rows in `.history[[t]]`..."))
      bool <- .history[[tp]]$cell_now %in% .history[[t]]$cell_past
      if (!all(bool)) {
        cat_log(paste0("... ... ... Filtering ", length(which(!bool)), " dead ends (", length(which(bool)), " remain)..."))
        .history[[tp]] <-
          .history[[tp]] |>
          filter(bool) |>
          as.data.table()
        cat_log(paste0("... ... ... Output: ", nrow(.history[[tp]]), " rows in `.history[[t - 1]]`..."))
        # Re-normalise weights
        if (rlang::has_name(.history[[tp]], "logwt")) {
          .history[[tp]][, logwt := lognormalise(logwt)]
        }
      }
    }

    #### Save particles
    # Write particles to file
    cat_log(paste0("... ... Recording (cleaned) outputs for `.history[[", t, "]]`..."))
    .history[[t]] <- .pf_snapshot(.dt = .history[[t]], .save = .record$save,
                                  .select = !is.null(.record$cols), .cols = .record$cols)
    .pf_write_particles(.particles = .history[[t]], .sink = .record$sink,
                        .filename = t, .write = write)
    # Drop saved particles for current time step, if necessary
    if (!.record$save) {
      .history[[t]] <- NA
    }
  }
  pb_close(.pb = pb)

  #### Return outputs
  .pf_backward_output(.start = t_onset,
                      .history = .history,
                      .record = .record)

}
