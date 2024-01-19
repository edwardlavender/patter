#' @title PF: backward killer
#' @description This function implements backward pruning (killing) of particle samples.
#'
#' @param .history Particle samples from the forward simulation, provided in any format accepted by [`.pf_history_list()`]. Particle samples must contain `cell_past` and `cell_now` columns.
#' @param .record A named `list` of output options, from [`pf_opt_record()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details This function removes 'dead ends' from particle samples. If you imagine the forward simulation, as implemented by [`pf_forward()`], as a branching process, like the growth of a fungal network between two points in space (representing the start and end of the time series), dead ends are the side branches that emerge during this process that do not reach the destination (because sooner-or-later they are rendered incompatible with the data). This function prunes dead-ends from the time series by running a fast `match`ing process backwards in time and retaining the subset of particle samples that lead to the destination particle samples. This process is very fast, and you can use the results to reconstruct movement paths (via [`pf_path()`]) and maps of space use (via `map_*()` functions), but crude.
#'
#' There are two, related limitations with the 'prune' methodology. The first is that the removal of dead ends tends to bias particle samples, because early samples (which invariably sooner-or-later end up on a dead-end) are more likely to get killed than later samples. This is known as particle degeneracy. Use the [`pf_backward_killer_diagnostics()`] function to evaluate trends in the effective sample size through time and examine whether this is an issue. The second is that while particles from the forward simulation are contingent upon the past (a marginal distribution), they do not embody information from the future (the joint distribution).
#'
#' To reconstruct the joint distribution of particle samples given all data (i.e., 'proper' movement trajectories), the backward sampler is required instead ([`pf_backward_sampler()`]). However, this is much more expensive.
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
  # History
  .history <- .pf_history_list(.history)
  read     <- .pf_history_read(.history)
  check_names(.pf_history_elm(.history, .elm = 1L, .read = read, .cols = .record$cols),
              c("cell_past", "cell_now"))
  # Variables
  write          <- !is.null(.record$sink)
  # Define progress bar
  timestep_final <- length(.history)
  pb <- pb_init(.min = 0L, .max = timestep_final)

  #### Implement loop
  for (t in rev(seq_len(timestep_final))) {

    #### Read particle samples for t and t - 1
    # For speed, we use arrow::read_parquet() directly rather than .pf_history_elm()
    pb_tick(.pb = pb, .t = (timestep_final - t) + 1L)
    cat_log(paste0("... Time step ", t, ":"))
    if (read) {
      if (t == timestep_final) {
        cat_log(paste0("... ... Reading `.history[[", timestep_final, "]]`..."))
        .history[[t]] <- arrow::read_parquet(.history[[t]],
                                             col_select = .record$cols)
      }
      if (t > 1) {
        cat_log(paste0("... ... Reading `.history[[", t - 1L, "]]`..."))
        .history[[t - 1L]] <- arrow::read_parquet(.history[[t - 1L]],
                                                  col_select = .record$cols)
      }
    }

    #### Filter particle samples
    # cell_past for current time step should match cell_now for previous one
    if (t < timestep_final && t > 1) {
      cat_log(paste0("... ... Cleaning `.history[[", t - 1, "]]`..."))
      cat_log(paste0("... ... Identifying `cell_now` (for the previous step) that match `cell_past` (for the current step)..."))
      cat_log(paste0("... ... ... Input: ", nrow(.history[[t]]), " rows in `.history[[t]]`..."))
      bool <- .history[[t - 1]]$cell_now %in% .history[[t]]$cell_past
      if (!all(bool)) {
        cat_log(paste0("... ... ... Filtering ", length(which(!bool)), " dead ends (", length(which(bool)), " remain)..."))
        .history[[t - 1]] <- .history[[t - 1]] |> filter(bool) |> as.data.table()
        cat_log(paste0("... ... ... Output: ", nrow(.history[[t - 1]]), " rows in `.history[[t - 1]]`..."))
      }
    }

    #### Save particles
    # Write particles to file
    cat_log(paste0("... ... Recording (cleaned) outputs for `.history[[", t, "]]`..."))
    .history[[t]] <- .pf_snapshot(.dt = .history[[t]], .save = .record$save,
                                  .select = !is.null(.record$cols), .cols = .record$cols)
    .pf_write_particles(.particles = .history[[t]], .sink = .record$sink, .write = write)
    # Drop saved particles for current time step, if necessary
    if (!.record$save) {
      .history[[t]] <- NA
    }
  }
  pb_close(.pb = pb)

  #### Return outputs
  .pf_backward_killer_outputs(.start = t_onset,
                              .history = .history,
                              .record = .record)

}

#' @title PF: backward killer diagnostics
#' @description This function calculates diagnostics from particle samples from [`pf_backward_killer()`].
#' @param .history Particle samples, provided in any format accepted by [`.pf_history_dt()`].
#' @param ... Arguments passed to [`.pf_history_dt()`], excluding `.collect` with is necessarily `TRUE`.
#'
#' @details Particle diagnostics are fully described in [`pf_diag`].
#'
#' @return The function returns a [`data.table`] with the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * `n`---an `integer` that defines the number of particles;
#' * `n_u`---an `integer` that defines the number of unique location samples (see [`.pf_diag_nu()`]);
#' * `ess`---a `double` that defines the effective sample size (see [`.pf_diag_ess()`]);
#'
#' @example man/examples/pf_backward_killer_diagnostics-examples.R
#'
#' @inherit pf_diag seealso
#' @author Edward Lavender
#' @export

pf_backward_killer_diagnostics <- function(.history, ...) {
  .history |>
    .pf_history_dt(..., .collect = TRUE) |>
    group_by(.data$timestep) |>
    summarise(timestep = .data$timestep[1],
              n = n(),
              n_u = .pf_diag_nu(.data$cell_now),
              ess = .pf_diag_ess(.data$lik)
    ) |>
    as.data.table()
}
