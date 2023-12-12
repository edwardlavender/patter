#' @title PF: run the backward pass
#' @description This function implements the backward correction of particle samples.
#' @param .history Particle samples from the forward simulation, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object. This must contain columns that define cell samples at each time step (`cell_now`) alongside previous samples (`cell_past`).
#' * An ordered list of file paths (from [`pf_setup_files()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .save_history A logical variable that defines whether or not to save updated particle samples in memory (see [`pf_forward()`]).
#' @param .write_history A named list, passed to [`arrow::write_parquet()`], to write updated particle samples to file (see [`pf_forward()`]).
#' @param .progress,.verbose,.txt Arguments to monitor function progress (see [`pf_forward()`]).
#'
#' @details At the time of writing, this function only removes 'dead ends' from particle samples. Backwards smoothing is not currently implemented.
#'
#' @example man/examples/pf_backward_killer-examples.R
#'
#' @return The function returns a [`pf-class`] object.
#'
#' @seealso
#' TO DO
#'
#' @author Edward Lavender
#' @export

pf_backward_killer <- function(.history,
                               .save_history = FALSE, .write_history = NULL,
                               .progress = TRUE, .verbose = TRUE, .txt = "") {

  #### Check user inputs
  t_onset <- Sys.time()
  check_inherits(.history, "list")
  if (!.save_history && is.null(.write_history)) {
    abort("`.save_history = FALSE` and `.write_history = NULL`. There is nothing to do.")
  }
  write_history_folder <- .pf_check_write_history(.write_history)

  #### Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_backward_*() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_backward_*() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Set up loop
  # Define whether or not .history dataframes need to be read from file
  if (inherits(.history[[1]], "data.frame")) {
    read_history <- FALSE
  } else {
    read_history <- TRUE
  }
  timestep_final <- length(.history)
  # Define progress bar
  pb <- pb_init(.n = timestep_final, .init = 0L, .progress = .progress)

  #### Implement loop
  for (t in rev(seq_len(timestep_final))) {

    #### Read particle samples for t and t - 1
    pb_tick(.pb = pb, .t = (timestep_final - t) + 1L, .progress = .progress)
    cat_to_cf(paste0("... Time step ", t, ":"))
    if (read_history) {
      if (t == timestep_final) {
        cat_to_cf(paste0("... ... Reading `.history[[", timestep_final, "]]`..."))
        .history[[t]] <- arrow::read_parquet(.history[[t]])
      }
      if (t > 1) {
        cat_to_cf(paste0("... ... Reading `.history[[", t - 1, "]]`..."))
        .history[[t - 1]] <- arrow::read_parquet(.history[[t - 1]])
      }
    }

    #### Filter particle samples
    # cell_past for current time step should match cell_now for previous one
    if (t < timestep_final && t > 1) {
      cat_to_cf(paste0("... ... Cleaning `.history[[", t - 1, "]]`..."))
      cat_to_cf(paste0("... ... Identifying `cell_now` (for the previous step) that match `cell_past` (for the current step)..."))
      cat_to_cf(paste0("... ... ... Input: ", nrow(.history[[t]]), " rows in `.history[[t]]`..."))
      bool <- .history[[t - 1]]$cell_now %in% .history[[t]]$cell_past
      if (!all(bool)) {
        cat_to_cf(paste0("... ... ... Filtering ", length(which(!bool)), " dead ends (", length(which(bool)), " remain)..."))
        .history[[t - 1]] <- .history[[t - 1]] |> filter(bool) |> as.data.table()
        cat_to_cf(paste0("... ... ... Output: ", nrow(.history[[t - 1]]), " rows in `.history[[t - 1]]`..."))
      }
    }

    #### Save particles
    # Write particles to file
    cat_to_cf(paste0("... ... Recording (cleaned) outputs for `.history[[", t, "]]`..."))
    if (!is.null(.write_history)) {
      .write_history$x    <- .history[[t]]
      .write_history$sink <- file.path(write_history_folder, paste0(t, ".parquet"))
      do.call(arrow::write_parquet, .write_history)
    }
    # Drop saved particles for current time step, if necessary
    if (!.save_history) {
      .history[[t]] <- NA
    }

  }
  pb_close(.pb = pb, .progress = .progress)

  #### Return outputs (modified from pf_forward())
  if (!.save_history) {
    .history <- list()
  }
  t_end <- Sys.time()
  time <- list(start = t_onset,
               end = t_onset,
               duration = difftime(t_end, t_onset))
  out <- list(history = .history,
              time = time)

  #### Return outputs
  class(out) <- c(class(out), "pf")
  out

}


#' @title PF: backward killer diagnostics
#' @description This function collates diagnostics from [`pf_backward_killer()`] outputs.
#' @param .sink A character string that defines the directory containing outputs.
#' @param .cl,.cl_varlist,.cl_chunks (optional) Cluster options, passed to [`cl_lapply()`].
#'
#' @return The function returns a [`data.table`] with the following columns:
#' * `timestep`---an `integer` that defines the timestep;
#' * `n`---an `integer` that defines the number of particles;
#' * `n_u`---an `integer` that defines the number of unique location samples;
#' * `ess`---a `double` that defines the effective sample size (see [`.pf_diag_ess()`]);
#'
#' @author Edward Lavender
#' @export

pf_backward_killer_diagnostics <- function(.sink,
                                           .cl = NULL, .cl_varlist = NULL, .cl_chunks = TRUE) {
  .history <- pf_setup_files(.sink)
  cl_lapply(.history,
            .cl = .cl, .varlist = .cl_varlist,
            .use_chunks = .cl_chunks,
            .fun = function(f) {
              d <- arrow::read_parquet(f)
              data.table(timestep = d$timestep[1],
                         n = fnrow(d),
                         n_u = .pf_diag_unique(d$cell_now),
                         ess = .pf_diag_ess(d$lik)
              )
            }) |>
    rbindlist()
}
