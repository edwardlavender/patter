#' @title PF: backward pass
#' @description This function implements the backward correction of particle samples.
#'
#' @param .history Particle samples from the forward simulation, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf_particles-class`] object. This must contain columns that define cell samples at each time step (`cell_now`) alongside previous samples (`cell_past`).
#' * An ordered list of file paths (from [`pf_files()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .record A named `list` of output options, from [`pf_opt_record()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details This function removes 'dead ends' from particle samples.
#'
#' @example man/examples/pf_backward_killer-examples.R
#'
#' @return The function returns a [`pf_particles-class`] object.
#'
#' @seealso
#'
#' @author Edward Lavender
#' @export

pf_backward_killer <- function(.history,
                               .record = pf_opt_record(),
                               .verbose = getOption("patter.verbose")) {

  #### Check user inputs
  # TO DO (enhance checks)
  t_onset <- Sys.time()
  check_inherits(.history, "list")

  #### Set up messages
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_backward_killer", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_backward_killer", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Set up loop
  # Define whether or not .history dataframes need to be read from file
  if (inherits(.history[[1]], "data.frame")) {
    read_history <- FALSE
  } else {
    read_history <- TRUE
  }
  # Variables
  write          <- !is.null(.record$sink)
  timestep_final <- length(.history)
  # Define progress bar
  pb <- pb_init(.min = 0L, .max = timestep_final)

  #### Implement loop
  for (t in rev(seq_len(timestep_final))) {

    #### Read particle samples for t and t - 1
    pb_tick(.pb = pb, .t = (timestep_final - t) + 1L)
    cat_log(paste0("... Time step ", t, ":"))
    if (read_history) {
      if (t == timestep_final) {
        cat_log(paste0("... ... Reading `.history[[", timestep_final, "]]`..."))
        .history[[t]] <- arrow::read_parquet(.history[[t]])
      }
      if (t > 1) {
        cat_log(paste0("... ... Reading `.history[[", t - 1L, "]]`..."))
        .history[[t - 1L]] <- arrow::read_parquet(.history[[t - 1L]])
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
  .history <- pf_files(.sink)
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
