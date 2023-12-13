#' @title PF: isolate distinct samples
#' @description This function identifies distinct particle (cell) samples from the forward filter for [`pf_backward_sampler()`].
#' @param .history Particle samples from the backward pass, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object.
#' * An ordered list of file paths (from [`pf_setup_files()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .cl,.cl_varlist,.cl_chunks Parallelisation options, passed to [`cl_lapply()`].
#' @param .save_opts,.write_opts Arguments to save particle histories in memory or write them to file (see [`pf_backward_sampler()`]).
#' @details
#' This function iterates over time steps, identifies distinct cells and saves these in memory and/or writes them to file for [`pf_backward_sampler()`]. This is designed to improve speed in [`pf_backward_sampler()`], which only requires distinct samples.
#' @return The function returns a `list` of distinct particle samples (if `.save_opts = TRUE`) or `NULL`. If `.write_opts` is supplied, distinct particle samples are written to file.
#' @author Edward Lavender
#' @export

pf_distinct <- function(.history,
                        .cl = NULL, .cl_varlist = NULL, .cl_chunks = TRUE,
                        .save_opts = FALSE, .write_opts = list()) {
  if (length(.history) == 1L && dir.exists(.history)) {
    .history <- pf_setup_files(.history)
  }
  read  <- inherits(.history[[1]], "character")
  write <- rlang::has_name(.write_opts, "sink")
  history <-
    cl_lapply(.history,
                       .cl = .cl, .varlist = .cl_varlist, .use_chunks = .cl_chunks,
                       .fun = function(d) {
                         # Read cells
                         if (read) {
                           d <- arrow::read_parquet(d,
                                                    col_select = c("timestep", "cell_now", "x_now", "y_now"))
                         }
                         # Identify distinct cells
                         d <-
                           d |>
                           dplyr::select("timestep", "cell_now", "x_now", "y_now") |>
                           dplyr::distinct(.data$cell_now, .keep_all = TRUE)
                         # Save oututs
                         if (write) {
                           arrow::write_parquet(d,
                                                file.path(.write_opts, paste0(d$timestep[1], ".parquet")))
                         }
                         if (.save_opts) {
                           .history
                         } else {
                           NULL
                         }
                       })

  # Return outputs
  if (.save_opts) {
    history
  } else {
    invisible(NULL)
  }
}

#' @title PF: run the backward pass
#' @description These functions implement backwards sampling of particle samples.
#' @param .history Particle samples from the forward simulation, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object.
#' * An ordered list of file paths (from [`pf_setup_files()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .dens_step,... A function, and associated inputs, used to calculate the probability density of movements between particle samples. `.dens_step` must accept the following arguments:
#'    * `.data_now`---a one-row [`data.table`] that defines the current particle sample (as in `.history`).
#'    * `.data_past`---a multi-row [`data.table`] that defines all particle samples for the previous time step (as in `.history`).
#'    * (optional) `...`---additional arguments passed from [`pf_backward_sampler()`].
#'
#' Three helper functions are provided:
#' * [`dstep()`] calculates distances between current and past particle samples (via [`terra::distance()`]) and translates these into probability densities (via [`dtruncgamma()`]). Arguments passed via `...` are passed to both [`terra::distance()`] (which requires a `lonlat` input) and [`dtruncgamma()`].
#' @param .save_history A logical variable that defines whether or not to save updated particle samples in memory (see [`pf_forward()`]).
#' @param .write_history A named list, passed to [`arrow::write_parquet()`], to write updated particle samples to file (see [`pf_forward()`]).
#' @param .cl,.cl_varlist,.cl_chunks (optional) Parallelisation options passed to [`cl_lapply()`]. Parallelisation is implemented over particles.
#' @param .verbose,.txt Arguments to monitor function progress (see [`pf_forward()`]).
#'
#' @details
#'
#' # Overview
#'
#' The forward filter ([`pf_forward()`]) reconstructs the possible locations of an individual at each time step, given the previous time step. Backwards sampling refines the particle filter in two ways:
#'
#' * We mitigate the issue of particle degeneracy;
#' * We reconstruct entire trajectories;
#'
#' # Pseudocode
#'
#' In outline, the backward pass (backward sampling) proceeds as follows:
#' * Identify the final particle samples;
#' * For each particle:
#'      * Calculate the probability density of movements from that particle to all particles at the previous time step via a `.dens_step` function.
#'      * In practice, this typically requires calculating the distances between particle samples and translating these into densities using the movement model;
#' * Sample a selected particle at the previous time step, in line with the probability densities linking each pair of particles;
#' * Repeat this process until the start of the time series;
#' * The process can be implemented in parallel for each particle;
#'
#' Note that the algorithm requires large numbers of calculations. Under default options, calculations are implemented on-the-fly, but it may be more efficient to pre-compute the set of distances between the set of unique particle pairs before implementation of the backward sampler. However, this is not currently implemented.
#'
#' @example man/examples/pf_backward_sampler-examples.R
#'
#' @return The function returns a [`pf_path-class`] object.
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward()`];
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_setup_files()`];
#'
#' * The backward pass is implemented by [`pf_backward_*()`];
#'
#' * Movement paths are built from PF outputs via `pf_path()` functions:
#'     * [`pf_path()`] reconstructs paths;
#'     * [`pf_path_pivot()`] supports path reconstruction;
#'
#' * To reconstruct maps of space use, see:
#'     * [`pf_coords()`] to extract particle coordinates;
#'     * [`pf_map_pou()`] for probability-of-use maps;
#'     * [`pf_map_dens()`] for smooth utilisation distributions;
#'     * [`get_hr()`] for home range estimates;
#'
#' @author Edward Lavender
#' @export

pf_backward_sampler <- function(.history,
                                .dens_step = dstep, ...,
                                .save_history = FALSE, .write_history = NULL,
                                .cl = NULL, .cl_varlist = NULL, .cl_chunks = TRUE,
                                .verbose = TRUE, .txt = ""
) {

  #### Check user inputs
  t_onset <- Sys.time()
  check_inherits(.history, "list")
  if (!.save_history && is.null(.write_history)) {
    abort("`.save_history = FALSE` and `.write_history = NULL`. There is nothing to do.")
  }
  write_history_folder <- .pf_check_write_history(.write_history)

  #### Set up messages (as usual)
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_backward_*() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_backward_*() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Set up loop
  cat_to_cf("... Set up...")
  # Define whether or not to read history files
  if (inherits(.history[[1]], "data.frame")) {
    read_history <- FALSE
  } else {
    read_history <- TRUE
  }
  # Define constants
  n_step     <- length(.history)
  if (read_history) {
    .history[[n_step]] <- arrow::read_parquet(.history[[n_step]])
  }
  n_particle <- fnrow(.history[[n_step]])
  density    <- NULL

  #### Generate paths
  cat_to_cf("... Generating path(s)...")
  paths <-
    cl_lapply(seq_len(n_particle),
              .cl = .cl,
              .varlist = .cl_varlist,
              .use_chunks = .cl_chunks,
              .fun = function(i) {

                #### Set up loop
                cat_to_cf(paste("... ... On particle", i, "..."))
                cat_to_cf("... ... ... Preparing to run sampler...")
                path <- dens <- list()
                path[[n_step]] <- .history[[n_step]][i, ]
                path[[n_step]][, density := 1]

                #### Run backwards sampler for a selected particle (i)
                cat_to_cf("... ... ... Running sampler...")
                for (t in n_step:2) {
                  # Read history if necessary
                  cat_to_cf(paste("... ... ... ... On time step", t, "..."))
                  if (read_history) {
                    # Drop history for t (to save memory) & read new history
                    .history[[t]] <- NA
                    .history[[t - 1]] <- arrow::read_parquet(.history[[t - 1]])
                  }
                  # Calculate step densities
                  dens <- .dens_step(.data_now = path[[t]],
                                     .data_past = .history[[t - 1]], ...)
                  # Sample a previous location
                  index <- sample.int(fnrow(.history[[t - 1]]), size = 1, prob = dens)
                  path[[t - 1]] <- .history[[t - 1]][index, ]
                  path[[t - 1]][, density := dens[index]]
                }

                #### Collate path
                cat_to_cf("... ... ... Collating paths...")
                path <-
                  path |>
                  rbindlist() |>
                  mutate(path_id = i, .before = 1L) |>
                  as.data.table()

                #### Save path
                cat_to_cf("... ... ... Recording path...")
                if (!is.null(.write_history)) {
                  .write_history$x    <- path
                  .write_history$sink <- file.path(write_history_folder, paste0(t, ".parquet"))
                  do.call(arrow::write_parquet, .write_history)
                }
                # Save path in memory
                if (.save_history) {
                  return(path)
                } else {
                  return(NULL)
                }

              })

  #### Return outputs
  cat_to_cf("... Completing simulation...")
  if (is.null(paths[[1]])) {
    paths <- NULL
  } else {
    paths <-
      paths |>
      rbindlist() |>
      arrange(.data$path_id, .data$timestep) |>
      as.data.table()
  }
  t_end <- Sys.time()
  time <- list(start = t_onset,
               end = t_onset,
               duration = difftime(t_end, t_onset))
  out <- list(path = paths,
              time = time)

  #### Return outputs
  class(out) <- c(class(out), "pf_path")
  out
}
