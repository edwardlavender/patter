#' @title PF: pre-calculate densities for the backward pass
#' @description This function is used to pre-calculate densities for the backward pass.
#'
#' @param .history Particle samples from the forward simulation.
#'
#' If `.in_memory = TRUE`, this can be provided as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object.
#' * An ordered list of file paths (from [`pf_setup_files()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#'
#' If `.in_memory = FALSE`, this must be a character string that defines the directory within which parquet files are stored.
#'
#' @param .dens_step,... A function, and associated arguments, to calculate the density of movements between locations (see [`pf_backward_sampler()`] and [`dstep()`]). This must accept the arguments described in [`pf_backward_sampler()`]:
#' * `.data_now`---a [`data.table`] of particle locations;
#' * `.data_past`---a [`data.table`] of paired particle locations;
#' * (optional) `...`---additional arguments;
#'
#' However, note that the exact form of `.data_now` and `.data_past` differ depending on the implementation (see Details):
#' * For implementation options (1) and (2A), densities are calculated between all cell pairs (i.e., two multi-row [`data.table`]s), by default via [`dstep()`]. Internally, this uses [`terra::distance()`] so you must specify `lonlat` and `pairwise = TRUE`.
#' * For implementation option (2B) `.data_now` is a single row (as described in [`pf_backward_sampler()`]). Under the default implementation, `lonlat` is required but `pairwise` is optional.
#'
#' For custom functions, note that coordinates are stored in each [`data.table`] in `x_now` and `y_now` columns. Other columns are dropped, with some exceptions.
#'
#' @param .in_memory A logical variable that defines whether or not to begin processing in memory (see Details).
#' @param .collect If `.in_memory = FALSE`, `.collect` is an `integer` that controls whether or not particle pairs are brought into memory for density calculations (see Details). If the number of particle pairs exceeds `.collect`, processing continues without bringing the data fully into memory.
#' @param .store If `.in_memory = FALSE` and `.collect` is exceeded, `.store` is required. This is a character string that defines the path to an empty directory within which to write files.
#' @param .cl,.varlist Parallelisation options for implementation 2B (see Details and [`cl_lapply()`]).
#' @param .verbose,.txt Arguments to monitor function progress (see [`pf_forward()`]).
#'
#' @details
#' The backward pass sweeps backwards in time through particle samples to reconstruct movement paths (see [`pf_backward_sampler()`]). For each particle, at each time step this algorithm samples a preceding location according to the probability densities connecting that particle to all particles from the preceding time step. The purpose of this function is to identify the set of unique transitions between particle pairs and pre-calculate probability densities for [`pf_backward_sampler()`].
#'
#' There are five steps:
#' 1. Identify cells. The first step is to identify set of unique locations (grid cells) sampled (by the forward filter) at each time step.
#' 2. Identify transitions. The second step is to reconstruct the set of possible transitions between cells at each time step. This requires a cross-join operation (at each time step, each (unique) cell is paired against all (unique) cells from the previous time step). This is memory intensive and can be optionally implemented using `sparklyr` (see below).
#' 3. Identify distinct transitions. The third step is to identify the set of unique cell combinations for which probability densities are required. This is hopefully considerably fewer less than the total number of transitions.
#' 4. Calculate densities. The fourth step is to calculate densities (e.g., via [`dstep()`]).
#' 5. Return outputs. The final step is to return densities in a format that can be efficiently accessed in [`pf_backward_sampler()`] (e.g., via [`dstep_lookup()`] or [`dstep_read()`]).
#'
#' There are three implementation options:
#' 1. `.in_memory` is `TRUE`. Under this option, steps 1--5 are implemented in memory. A `list` is returned that can be accessed in [`pf_backward_sampler()`] via [`dstep_lookup()`].
#' 2. `.in_memory` is `FALSE`. Under this option, steps 1--5 are implemented without bringing the data (fully) into memory. Steps 1--3 are achieved using `sparklyr`. The function establishes a connection to Spark, implements 1--3 using Spark, and counts the resultant number of unique transitions for which densities are required (`n`). What happens next depends on whether or not `n <= .collect`:
#'     * (A) `n <= .collect`. If `n < .collect`, data are brought into memory for steps 4--5. A `list` is returned that can be accessed in [`pf_backward_sampler()`] via [`dstep_lookup()`].
#'     * (B) `n > .collect`. If `n > .collect`, steps 4--5 are implemented in batches:
#'          * For each grid cell, a parquet file is written to `{.store}/pairs/{cell}.parquet` with the coordinates of cell pairs;
#'          * Parquet files are iteratively read into memory, density calculations are implemented and, for each grid cell, a `list` of densities to paired cells is written to `{.store}/density/{cell}.qs`. This step can be optionally parallelised. Limited experimentation suggested that this approach is faster than a full Spark implementation. The resultant files can be accessed in [`pf_backward_sampler()`] via [`dstep_read()`]. The function itself returns `invisible(NULL)`.
#'
#'
#' @return The function returns:
#' * Implementations 1 and 2A: a named, nested `list`
#' * Implementation 2B: `invisible(NULL)` (see Details)
#'
#' @examples
#' # See ?`pf_backward_sampler()` for examples
#'
#' @seealso
#' * [`pf_forward()`] to implement the forward simulation;
#' * [`pf_backward_*()`] implements the backward pass;
#' * [`pf_path()`] reconstructs movement paths;
#' * [`pf_map_pou()`] and [`pf_map_dens()`] generate maps of space use;
#' * [`pf_setup_files()`] and [`pf_coords()`] are helper functions;
#' @author Edward Lavender
#' @name pf_backward_dens

#' @rdname pf_backward_dens
#' @export

pf_backward_dens <- function(.history, .dens_step, ...,
                             .in_memory = TRUE,
                             .collect = 1e9, .store = NULL,
                             .cl = NULL, .varlist = NULL,
                             .verbose = TRUE, .txt = "") {
  if (.in_memory) {
    .pf_backward_dens_mem(.history = .history,
                          .dens_step = .dens_step, ...,
                          .verbose = .verbose, .txt = .txt)
  } else {
    .pf_backward_dens_spark(.history = .history,
                            .dens_step = .dens_step, ...,
                            .collect = .collect, .store = .store,
                            .verbose = .verbose, .txt = .txt,
                            .cl = .cl, .varlist = .varlist)
  }
}

#' @title PF: run the backward pass
#' @description These functions implement backwards sampling of particle samples.
#'
#' @param .data_now,.data_past,.density Arguments for `.dens_step*` functions.
#'  * `.data_now` and `.data_past` are [`data.table`]s (see `.dens_step`), below;
#'  * `.density` is either:
#'      * A `list` that contains movement densities (for [`dstep_lookup()`]) from [`pf_backward_dens()`];
#'      * A `character` string that defines the directory to `parquet` files containing movement densities (for [`dstep_read()`]) from [`pf_backward_dens()`];
#'
#' @param .history Particle samples from the forward simulation, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object.
#' * An ordered list of file paths (from [`pf_setup_files()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .dens_step,... A function, and associated inputs, used to calculate the probability density of movements between particle samples. `.dens_step` must accept the following arguments:
#'    * `.data_now`---a one-row [`data.table`] that defines the current particle sample (as in `.history`).
#'    * `.data_past`---a multi-row [`data.table`] that defines all particle samples for the previous time step (as in `.history`).
#'    * (optional) `...`---additional arguments passed from [`pf_backward_p()`].
#'
#' Three helper functions are provided:
#' * [`dstep()`] calculates distances between current and past particle samples (via [`terra::distance()`]) and translates these into probability densities (via [`dtruncgamma()`]). Arguments passed via `...` are passed to both [`terra::distance()`] (which requires a `lonlat` input) and [`dtruncgamma()`].
#' * [`dstep_lookup()`] looks up pre-calculated density values from an object in memory (from XXX);
#' * [`dstep_read()`] reads selected pre-calculated density values (from XXX) into memory. This requires the `qs` package but, for speed, performs no checks as to whether it is available.
#'
#' @param .save_history A logical variable that defines whether or not to save updated particle samples in memory (see [`pf_forward()`]).
#' @param .write_history A named list, passed to [`arrow::write_parquet()`], to write updated particle samples to file (see [`pf_forward()`]).
#' @param .progress,.cl,.varlist (optional) Parallelisation options. Parallelisation is implemented over particles.
#' * `.progress` is a logical variable that defines whether or not to show a progress bar.
#' * `.cl` and `.varlist` are cluster controls passed to [`cl_lapply()`].
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
#' Note that the algorithm requires large numbers of calculations. Under default options, calculations are implemented on-the-fly, but it may be more efficient to pre-compute the set of distances between the set of unique particle pairs before implementation of the backward sampler.
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
#' @name pf_backward_p

#' @rdname pf_backward_p
#' @export

dstep_lookup <- function(.data_now, .data_past, .density) {
  rbindlist(.density[[as.character(.data_now$cell_now)]][as.character(.data_past$cell_now)])$density
}

#' @rdname pf_backward_p
#' @export

dstep_read <- function(.data_now, .data_past, .density) {
  # Read densities for movements from .data_now$cell_now
  dens <- qs::qread(file.path(.density, as.character(.data_now$cell_now), "density.qs"))
  # Extract densities for cell_now to .data_past$cell_now
  unlist(dens[as.character(.data_past$cell_now)])
}

#' @rdname pf_backward_p
#' @export

pf_backward_sampler <- function(.history,
                                .dens_step = dstep, ...,
                                .save_history = FALSE, .write_history = NULL,
                                .cl = NULL, .varlist = NULL,
                                .progress = TRUE, .verbose = TRUE, .txt = ""
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
  n_particle <- nrow(.history[[1]])
  n_step     <- length(.history)
  density    <- NULL
  # Control progress bar (over particles, not time steps)
  if (!.progress) {
    pbo <- pbapply::pboptions(type = "none")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }

  #### Generate paths
  cat_to_cf("... Generating path(s)...")
  paths <-
    cl_lapply(seq_len(n_particle),
              .cl = .cl,
              .varlist = .varlist,
              .fun = function(i) {

                #### Set up loop
                cat_to_cf(paste("... ... On particle", i, "..."))
                cat_to_cf("... ... ... Preparing to run sampler...")
                if (read_history) {
                  .history[[n_step]] <- arrow::read_parquet(.history[[n_step]])
                }
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
                  index <- sample.int(n_particle, size = 1, prob = dens)
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
