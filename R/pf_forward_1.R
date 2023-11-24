#' @title PF: run the forward simulation
#' `r lifecycle::badge("superseded")`
#'
#' @description This function implements forward simulation of possible locations.
#'
#' This function is no longer being actively developed. [`acs()`]/[`dc()`] plus [`pf_forward_1()`] will remain available, but [`pf_forward_2()`] is faster, more advanced and recommended.
#'
#' @param .obs A [`data.table`] that defines the time series of observations (see [`acs()`]). For [`pf_forward_1()`], at a minimum, this must contain the following column(s):
#' * `timestep`---an `integer` that defines the time step;
#' * Any columns required by `.kick` (see below);
#' @param .bathy (optional) A [`SpatRaster`] over the region of interest, optionally used by `.kick()`.
#' @param .record A list of [`SpatRaster`]s, or a character vector of file paths to [`SpatRaster`]s (see [`pf_setup_files()`]), that define the set of possible locations of the individual according to the data (i.e., an AC* algorithm).
#' @param .kick,.lonlat,... A function, and associated inputs, used to 'kick' particles into new (proposal) locations. `.kick` must support the following inputs:
#' * `.particles`---a [`data.table`] that defines the `cell` IDs and associated coordinates (`x_now` and `y_now`) of current particle samples;
#' * (optional) `.obs`---the `.obs` [`data.table`];
#' * (optional) `.t`---the `timestep` (used to index `.obs`);
#' * (optional) `.bathy`---the bathymetry [`SpatRaster`];
#' * (optional) `.lonlat`---a `logical` variable that define whether or not particle samples are longitude/latitude or planar coordinates;
#' * (optional) `...`---additional arguments, passed via [`pf_forward_1()`], if required;
#'
#' See [`pf_kick()`] for a template random walk movement model. In this function, `...` are passed to [`rlen()`] and [`rangrw()`].
#' @param .n An `integer` that defines the number of particle samples at each time step.
#' @param .save_history A logical variable that defines whether or not to save particle samples in the `history` element of the output. This is only sensible for small-scale applications (i.e., short time series and few particles).
#' @param .write_history A named list, passed to [`arrow::write_parquet()`], to save particle samples to file at each time step. The `sink` argument should be the directory in which to write files. Files are named by `.obs$timestep` (i.e., `1.parquet`, `2.parquet`, ..., `N.parquet`).
#' @param .progress A logical variable that defines whether or not to implement a progress bar (via [`progress::progress_bar()`]).
#' @param .prompt,.verbose,.txt Controls on function prompts and messages (see [`acs()`]).
#'
#' @details The forward simulation is implemented as follows:
#' 1. At each time step, `.n` grid cells (particles) are sampled (at `t = 1`) or resampled from a set of proposals (at subsequent time steps) with replacement, in line with AC* weights;
#' 2. The previous locations (`NA` for `t = 1`) and the (accepted) current locations are recorded;
#' 3. Each particle is 'kicked' into new (proposal) location (grid cell), by the movement model;
#' 4. Steps 1--3 are repeated until the end of the time series;
#'
#' @return The function returns a [`pf-class`] object.
#'
#' @example man/examples/pf_forward-examples.R
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward_*()`]:
#'     * [`pf_forward_1()`] refines AC-branch algorithm ([`acs()`] and [`dc()`]) outputs using PF;
#'     * [`pf_forward_2()`] is an integrated implementation that couples AC- and PF-branch algorithms internally;
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_setup_files()`];
#'     * Template movement models, namely [`pf_kick()`];
#'
#' * The backward pass is implemented by [`pf_backward()`];
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

pf_forward_1 <- function(.obs,
                         .bathy,
                         .record, .kick, ...,  .lonlat = FALSE,
                         .n = 100L,
                         .save_history = FALSE, .write_history = NULL,
                         .progress = TRUE,
                         .prompt = FALSE, .verbose = TRUE, .txt = "") {

  #### Check user inputs
  t_onset <- Sys.time()
  .pf_check_obs(.obs)
  if (!.save_history && is.null(.write_history)) {
    abort("`.save_history = FALSE` and `.write_history = NULL`. There is nothing to do.")
  }
  write_history_folder <- .pf_check_write_history(.write_history)
  check_dots_for_missing_period(formals(), list(...))

  #### Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_forward_1() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_forward_1() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Set up loop
  history <- list()
  if (inherits(.record[[1]], "SpatRaster")) {
    read_record <- FALSE
  } else {
    read_record <- TRUE
  }
  # Global variables
  pnext <- NULL
  weight <- x_next <- y_next <- NULL
  # Progress
  timestep_final <- max(.obs$timestep)
  if (.progress) {
    pb <- progress::progress_bar$new(total = timestep_final)
    pb$tick(0)
  }

  #### Implement particle filtering
  for (t in .obs$timestep) {
    # t = 1
    if (.progress) pb$tick()
    cat_to_cf(paste0("... Time step ", t, ":"))

    #### Define AC-branch layer
    if (read_record) {
      cat_to_cf(paste0("... ... Reading record..."))
      .record[[t]] <- terra::rast(.record[[t]])
    }

    #### Sample starting set of particles (pnow) by AC weights
    if (t == 1) {
      # Sample particles
      # * More likely particles are more likely to be sampled/sampled more often
      cat_to_cf(paste0("... ... Sampling starting particles..."))
      pnow <-
        .record[[t]] |>
        terra::spatSample(size = .n, method = "weights", replace = TRUE,
                          cells = TRUE, xy = TRUE, values = TRUE, as.df = FALSE) |>
        lazy_dt() |>
        mutate(timestep = 1L,
               cell_past = NA_integer_,
               cell_now = as.integer(.data$cell)) |>
        select("timestep",
               "cell_past", "cell_now",
               x_now = "x", y_now = "y",
               weight = "lyr.1") |>
        as.data.table()
    }

    #### Resample current particles (pnext from previous time step) using weights
    if (t > 1) {
      # Define weights
      cat_to_cf(paste0("... ... Extracting particle weights..."))
      pnow[, weight := terra::extract(.record[[t]], pnow$cell_now, raw = TRUE)[, 1]]
      # Resample from pnow with replacement
      # * Locations into which we have jumped that are more likely are sampled more often
      # * Use data.table & sample.int directly as dplyr::slice_sample()
      # * ... does allow sampling more than the number of rows
      cat_to_cf(paste0("... ... Resampling particles..."))
      pnow <- pnow[which(weight > 0), ]
      if (nrow(pnow) == 0L) {
        msg("There are no particles with positive weights at timestep {t}. `history` returned up to this point.",
            .envir = environment())
        return(history)
      }
      pnow <- pnow[sample.int(.N, size = .n, replace = TRUE, prob = weight), ]
    }
    # Save particles
    # * Create a snapshot here to avoid further modification by reference of history[[t]]
    snapshot <- data.table::copy(pnow)
    if (.save_history) {
      history[[t]] <- snapshot
    }
    if (!is.null(.write_history)) {
      .write_history$x    <- snapshot
      .write_history$sink <- file.path(write_history_folder, paste0(t, ".parquet"))
      do.call(arrow::write_parquet, .write_history)
    }

    #### Kick particles into new (proposal) locations
    if (t < timestep_final) {
      cat_to_cf(paste0("... ... Kicking particles into new locations..."))
      pnext <-
        pnow |>
        # Kick particles:
        .kick(.obs = .obs, .t = t, .bathy = .bathy, .lonlat = .lonlat, ...) |>
        # Coerce particle coordinates onto grid (for pf_backward()):
        mutate(cell_next = as.integer(terra::cellFromXY(.record[[t]], cbind(.data$x_next, .data$y_next))),
               xy = terra::xyFromCell(.record[[t]], .data$cell_next),
               x_next = .data$xy[, 1],
               y_next = .data$xy[, 2]) |>
        as.data.table()
    }

    #### Visualise particle samples & proposal locations
    if (.prompt) {
      cat_to_cf(paste0("... ... Visualising current & proposal locations..."))
      print(utils::head(pnext))
      terra::plot(.record[[t]], main = t)
      if (t < timestep_final) {
        graphics::arrows(x0 = pnext$x_now, y0 = pnext$y_now,
                         x1 = pnext$x_next, y1 = pnext$y_next,
                         length = 0.02)
      }
      readline("Press [Enter] to continue...")
    }

    #### Move loop on
    if (t < timestep_final) {
      cat_to_cf(paste0("... ... Moving on..."))
      # `pnext` becomes `pnow`
      pnow <-
        pnext |>
        mutate(timestep = as.integer(t + 1)) |>
        select("timestep",
               cell_past = "cell_now", cell_now = "cell_next",
               x_now = "x_next", y_now = "y_next") |>
        as.data.table()
      # clean up .record[[t]] for speed
      .record[[t]] <- NA
    }

  }

  #### Return outputs
  t_end <- Sys.time()
  time <- list(start = t_onset,
               end = t_end,
               duration = difftime(t_end, t_onset))
  out <- list(history = history,
              time = time)

  #### Return outputs
  class(out) <- c(class(out), "pf")
  out

}
