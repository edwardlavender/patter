#' @title AC* algorithms
#' @description This function implements the acoustic-container (AC) algorithm and its extensions (e.g., the acoustic-container depth-contour (ACDC) algorithm).
#' @param .obs A [`data.table`] with observations, typically from [`acs_setup_obs()`]. At a minimum, `.obs` is expected to contain the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * `timestamp`---a regular sequences of `POSIXct` time stamps;
#' * `date`---a `character` that defines the date;
#' * `detection_id`---an `integer` vector that uniquely defines each detection;
#' * `detection`---an `integer` that distinguishes the time steps at which detections were (1) or were not (0) recorded;
#' * `receiver_id`---a `list` that defines the receiver(s) that recorded detections at each time step;
#' * `buffer_past`---a `double` that controls container growth from the past to the present;
#' * `buffer_future`---a `double` that controls container shrinkage from the future to the present;
#'
#' Other columns can be included as necessary for `.update_ac` (see below).
#' @param .bathy A [`SpatRaster`] that defines the grid over which the algorithms are implemented. This layer should be the same as that used to define `.detection_overlaps` (via [`acs_setup_detection_containers()`] and [`acs_setup_detection_overlaps()`]) and `.detection_kernels` (via [`acs_setup_detection_kernels()`]).
#' @param .detection_overlaps (optional) A named `list` of detection container overlaps, defined over `.bathy`, from [`acs_setup_detection_overlaps()`]. If un-supplied, it is silently assumed that receiver detection containers do not overlap. If supplied, receiver overlaps are taken into account in detection probability calculations.
#' @param .detection_kernels A named `list` of detection probability kernels, defined over `.bathy`, from [`acs_setup_detection_kernels()`].
#' @param .update_ac,... (optional) A function and additional arguments used to update the [`SpatRaster`] that defines the possible locations of the individual given the data (according to the AC algorithm) at each time step. For example, if you have depth observations, you could use a depth-error model to define a probability surface that describes the possible locations of the individual at each time step and combine this, via `.update_ac`, with the surface from the AC algorithm. Information from other variables can be integrated in the same way. The function must accept five arguments in the following order (even if they are unused):
#' * `.spat`---a [`SpatRaster`] that defines the possible locations of the individual given the data at each time step;
#' * `.bathy`---the `.bathy` [`SpatRaster`] (above);
#' * `.obs`---the `.obs` [`data.table`] (above);
#' * `.t`---an integer that defines the current time step (i.e., row in `.obs`);
#' * `...`---Any additional arguments passed to the function;
#' @param .save_record,.save_cumulative Logical inputs that control options for saving outputs in memory.
#' * `.save_record` defines whether or not to save the record of the possible locations of the individual at each time step in the `record` element of the output. This is only sensible for relatively short time series (use `.write_record`, below, otherwise).
#' * `.save_cumulative` defines whether or not to save a cumulative (probability-of-use) map, derived from the normalised summation of each element in `record` in the `map` element of the output. In general, this is not necessary since AC* outputs are refined by particle filtering (see [`pf_forward()`]) before mapping.
#' @param .write_record A named list, passed to [`terra::writeRaster`], to save the `record` [`SpatRaster`]s to file at each time step. The `filename` argument should define the directory in which to write files. Files are named by `.obs$timestep` (i.e., `1.tif`, `2.tif`, ..., `N.tif`). This is typically desirable but considerably reduces speed.
#' @param .progress A logical variable that defines whether or not to implement a progress bar (via [`progress::progress_bar()`]).
#' @param .prompt A logical variable that defines whether or not a user prompt is required between time steps. If provided, the function plots the possible locations of the individual at each time step. This is useful for diagnostics.
#' @param .verbose A logical variable that defines whether or not to print messages to the console or to file to relay function progress. If `con = ""`, messages are printed to the console; otherwise, they are written to file (see below).
#' @param .con If `.verbose = TRUE`, `.con` is character string that defines the full pathway to a `.txt` file (which can be created on-the-fly) into which messages are written to relay function progress. This approach, rather than printing to the console, is recommended for clarity, speed and debugging.
#'
#' @details
#' # Background
#'
#' For a full description of the AC* algorithms, see the resources linked in 'Source' below.
#'
#' # Tips
#' * **Datasets**. It is good practice to ensure that the datasets that underpin the AC* algorithms (e.g., acoustic data, receiver servicing dates, archival data, receiver locations), as used in `acs_setup_*` functions are aligned (i.e., defined for the same individual, the same receiver(s) and the same time frame). Use a consistent bathymetry grid in all functions. We have tested functions using a bathymetry grid with a Universal Transverse Mercator Projection. We believe other projections should work, providing the units of variables are comparable, but this is untested.
#' * **False detections.**. The usual data processing considerations for passive acoustic telemetry data apply with the AC* algorithms. Of particular importance is to check for false detections and to check whether, given the assumed detection range(s) (see [`acs_setup_detection_containers()`]) and mobility parameter (see [`acs_setup_obs()`]), the individual could have moved have between sequential receivers within the available time. It is worth checking this before implementing [`acs()`] because this check only requires you to consider sequential detections (and should take seconds), while [`acs()`] may consider may intermediate time steps (depending on the implementation) and generally requires longer computation times. See the following functions as a starting point:
#'    * [`false_detections`](https://rdrr.io/github/ocean-tracking-network/glatos/man/false_detections.html) in the [`glatos`](https://github.com/ocean-tracking-network/glatos) package;
#'    * [`process_false_detections_sf`](https://edwardlavender.github.io/flapper/reference/process_false_detections_sf.html) in the [`flapper`](https://github.com/edwardlavender/flapper) package;
#'    * [`get_detection_overlaps`](https://edwardlavender.github.io/flapper/reference/get_detection_overlaps.html) in the [`flapper`](https://github.com/edwardlavender/flapper) package;
#' * **Mobility**. Before you implement the AC* algorithms, check your parameterisation of `.mobility` (see [`acs_setup_obs()`]) aligns with the data. See the following functions as a starting point:
#'    * [`get_mvt_mobility_from_acoustics`](https://edwardlavender.github.io/flapper/reference/get_mvt_mobility.html) in the [`flapper`](https://github.com/edwardlavender/flapper) package;
#'    * [`get_mvt_mobility_from_archival`](https://edwardlavender.github.io/flapper/reference/get_mvt_mobility.html) in the [`flapper`](https://github.com/edwardlavender/flapper) package;
#' * **Depth**. For implementations of the ACDC algorithm and its derivatives via `.update_ac`, it is often preferable to check that the assumed models always identify at least some locations in which the individual could have been located at each time step. For example, in the ACDC algorithm, if your depth-error model is too restrictive, there may be no possible locations in which the individual can be located at a given time step. It is quicker to identify this and resolve it before implementing an AC* algorithm with an inappropriate depth-error model.
#'
#' # Feature requests
#' Please submit a [feature request](https://github.com/edwardlavender/patter/issues) if you would like to see new features added to this function (and associated routines).
#' * **Time-specific detection kernels.** This function does not currently support temporally varying detection kernels (although receiver-specific detection kernels are fine).
#' * **Parallelisation.** This function does not currently implement parallelisation (unlike predecessor functions in the [`flapper`](https://github.com/edwardlavender/flapper) package). However, the new approach is simpler and faster.
#'
#' @return The function returns an AC-branch ([`acb-class`]) object.
#'
#' @example man/examples/acs-examples.R
#'
#' @seealso
#' AC-branch algorithms in [`patter`] include the AC* algorithm (`[acs()`]) and the DC algorithm ([`dc()`]).
#'
#' This function is part of a series of functions designed to implement the AC* algorithms. See:
#' 1. [`acs_setup_obs()`] to set up observations;
#' 2. [`acs_setup_detection_containers()`] and [`acs_setup_detection_overlaps()`] to identify receiver overlaps (used in detection probability calculations);
#' 3. [`acs_setup_detection_kernels()`] to define detection probability kernels;
#' 4. [`acs()`] to implement the AC algorithm;
#'
#' AC-branch algorithms (i.e., [`acs()`], [`dc()`]) are typically followed by particle filtering to reconstruct movement paths and refine maps of space use (see [`pf_forward()`] and associated `pf_*()` functions).
#'
#' @source This function evolved from the `.acs()`, `.acs_pl()`, [`ac`](https://edwardlavender.github.io/flapper/reference/ac.html) and [`acdc`](https://edwardlavender.github.io/flapper/reference/acdc.html) functions in the [`flapper`](https://github.com/edwardlavender/flapper) package. Key developments include:
#' * Implementation of the algorithm over a single timeline;
#' * Re-parameterisation of container dynamics at each time step with respect to the possible locations of the individual given the data, given the past and given the future;
#' * Exploitation of [`data.table`] and [`terra`] for substantially improved speed;
#' * The use of [`terra::buffer()`] to represent container dynamics, which is faster and removes the polygon versus grid discrepancy in [`flapper`](https://github.com/edwardlavender/flapper);
#' * Implementation of the `.ac_update` argument to implement the ACDC algorithm (and related approaches) with enhanced flexibility;
#'
#' @author Edward Lavender
#' @export

acs <- function(.obs,
                .bathy,
                .detection_overlaps = NULL,
                .detection_kernels,
                .update_ac = NULL, ...,
                .save_cumulative = FALSE,
                .save_record = FALSE,
                .write_record = NULL,
                .progress = TRUE, .prompt = FALSE,
                .verbose = TRUE, .con = "") {

  #### Check user inputs
  t_onset <- Sys.time()
  .obs <- .acs_check_obs(.obs)
  .acs_check_bathy(.bathy)
  .acs_check_detection_overlaps(.detection_overlaps)
  .acs_check_detection_kernels(.detection_kernels, .bathy)
  if (!.save_cumulative && !.save_record && is.null(.write_record)) {
    abort("`.save_cumulative = FALSE`, `.save_record = FALSE` and `.write_record = NULL`. There is nothing to do.")
  }
  write_record_folder <- .acs_check_write_record(.write_record)
  if (!.verbose & .con != "") {
    warn("Input to `.con` ignored since `.verbose = FALSE`.")
  }
  # Catch dots
  check_dots_for_missing_period(formals(), list(...))

  #### Set up messages
  # Define log file & function to send messages to the console/file
  if (.verbose && .con != "") {
    create_log(.con)
  }
  append_messages <- ifelse(.con == "", FALSE, TRUE)
  cat_to_cf <- function(..., message = .verbose, file = .con, append = append_messages) {
    if (message) cat(paste(..., "\n"), file = .con, append = append)
  }
  cat_to_cf(paste0("patter::acs() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::acs() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Set up loop
  # Define empty list for outputs
  record     <- list()
  cumulative <- NULL
  # Define past = NULL for the first time step
  # * This is solely to stop an RStudio warning flag (past is defined before first use)
  past   <- NULL
  # Define the final detection
  detection_id_final <- max(.obs$detection_id)
  # Define progress bar
  if (.progress) {
    pb <- progress::progress_bar$new(total = max(.obs$timestep))
    pb$tick(0)
  }

  #### Implement loop
  for (t in .obs$timestep) {
    if (.progress) pb$tick()
    cat_to_cf(paste0("... Time step ", t, ":"))

    #### Define location given data (detection/non detection)
    if (.obs$detection[t] == 1) {
      # (1) Define location _given_detection_ at current time step
      cat_to_cf(paste0("... ... Detection ", .obs$detection_id[t], ":"))
      cat_to_cf(paste0("... ... * Defining location given data..."))
      if (t == 1) {
        # * This is only necessary at the very first time step
        # * At subsequent acoustic time steps, given_data is defined by what was
        # * ... the kernel around the next receiver(s) (see below)
        # Identify receiver(s) that recorded detections at the selected time step
        detections_current <- .obs$receiver_id[t][[1]]
        # Identify remaining (active) receivers which did not record a detection (if any)
        absences_current <- .acs_absences(.obs$date[t], detections_current, .detection_overlaps)
        given_data       <- .acs_given_detection(detections_current, absences_current, .detection_kernels)
      }
    } else {
      # (2) Define location _given non-detection_ at current time step
      given_data <- .detection_kernels$bkg_inv_surface_by_design[[.detection_kernels$array_design_by_date[[.obs$date[t]]]]]
    }
    # Filter by depth (and/or other constraints)
    if (!is.null(.update_ac)) {
      given_data <- .update_ac(given_data, .bathy, .obs, t, ...)
    }

    #### Define location given past
    if (t > 1) {
      cat_to_cf(paste0("... ... * Defining location given past..."))
      given_past <- terra::buffer(terra::classify(past, cbind(0, NA)), .obs$buffer_past[t])
    } else {
      given_past <- NULL
    }

    #### Define location given future
    if (.obs$detection_id[t] < detection_id_final) {
      cat_to_cf(paste0("... ... * Defining location given future..."))
      if (.obs$detection[t] == 1) {
        # (1) Define the location at the next detection
        # * We only need to do this once per acoustic time step & if
        # ... a) the receivers that recorded the detection or
        # ... b) the set of active (& overlapping) receivers changes
        # * Otherwise, we automatically reuse next_kernel from the previous
        # ... acoustic time step
        cat_to_cf(paste0("... ... - Defining future kernel..."))
        t_next <- .obs$timestep[.obs$detection_id == .obs$detection_id[t] + 1][1]
        detections_next <- .obs$receiver_id[t_next][[1]]
        absences_next   <- .acs_absences(.obs$date[t_next], detections_next, .detection_overlaps)
        if (t == 1 | !isTRUE(all.equal(detections_current, detections_next)) | !isTRUE(all.equal(absences_current, absences_next))) {
          next_kernel <- .acs_given_detection(detections_next, absences_next, .detection_kernels, .zero_to_na = TRUE)
        }
      }
      # (2) Define given_future for current time step
      # * Note that `next_kernel` has already been correctly specified
      # * ... with zeros set to NA to ensure correct buffering
      cat_to_cf(paste0("... ... - Defining future buffer..."))
      given_future <- terra::buffer(next_kernel, .obs$buffer_future[t])
    } else {
      given_future <- NULL
    }

    #### Define present location (given data, past & future)
    # (A) Comments
    cat_to_cf(paste0("... ... * Defining present..."))
    # terra::plot(given_data)    # `given_data` is a probability surface (NA, >0)
    # terra::plot(given_past)    # `given_past` is just a buffer (0, 1)
    # terra::plot(given_future)  # `given_future` is just a buffer (0, 1)
    # (B) Define present
    if (t == 1) {
      present <- given_data * given_future
    } else if (!is.null(given_future)) {
      present <- given_data * given_past * given_future
    } else {
      present <- given_data * given_past
    }
    present <- terra::mask(present, .bathy)
    # Renormalise
    present <- normalise(present)
    .acs_check_present(present, t)

    if (.prompt) {
      terra::plot(present, main = glue::glue("t = {t}"))
      given_data |>
        terra::as.polygons() |>
        terra::lines(col = "red", lwd = 2)
      if (!is.null(given_past)) {
        given_past |>
          terra::as.polygons() |>
          terra::lines(col = "orange", lwd = 2)
      }
      if (!is.null(given_future)) {
        given_future |>
          terra::as.polygons() |>
          terra::lines(col = "green4", lwd = 2)
      }
      readline("Press [Enter] to continue or [Esc] to exit...")
    }

    #### Update record
    cat_to_cf(paste0("... ... * Updating record ..."))
    # Save 'present' in memory
    if (.save_record) {
      record[[t]] <- present
    }
    # Write 'present' to file
    if (!is.null(.write_record)) {
      .write_record$x <- present
      .write_record$filename <- file.path(write_record_folder, paste0(.obs$timestep[t], ".tif"))
      do.call(terra::writeRaster, .write_record)
    }
    # Update cumulative map
    # * Note that it is essential `present` is normalised here
    if (.save_cumulative) {
      if (t == 1) {
        cumulative <- present
      } else {
        cumulative <- sum(cumulative, present, na.rm = TRUE)
      }
    }

    #### Update objects
    cat_to_cf(paste0("... ... * Moving on..."))
    # The present becomes the past (at the next time step)
    past <- present
    # When we change acoustic time steps, the next kernel becomes the current one
    if (.obs$detection_id[t] < detection_id_final && .obs$detection_id[t] != .obs$detection_id[t + 1]) {
      given_data          <- next_kernel
      detections_current  <- detections_next
      absences_current    <- absences_next
    }

  }

  #### Renormalise cumulative map
  # (This is the same as dividing by the number of time steps)
  if (.save_cumulative) {
    cumulative <- normalise(cumulative)
  }

  #### Define output list
  t_end <- Sys.time()
  time <- list(start = t_onset,
               end = t_onset,
               duration = difftime(t_end, t_onset))
  out <- list(record = record,
              map = cumulative,
              time = time)

  #### Return outputs
  class(out) <- c(class(out), "acb")
  out

}
