#' @title AC* helper: internal checks
#' @description These are internal check functions.
#' @param .b The `.bathy` [`SpatRaster`].
#' @param .k The `.detection_kernels` `list`.
#' @param .o The `.obs` [`data.table`].
#' @param .ov The `detection_overlaps` `list`.
#' @param .w The `write_record` `list`.
#' @author Edward Lavender
#' @name .acs_check

#' @rdname .acs_check
#' @keywords internal

.acs_check_obs <- function(.o) {
  if (inherits(.o, "data.frame") & !inherits(.o, "data.table")) {
    .o <- as.data.table(.o)
  }
  check_inherits(.o, "data.table")
  check_names(.o, c("timestep", "timestamp", "date", "detection_id", "detection", "receiver_id", "buffer_past", "buffer_future"))
  check_inherits(.o$timestep, "integer")
  check_inherits(.o$timestamp, "POSIXct")
  check_inherits(.o$detection_id, "integer")
  check_inherits(.o$detection, "integer")
  check_inherits(.o$receiver_id, "list")
  check_inherits(.o$buffer_past, "numeric")
  check_inherits(.o$buffer_future, "numeric")
  if (!all(
    lubridate::year(.o$timestamp) == lubridate::year(.o$date),
    lubridate::month(.o$timestamp) == lubridate::month(.o$date),
    lubridate::day(.o$timestamp) == lubridate::day(.o$date)
  )) {
    abort("There is a discrepancy between `.obs$timestamp` and `.obs$date`.")
  }
  .o
}

#' @rdname .acs_check
#' @keywords internal

.acs_check_bathy <- function(.b) {
  check_inherits(.b, "SpatRaster")
}

#' @rdname .acs_check
#' @keywords internal

.acs_check_detection_overlaps <- function(.ov) {
  check_named_list(.ov)
  if (!is.null(.ov)) {
    check_names(.ov, c("list_by_receiver", "list_by_date"))
  }
}

#' @rdname .acs_check
#' @keywords internal

.acs_check_detection_kernels <- function(.k, .b) {
  check_named_list(.k)
  check_names(.k, c("receiver_specific_kernels",
                    "receiver_specific_inv_kernels",
                    "array_design",
                    "array_design_by_date",
                    "bkg_surface_by_design",
                    "bkg_inv_surface_by_design"))
  # Check bathy, detection_overlaps & detection_kernels
  if (!terra::compareGeom(.b, compact(.k$receiver_specific_kernels)[[1]])) {
    abort("The properties of the bathymetry grid and the detection kernel SpatRaster(s) are not equal.")
  }
}


#' @rdname .acs_check
#' @keywords internal

.acs_check_write_record <- function(.w) {
  if (!is.null(.w)) {
    check_named_list(.w)
    check_names(.w, "filename")
    if (length(.w$filename) != 1L) {
      abort("write_record$filename should be a single directory in which to save files.")
    }
    check_dir(.w$filename)
    if (length(list.files(.w$filename)) != 0L) {
      warn("`write_record$filename` ('{.w$filename}') is not an empty directory.",
           .envir = environment())
    }
  }
  .w$filename
}


#' @title AC* helper: Define active, overlapping receivers with absences
#' @description Given a detection at one or more receivers on a given date, this function defines the set of remaining, active, overlapping receivers that did not record detections.
#' @param .date A `character` that defines the date (e.g., `obs$date[t]` internally in [`.acs()`]).
#' @param .detections An `integer` vector that defines the receiver(s) that recorded detection(s) at the current time step (e.g., `recs_current` or `recs_next` internally in [`.acs()`]).
#' @param .overlaps A named `list` from [`acs_setup_detection_overlaps()`] that defines receiver overlaps (e.g., `detection_overlaps` internally in [`.acs()`]). `NULL` is permitted.
#' @details In the AC* algorithms, at the moment of detection, the probability kernels that describe the possible locations of an individual given the data depend on both the receivers that record detections and those that did not (eqn S5 in Lavender et al., 2023). This function is used to restrict the set of receivers to which eqn S5 needs to be applied.
#'
#' # Warning
#' For speed, this function performs no internal checks.
#'
#' @return The function returns an `integer` vector that defines the set of receivers that overlap with `.detections` but did not record detections. `NULL` indicates no overlapping receivers.
#' @seealso This function defines the `absences` argument for [`.acs_given_detection()`].
#' @author Edward Lavender
#' @keywords internal

.acs_absences <- function(.date, .detections, .overlaps){
  absences <- NULL
  if (!is.null(.overlaps)) {
    # Define overlapping receivers
    absences <-
      lapply(.detections, function(r) {
        ov <- .overlaps$list_by_receiver[[r]]
        ov <- ov[rownames(ov) == .date, 3:ncol(ov)]
        colnames(ov)[ov == 1]
      }) |>
      unlist() |>
      as.integer() |>
      unique()
    # Define the set of overlapping receivers that did not record detections
    absences <- absences[!(absences %in% .detections)]
    if (length(absences) == 0L) {
      absences <- NULL
    }
  }
  absences
}


#' @title AC* helper: Define the individual's location given detection(s)
#' @description This function defines an (unnormalised) probability surface that represents the possible locations of an individual given one or more detections.
#' @param .detections An `integer` vector of the receiver(s) that recorded detections at a given time step.
#' @param .absences An `integer` vector of the remaining, overlapping receiver(s) that did not record a detection, from [`.acs_absences()`].
#' @param .kernels A `list` from [`acs_setup_detection_kernels`].
#' @param .zero_to_na A logical variable that defines whether or not to classify zeros as `NA`s. This should be `FALSE` for defining `given_data` in [`.acs()`], so that this layer is correctly masked, but `TRUE` for defining `next_kernel` in [`.acs()`], so that `next_kernel` is correctly buffered.

#' @details In the AC* algorithms, at the moment of detection, the probability kernels that describe the possible locations of an individual given the data depend on both the receivers that record detections and those that did not (eqn S5 in Lavender et al., 2023). This function solves eqn S5. For computational efficiency, the equation is solved in a stepwise manor such that the number of necessary operations is kept to a minimum.
#'
#' # Warning
#' For speed, this function performs no internal checks.
#'
#' @author Edward Lavender
#' @keywords internal

.acs_given_detection <- function(.detections, .absences, .kernels, .zero_to_na = FALSE) {
  # Define kernel around receiver(s) with detection(s)
  if (length(.detections) == 1L) {
    k <- .kernels$receiver_specific_kernels[[.detections]]
  } else {
    k <- do.call(c, .kernels$receiver_specific_kernels[.detections])
    k <- terra::app(k, "prod")
  }
  # Define inverse kernels around receiver(s) without detection(s)
  if (!is.null(.absences)) {
    if (length(.absences) == 1L) {
      ka <- .kernels$receiver_specific_inv_kernels[[.absences]]
    } else {
      ka <- do.call(c, .kernels$receiver_specific_inv_kernels[.absences])
      ka <- terra::app(ka, "prod")
    }
    k <- k * ka
  }
  # Reclassify 0 to NA
  # * This is necessary for next_kernel in .acs() b/c
  # * ... terra::buffer() ignores NAs but not zeros
  if (.zero_to_na) {
    k <- terra::classify(k, cbind(0, NA))
  }
  k
}


#' @title AC* algorithm back-end
#' @description This function is the back-end of the acoustic-container and acoustic-container depth-contour algorithms.
#' @param obs A [`data.table`] with observations, from [`acs_setup_obs()`].
#' @param bathy A [`SpatRaster`] that defines the grid over which the algorithms are implemented.
#' @param detection_overlaps A named `list`, from [`acs_setup_detection_overlaps()`].
#' @param detection_kernels A `list`, from [`acs_setup_detection_kernels()`].
#' @param save_record,save_cumulative Logical inputs that control options for saving outputs in memory.
#' * `save_record` defines whether or not to save the record of the possible locations of the individual at each time step in the `record` element of the output;
#' * `save_cumulative` defines whether or not to save a cumulative (probability-of-use) map, derived from the normalised summation of each element in `record` in the `map` element of the output;
#' @param write_record A named list, passed to [`terra::writeRaster`], to save the `record` `SpatRaster`s to file at each time step. The `filename` argument should define the directory in which to write files. Files are named by time step (i.e., 1.tif, 2.tif, ..., N.tif).
#' @param progress A logical variable that defines whether or not to implement a progress bar (via [`progress::progress_bar()`]).
#' @param prompt A logical variable that defines whether or not a user prompt is required between time steps. If provided, the function plots the possible locations of the individual at each time step. This is useful for diagnostics.
#' @param verbose A logical variable that defines whether or not to print messages to the console or to file to relay function progress. If `con = ""`, messages are printed to the console; otherwise, they are written to file (see below).
#' @param con If `verbose = TRUE`, `con` is character string that defines the full pathway to a `.txt` file (which can be created on-the-fly) into which messages are written to relay function progress. This approach, rather than printing to the console, is recommended for clarity, speed and debugging.
#' @return The function returns an [`ac_record-class`] object.
#'
#' @source This function evolved from `.acs()` in the [flapper](https://github.com/edwardlavender/flapper) package. Key developments include:
#' * Implementation of the algorithm over a single timeline;
#' * Re-parameterisation of container dynamics at each time step with respect to the possible locations of the individual given the data, given the past and given the future;
#' * Exploitation of [`data.table`] and [`terra`] for substantially improved speed;
#' * The use of [`terra::buffer()`] to represent container dynamics, which is faster and removes the polygon versus grid discrepancy in [flapper](https://github.com/edwardlavender/flapper);
#'
#' @seealso For internal helpers, see [`.acs_absences()`] and [`.acs_given_detection()`].
#' @author Edward Lavender
#' @keywords internal

.acs <- function(obs,
                 bathy,
                 detection_overlaps = NULL,
                 detection_kernels,
                 save_cumulative = FALSE,
                 save_record = FALSE,
                 write_record = NULL,
                 progress = TRUE, prompt = FALSE,
                 verbose = TRUE, con = "") {

  #### Check user inputs
  t_onset <- Sys.time()
  obs <- .acs_check_obs(obs)
  .acs_check_bathy(bathy)
  .acs_check_detection_overlaps(detection_overlaps)
  .acs_check_detection_kernels(detection_kernels, bathy)
  if (!save_cumulative && !save_record && is.null(write_record)) {
    abort("`save_cumulative = FALSE`, `save_record = FALSE` and `write_record = NULL`. There is nothing to do.")
  }
  write_record_folder <- .acs_check_write_record(write_record)
  if (!verbose & con != "") {
    warn("Input to `con` ignores since `verbose = FALSE`.")
  }

  #### Set up messages
  # Define log file & function to send messages to the console/file
  if (verbose && con != "") {
    create_log(con)
  }
  append_messages <- ifelse(con == "", FALSE, TRUE)
  cat_to_cf <- function(..., message = verbose, file = con, append = append_messages) {
    if (message) cat(paste(..., "\n"), file = con, append = append)
  }
  cat_to_cf(paste0("patter::.acs() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::.acs() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Set up loop
  # Define empty list for outputs
  record     <- list()
  cumulative <- NULL
  # Define past = NULL for the first time step
  # * This is solely to stop an RStudio warning flag (past is defined before first use)
  past   <- NULL
  # Define the final detection
  detection_id_final <- max(obs$detection_id)
  # Define progress bar
  if (progress) {
    pb <- progress::progress_bar$new(total = max(obs$timestep))
    pb$tick(0)
  }

  #### Implement loop
  for (t in obs$timestep) {
    if (progress) pb$tick()
    cat_to_cf(paste0("... Time step ", t, ":"))

    #### Define location given data (detection/non detection)
    if (obs$detection[t] == 1) {
      # (1) Define location _given_detection_ at current time step
      cat_to_cf(paste0("... ... Detection ", obs$detection_id[t], ":"))
      cat_to_cf(paste0("... ... * Defining location given data..."))
      if (t == 1) {
        # * This is only necessary at the very first time step
        # * At subsequent acoustic time steps, given_data is defined by what was
        # * ... the kernel around the next receiver(s) (see below)

        # Identify receiver(s) that recorded detections at the selected time step
        detections_current <- obs$receiver_id[t][[1]]
        # Identify remaining (active) receivers which did not record a detection (if any)
        absences_current <- .acs_absences(obs$date[t], detections_current, detection_overlaps)
        given_data       <- .acs_given_detection(detections_current, absences_current, detection_kernels)
      }
    } else {
      # (2) Define location _given non-detection_ at current time step
      given_data <- detection_kernels$bkg_inv_surface_by_design[[detection_kernels$array_design_by_date[[obs$date[t]]]]]
    }
    # Filter given_data by depth
    # TO DO
    # * use calc_depth_error()

    #### Define location given past
    if (t > 1) {
      cat_to_cf(paste0("... ... * Defining location given past..."))
      given_past <- terra::buffer(terra::classify(past, cbind(0, NA)), obs$buffer_past[t])
    } else {
      given_past <- NULL
    }

    #### Define location given future
    if (obs$detection_id[t] < detection_id_final) {
      cat_to_cf(paste0("... ... * Defining location given future..."))
      if (obs$detection[t] == 1) {
        # (1) Define the location at the next detection
        # * We only need to do this once per acoustic time step & if
        # ... a) the receivers that recorded the detection or
        # ... b) the set of active (& overlapping) receivers changes
        # * Otherwise, we automatically reuse next_kernel from the previous
        # ... acoustic time step
        cat_to_cf(paste0("... ... - Defining future kernel..."))
        t_next <- obs$timestep[obs$detection_id == obs$detection_id[t] + 1][1]
        detections_next <- obs$receiver_id[t_next][[1]]
        absences_next   <- .acs_absences(obs$date[t_next], detections_next, detection_overlaps)
        if (t == 1 | !isTRUE(all.equal(detections_current, detections_next)) | !isTRUE(all.equal(absences_current, absences_next))) {
          next_kernel <- .acs_given_detection(detections_next, absences_next, detection_kernels, .zero_to_na = TRUE)
        }
      }
      # (2) Define given_future for current time step
      # * Note that `next_kernel` has already been correctly specified
      # * ... with zeros set to NA to ensure correct buffering
      cat_to_cf(paste0("... ... - Defining future buffer..."))
      given_future <- terra::buffer(next_kernel, obs$buffer_future[t])
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
    # * Take given_data as the baseline
    # * Set NAs to 0
    # * This is essential to ensure correct masking, since NAs are ignored
    present <- terra::classify(given_data, cbind(NA, 0))
    if (!is.null(given_past)) {
      present <- terra::mask(present, given_past, maskvalues = 0, updatevalue = 0)
    }
    if (!is.null(given_future)) {
      present <- terra::mask(present, given_future, maskvalues = 0, updatevalue = 0)
    }
    present <- terra::mask(present, bathy)
    # Renormalise
    present <- present / as.numeric(terra::global(present, "sum", na.rm = TRUE))
    if (prompt) {
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
    if (save_record) {
      record[[t]] <- present
    }
    # Write 'present' to file
    if (!is.null(write_record)) {
      write_record$x <- present
      write_record$filename <- file.path(write_record_folder, paste0(obs$timestep[t], ".tif"))
      do.call(terra::writeRaster, write_record)
    }
    # Update cumulative map
    #
    # TO DO
    # * Confirm with SB about adding normalised surfaces
    #
    if (save_cumulative) {
      if (t == 1) {
        cumulative <- present
      } else {
        cumulative <- cumulative + present
      }
    }

    #### Update objects
    cat_to_cf(paste0("... ... * Moving on..."))
    # The present becomes the past (at the next time step)
    past <- present
    # When we change acoustic time steps, the next kernel becomes the current one
    if (obs$detection_id[t] < detection_id_final && obs$detection_id[t] != obs$detection_id[t + 1]) {
      given_data          <- next_kernel
      detections_current  <- detections_next
      absences_current    <- absences_next
    }

  }

  #### Renormalise cumulative map
  if (save_cumulative) {
    cumulative <- cumulative / as.numeric(terra::global(cumulative, "sum", na.rm = TRUE))
  }

  #### Define output list
  t_end <- Sys.time()
  time <- list(start = t_onset,
               end = t_onset,
               duration = difftime(t_end, t_onset))
  out <- list(archive = list(record = record,
                             map = cumulative),
              time = time)

  #### Return outputs
  class(out) <- c(class(out), "ac_record")
  out

}
