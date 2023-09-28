#' @title DC: a template DC model
#' @description This function is an example DC model, of the kind required by `.model` in [`dc()`].
#' @param .obs The `.obs` [`data.table`] from [`dc()`].
#' @param .t An `integer` that defines the time step (used to index `.obs`).
#' @param .bathy The `.bathy` [`SpatRaster`] from [`dc()`].
#' @param ... Additional arguments passed from [`dc()`] (unused here).
#'
#' @details
#' This model assumes the individual can be located anywhere between a shallow and deep depth limit (as defined by `depth_shallow` and `depth_deep` columns in `.obs`) with equal probability.
#'
#' # Warning
#' * This function is used to streamline examples and does not represent a generically suitable model.
#' * The function does not check user inputs.
#'
#' @return The function returns a normalised [`SpatRaster`].
#' @author Edward lavender
#' @examples
#' # For examples, see `?dc()`
#'
#' @export

dc_setup_model <- function(.obs, .t, .bathy, ...) {
  map <- (.bathy >= .obs$depth_shallow[.t] & .bathy <= .obs$depth_deep[.t]) + 0
  normalise(map)
}

#' @title DC: the depth-contour algorithm
#' @description This function implements the depth contour algorithm.
#' @param .obs A [`data.table`] that defines (sorted) depth time series for a selected individual. At a minimum, this must contain the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * Any columns required by `.model`
#' @param .bathy A [`SpatRaster`] that defines the bathymetry.
#' @param .model,... A `function` that defines the possible locations of the individual on `.bathy` at a given time step. This must accept three inputs in the following order:
#' * `.obs`---the `.obs` [`data.table`];
#' * `.t`---an `integer` that defines the time step (used to index `.obs`);
#' * `.bathy`---the bathymetry [`SpatRaster`];
#' * `...`---additional arguments, if necessary;
#'
#' `model` should return a [`SpatRaster`]. Cell values are assumed to represent probabilities.
#' @param .save_record A logical variable that defines whether or not to save the output of `.model` at each time step in memory.
#' @param .write_record A named list, passed to [`terra::writeRaster`], to save the `record` [`SpatRaster`]s to file at each time step. The `filename` argument should define the directory in which to write files (see [`acs()`]).
#' @param .cl,.varlist Parallelisation options (see [`cl_lapply()`]).
#' @param .progress,.verbose,.con Options to monitor function progress (see [`acs()`])
#' @return The function returns an [`acs-class`] object.
#' @author Edward Lavender
#' @export

dc <- function(.obs, .bathy, .model, ...,
               .save_record = FALSE, .write_record = NULL,
               .cl = NULL, .varlist = NULL,
               .progress = TRUE, .verbose = FALSE, .con = "") {

  #### Check user inputs
  t_onset <- Sys.time()
  .dc_check_obs <- .pf_check_obs
  .obs <- .dc_check_obs(.obs)
  if (!.save_record && is.null(.write_record)) {
    abort("`.save_record = FALSE` and `.write_record = NULL`. There is nothing to do.")
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
  cat_to_cf(paste0("patter::dc() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::dc() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Set up loop
  # Define empty list for outputs
  record     <- list()
  cumulative <- NULL

  #### Implement loop
  if (!.progress) {
    pbo <- pbapply::pboptions(type = "none")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }
  record <- cl_lapply(.obs$timestep, .cl = .cl, .varlist = .varlist, fun = function(t) {
    cat_to_cf(paste0("... Time step ", t, ":"))

    #### Implement depth model
    present <- .model(.obs, t, .bathy)
    .acs_check_present(present, t, .type = "dc")

    #### Update record
    cat_to_cf(paste0("... ... * Updating record ..."))
    # Write 'present' to file
    if (!is.null(.write_record)) {
      .write_record$x <- present
      .write_record$filename <- file.path(write_record_folder, paste0(.obs$timestep[t], ".tif"))
      do.call(terra::writeRaster, .write_record)
    }
    if (.save_record) {
      return(present)
    } else {
      return(NULL)
    }
  })

  #### Define output list
  t_end <- Sys.time()
  time <- list(start = t_onset,
               end = t_onset,
               duration = difftime(t_end, t_onset))
  out <- list(record = record,
              map = cumulative,
              time = time)

  #### Return outputs
  class(out) <- c(class(out), "acs")
  out

}
