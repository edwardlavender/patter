#' @title DC: a template DC model
#' @description This function is an example DC model, of the kind required by `.model` in [`dc()`].
#' @param .obs The `.obs` [`data.table`] from [`dc()`].
#' @param .t An `integer` that defines the time step (used to index `.obs`).
#' @param .bathy The `.bathy` [`SpatRaster`], posibly as a `PackedSpatRaster`, from [`dc()`].
#' @param .unwrap A logical variable that defines whether or not to unwrap `.bathy` (via [`terra::unwrap()`]). This is required if `.bathy` is passed to this function as a `PackedSpatRaster`, which is the case for implementations of [`dc()`] using socket clusters.
#' @param ... Additional arguments passed from [`dc()`] (unused here).
#'
#' @details
#' This function assigns equal probability to all locations between two depth limits (as defined by `depth_shallow` and `depth_deep` columns in `.obs`).
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

dc_setup_model <- function(.obs, .t, .bathy,
                           .unwrap = ifelse(inherits(.bathy, "SpatRaster"), FALSE, TRUE),
                           ...) {
  if (.unwrap) {
    .bathy <- terra::unwrap(.bathy)
  }
  map <- (.bathy >= .obs$depth_shallow[.t] & .bathy <= .obs$depth_deep[.t]) + 0
  normalise(map)
}

#' @title DC: the depth-contour algorithm
#' @description This function implements the depth-contour algorithm.
#' @param .obs A [`data.table`] that defines (sorted) depth time series for a selected individual. At a minimum, this must contain the following columns:
#' * `timestep`---an `integer` that defines the time step;
#' * Any columns required by `.model`;
#' @param .bathy A [`SpatRaster`] that defines the bathymetry (for `.model`).
#' @param .model,... A `function` that defines the possible locations of the individual on `.bathy` at a given time step. This must accept five inputs in the following order:
#' * `.obs`---the `.obs` [`data.table`];
#' * `.t`---an `integer` that defines the time step (used to index `.obs`);
#' * `.bathy`---the bathymetry [`SpatRaster`], possibly in wrapped form as a `PackedSpatRaster`;
#' * `.unwrap`---a logical variable that defines whether or not to 'unwrap' `.bathy`. This is defined internally based on `.cl`: if a socket cluster is supplied, `.bathy` passed as a `PackedSpatRaster` and `.model` must unwrap `.bathy` (via [`terra::unwrap()`]) before any calculations;
#' * `...`---additional arguments, if necessary;
#'
#' `.model` should return a [`SpatRaster`]. Cell values are assumed to represent probabilities.
#' @param .save_record A logical variable that defines whether or not to save the output of `.model` at each time step in memory.
#' @param .write_record A named list, passed to [`terra::writeRaster`], to save the `record` [`SpatRaster`]s to file at each time step. The `filename` argument should define the directory in which to write files (see [`acs()`]).
#' @param .cl,.varlist Parallelisation options (see [`cl_lapply()`]).
#' @param .progress,.verbose,.txt Options to monitor function progress (see [`acs()`])
#' @return The function returns an AC-branch ([`acb-class`]) object.
#' @example man/examples/dc-examples.R
#' @author Edward Lavender
#' @export

dc <- function(.obs, .bathy, .model, ...,
               .save_record = FALSE, .write_record = NULL,
               .cl = NULL, .varlist = NULL,
               .progress = TRUE, .verbose = TRUE, .txt = "") {

  #### Check user inputs
  t_onset <- Sys.time()
  .dc_check_obs <- .pf_check_obs
  .obs <- .dc_check_obs(.obs)
  if (!.save_record && is.null(.write_record)) {
    abort("`.save_record = FALSE` and `.write_record = NULL`. There is nothing to do.")
  }
  write_record_folder <- .acs_check_write_record(.write_record)
  check_dots_for_missing_period(formals(), list(...))

  #### Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::dc() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::dc() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Set up loop
  # Define empty list for outputs
  record     <- list()
  cumulative <- NULL
  # Wrap .bathy for cluster
  unwrap_bathy <- FALSE
  if (inherits(.cl, "cluster")) {
    .bathy <- terra::wrap(.bathy)
    unwrap_bathy <- TRUE
  }
  # Implement progress bar
  if (!.progress) {
    pbo <- pbapply::pboptions(type = "none")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }

  #### Implement loop
  record <- cl_lapply(.obs$timestep, .cl = .cl, .varlist = .varlist, .fun = function(t) {
    cat_to_cf(paste0("... Time step ", t, ":"))

    #### Implement depth model
    cat_to_cf(paste0("... ... Implementing DC model..."))
    present <- .model(.obs, t, .bathy, .unwrap = unwrap_bathy, ...)
    cat_to_cf(paste0("... ... Confirming validity..."))
    .acs_check_present(present, t, .type = "dc")

    #### Update record
    cat_to_cf(paste0("... ... Updating record ..."))
    if (!is.null(.write_record)) {
      .write_record$x <- present
      .write_record$filename <- file.path(write_record_folder, paste0(.obs$timestep[t], ".tif"))
      do.call(terra::writeRaster, .write_record)
    }
    if (.save_record) {
      if (is.null(.cl)) {
        return(present)
      } else {
        return(terra::wrap(present))
      }
    } else {
      return(NULL)
    }
  })

  #### Unwrap SpatRasters
  if (.save_record && !is.null(.cl)) {
    cat_to_cf(paste0("... ... Unwrapping record ..."))
    record <- lapply(record, terra::unwrap)
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
