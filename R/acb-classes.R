#' @title AC-branch algorithms: [`acb-class`] objects
#' @description An [`acb-class`] object is a S3 class that defines the named `list` returned by a AC-branch algorithm (i.e., [`acs()`] or [`dc()`]).
#'
#' @details [`acb-class`] objects contain the following named elements:
#'
#' * `record`---`NULL` or a `list` of [`SpatRaster`]s, one for each time step, that define the set of possible locations of the individual at each time step (and their associated probability densities), according to the AC-branch algorithm. If the AC-branch function ([`acs()`] or [`dc()`]) is implemented with `.save_history = FALSE`, this element is `NULL`.
#'
#' * `map`---`NULL` or a [`SpatRaster`] representing the cumulative map of space use. This is the sum of the [`SpatRaster`]s from each time step, normalised by the number of time steps. For [`acs()`] with `.save_cumulative = FALSE` and for [`dc()`] this element is `NULL`.
#'
#' * `time`---a named `list` that records algorithm timings, including:
#'      * `start`---the start time;
#'      * `end`---the end time;
#'      * `duration`---the difference between the start and end time (i.e., wall time), from [`difftime`];
#'
#' To write/read an [`acb-class`] object from file, the `record` [`SpatRaster`]s need to be wrapped/unwrapped (via [`terra::wrap()`] and [`terra::unwrap()`]). Helper functions may be added to do this in future, if requested.
#'
#' @seealso
#' AC-branch algorithms in [`patter`] include the AC* algorithm ([`acs()`]) and the DC algorithm ([`dc()`]).
#'
#' To implement the AC* algorithm, use:
#'   1. [`acs_setup_obs()`] to set up observations;
#'   2. [`acs_setup_detection_overlaps()`] to identify receiver overlaps (used in detection probability calculations);
#'   3. [`acs_setup_detection_kernels()`] and [`acs_setup_detection_pr()`] to define detection probability kernels;
#'   4. [`acs()`] to implement the AC algorithm;
#'
#' To implement the DC algorithm, use:
#'   1. [`dc()`] to implement the algorithm;
#'
#' AC-branch algorithms return an [`acb-class`] object.
#'
#' AC-branch algorithms are typically followed by particle filtering to reconstruct movement paths and refine maps of space use (see `pf_*()` functions).
#'   1. To begin, see [`pf_forward_1()`];
#'   2. To implement AC- and PF-branch algorithms simultaneously, see [`pf_forward_2()`];
#'
#' @author Edward Lavender
#' @docType package
#' @name acb-class
NULL
