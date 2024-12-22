#' @title [`patter`] set up: datasets
#' @description This function is used to set up and validate telemetry data for [`patter`] functions. Use it if you have acoustic and/or archival data.
#'
#' @param .map (optional) A [`SpatRaster`] that defines the study area (see [glossary]).
#' @param .detections,.services,.archival,.moorings (optional) [`data.table`]s of observations and and associated parameters.
#' * `.detections` contains a detection time series (see [`dat_detections`] for an example);
#' * `.moorings` contains receiver deployments (see [`dat_moorings`]) for an example);
#' * `.services` contains receiver servicing information (times during the deployment period of a receiver when it was not active due to servicing);
#' * `.archival` contains archival (depth) time series (see [`dat_archival`] for an example);
#'
#' @details
#' The preparation of datasets for [`patter`] is a one-off inconvenience. You should be able to analyse any kind of electronic tagging and tracking data using the main [`patter`] functions (such as [`pf_filter()`]). For passive acoustic telemetry data and archival (depth) data, [`patter`] provides some additional helper routines and functionality (such as data assembly routines for the particle filter). If you have acoustic and/or archival data, use [`pat_setup_data()`] to verify that your datasets meet [`patter`] requirements and exploit this additional functionality. See the [`check_dlist`] documentation for the required properties of each input dataset. All requirements are kept to a minimum and are straightforward to address. To minimise inconvenience, all inputs are optional in [`pat_setup_data()`]. For other data types, see the documentation for [`assemble`]`_*()` functions to incorporate them in particle filtering algorithms. Downstream functions may assume that input data are correctly formatted, which streamlines the API, documentation and internal code.
#'
#' On Linux, this function cannot be used within a `Julia` session.
#'
#' @return The function returns a named `list` with one element for each input argument.
#'
#' @example man/examples/example-pat_setup_data.R
#' @author Edward Lavender
#' @export

pat_setup_data <- function(.map = NULL,
                           .detections = NULL,
                           .moorings = NULL,
                           .services = NULL,
                           .archival = NULL) {
  dlist <- list(map = NULL,
                detections = NULL,
                moorings = NULL,
                services = NULL,
                archival = NULL)
  dlist$map <- check_map(.map)
  dlist$detections <- check_detections(.detections = .detections, .moorings = .moorings)
  dlist$moorings  <- check_moorings(.moorings = .moorings,
                                    .detections = dlist$detections,
                                    .map = .map)
  dlist$services  <- check_services(.services = .services, .moorings = .moorings)
  dlist$archival  <- check_archival(.archival)
  dlist
}
