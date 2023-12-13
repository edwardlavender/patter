#' @title Set up data for [`patter`]
#' @description This function is used to set up movement datasets, spatial datasets and associated parameters for all [`patter`] functions. In general, this function should be called before other [`patter`] functions.
#'
#' @param .acoustics,.services,.archival,.moorings (optional) Movement datasets.
#' * `.acoustics` contains the detection time series (see [`dat_acoustics`] for an example);
#' * `.moorings` contains the receiver deployments (see [`dat_moorings`]) for an example);
#' * `.services` contains receiver servicing information (times during the deployment period of a receiver when it was not active due to servicing);
#' * `.archival` contains archival (depth) time series (see [`dat_archival`] for an example);
#' @param .bathy (optional) A [`SpatRaster`] that defines a (bathymetry) grid over the study area.
#' @param .lonlat (optional) A `logical` variable that defines whether spatial data is in longitude/latitude format.
#'
#' @details
#' The preparation of datasets for [`patter`] is a one-off inconvenience. See the [`check_data`] function documentation for full details as to the required properties of each input dataset. All requirements are kept to a minimum and straightforward to address. To minimise inconvenience, all inputs are optional in [`pf_setup_data()`]. If you are only interested in  specific routine(s), check the associated documentation to see which inputs are essential for those routine(s). The output of [`pat_setup_data()`] is widely passed to downstream [`patter`] functions, which streamlines the API, documentation and internal code.
#'
#' @return The function returns a named `list` with three elements:
#' * `data`---a named list, with one element for each movement dataset;
#' * `spatial`---a named list, with one element for each spatial dataset;
#' * `pars`---a named list, with required parameters;
#'
#' @examples
#' data <- pat_setup_data(.acoustics = dat_acoustics,
#'                       .services = NULL,
#'                       .archival = dat_archival,
#'                       .moorings = dat_moorings,
#'                       .bathy = dat_gebco(),
#'                       .lonlat = FALSE)
#' summary(data)
#'
#' @author Edward Lavender
#' @export

pat_setup_data <- function(.acoustics = NULL,
                           .moorings = NULL,
                           .services = NULL,
                           .archival = NULL,
                           .bathy = NULL,
                           .lonlat = NULL) {
  # Define blank data
  data <- list(acoustics = NULL,
               moorings = NULL,
               services = NULL,
               archival = NULL)
  spatial <- list(bathy = NULL)
  pars <- list(lonlat = NULL,
               spatna = NULL)
  # Check movement datasets
  data$acoustics <- check_acoustics(.acoustics = .acoustics)
  data$moorings  <- check_moorings(.moorings = .moorings, .lonlat = .lonlat, .bathy = .bathy)
  data$services  <- check_services(.services = .services, .moorings = .moorings)
  data$archival  <- check_archival(.archival)
  # Check spatial datasets
  spatial$bathy <- check_bathy(.bathy)
  # Define parameters
  pars$lonlat <- .lonlat
  pars$spatna <- spatContainsNA(.bathy)
  # Return list
  list(data = data,
       spatial = spatial,
       pars = pars)
}
