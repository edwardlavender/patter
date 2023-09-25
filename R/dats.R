#' @title Dataset: passive acoustic telemetry moorings
#' @description A dataset with receiver locations and associated information. Data are arranged by `receiver_id` (see below).
#'
#' @format A [`data.table`] with 40 observations and 7 variables:
#'   * `receiver_id`---an integer that distinguishes each unique receiver deployment.
#'   * `receiver_start`---a date that defines the start date of each receiver's deployment.
#'   * `receiver_end`---a date that defines the end date of each receiver's deployment.
#'   * `receiver_lon`---a number that defines the longitude (decimal degrees) of each receiver.
#'   * `receiver_lat`---a number that defines the latitude (decimal degrees) of each receiver.
#'   * `receiver_easting`---a number that defines the receiver easting (UTM 29N).
#'   * `receiver_northing`---a number that defines the receiver northing (UTM 29N).
#'   * `receiver_depth`---a number that defines the approximate depth of each receiver below the surface (m).
#'
#' @source Data were collected by, and belong to, Marine Scotland Science and NatureScot. Data were processed by Edward Lavender. If you wish to use these data, please contact Marine Scotland Science and NatureScot for further information.
#'
#' @references Data collection and processing are described in Lavender (2022). Modelling the movements of flapper skate (*Dipturus intermedius*) in relation to a Scottish Marine Protected Area. University of St Andrews. https://www.doi.org/10.17630/sta/201
"dat_moorings"


#' @title Dataset: passive acoustic telemetry detections
#' @description A dataset containing a sample of processed flapper skate (*Dipturus intermedius*) detection time series. Data are arranged by `individual_id`, `timestamp` and then `receiver_id` (see below).
#'
#' @format A [`data.table`] with 39,242 observations and 3 variables:
#'   * `individual_id`---a unique identifier of the individual that was detected.
#'   * `timestamp`---a `POSIXct` object that defines the time of each observation.
#'   * `receiver_id`---a unique identifier of the receiver at which the individual was detected (see [`dat_moorings`]).
#'
#' @source Data were collected by, and belong to, Marine Scotland Science and NatureScot. Data were processed by Edward Lavender. If you wish to use these data, please contact Marine Scotland Science and NatureScot for further information.
#'
#' @references Data collection and processing are described in Lavender (2022). Modelling the movements of flapper skate (*Dipturus intermedius*) in relation to a Scottish Marine Protected Area. University of St Andrews. https://www.doi.org/10.17630/sta/201
"dat_acoustics"


#' @title Dataset: archival time series
#' @description A dataset containing a sample of flapper skate (*Dipturus intermedius*) depth (m) time series. Observations were sampled every 2 minutes using archival tags. Data are arranged by `individual_id` and then `timestamp`.
#'
#' @format A [`data.table`] with 75,000 observations and 3 variables:
#'   * `individual_id`---a number that defines each individual.
#'   * `timestamp`---a `POSIXct` object that defines the time of each observation.
#'   * `depth`---a number that defines the depth (m) of the individual at each time step.
#'
#' @source Data were collected by, and belong to, Marine Scotland Science and NatureScot. Data were processed by Edward Lavender. If you wish to use these data, please contact Marine Scotland Science and NatureScot for further information.
#'
#' @references Data collection and processing are described in Lavender (2022). Modelling the movements of flapper skate (*Dipturus intermedius*) in relation to a Scottish Marine Protected Area. University of St Andrews. https://www.doi.org/10.17630/sta/201
"dat_archival"


#' @title Dataset: a bathymetry dataset
#' @description This function loads a processed bathymetry (m) dataset for the west coast of Scotland.
#'
#' @return The function returns a [`SpatRaster`] with 264, rows, 190 columns and 1 layer, with the following properties:
#'   * `dimensions`---264, 190, 1 (nrow, ncol, nlyr)
#'   * `resolution`---100, 100  (x, y)
#'   * `extent`---695492.1, 714492.1, 6246657, 6273057  (xmin, xmax, ymin, ymax)
#'   * `coord. ref.`---WGS 84 / UTM zone 29N (EPSG:32629)
#'
#' @source Raw data were sourced from GEBCO Compilation Group (2019) GEBCO 2019 Grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e)
#' @export

dat_gebco <- function() {
  terra::rast(system.file("extdata", "dat_gebco.tif", package = "patter", mustWork = TRUE))
}



