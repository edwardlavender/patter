#' @title Passive acoustic telemetry moorings
#' @description A dataset containing a sample of passive acoustic telemetry receiver locations and associated information. Data are arranged by `receiver_id` (see below).
#'
#' @format A dataframe with 40 observations and 8 variables:
#' \describe{
#'   * `receiver_id`---an integer that distinguishes each unique receiver deployment.
#'   * `receiver_start`---a date that defines the start date of each receiver's deployment.
#'   * `receiver_end`---a date that defines the end date of each receiver's deployment.
#'   * `receiver_lon`---a number that defines the longitude (decimal degrees) of each receiver.
#'   * `receiver_lat`---a number that defines the latitude (decimal degrees) of each receiver.
#'   * `reciver_easting`---a number that defines the receiver easting (UTM 29N).
#'   * `receiver_northing`---a number that defines the receiver northing (UTM 29N).
#'   * `receiver_depth`---a number that defines the approximate depth of each receiver below the surface (m).
#' }
#'
#' @source Data were collected by, and belong to, Marine Scotland Science and NatureScot. Data were processed by Edward Lavender. If you wish to use these data, please contact Marine Scotland Science and NatureScot for further information.
#'
#' @references Data collection and processing are described in Lavender (2022). Modelling the movements of flapper skate (*Dipturus intermedius*) in relation to a Scottish Marine Protected Area. University of St Andrews. https://www.doi.org/10.17630/sta/201
"dat_moorings"

#' @title Passive acoustic telemetry detections
#' @description A dataset containing a sample of processed flapper skate (*Dipturus intermedius*) detection time series. Data are arranged by `individual_id`, `timestamp` and then `receiver_id` (see below).
#'
#' @format A dataframe with 39,242 observations and 9 variables:
#' \describe{
#'   * `individual_id`---a unique identifier of the individual that was detected.
#'   * `timestamp`---a `POSIXct` object that defines the time of each observation.}
#'   * `receiver_id`---a unique identifier of the receiver at which the individual was detected (see [`dat_moorings`]).
#' }
#'
#' @source Data were collected by, and belong to, Marine Scotland Science and NatureScot. Data were processed by Edward Lavender. If you wish to use these data, please contact Marine Scotland Science and NatureScot for further information.
#'
#' @references Data collection and processing are described in Lavender (2022). Modelling the movements of flapper skate (*Dipturus intermedius*) in relation to a Scottish Marine Protected Area. University of St Andrews. https://www.doi.org/10.17630/sta/201
"dat_acoustics"

#' @title Archival time series
#' @description A dataset containing a sample of flapper skate (*Dipturus intermedius*) depth (m) time series. Observations were sampled every 2 minutes using archival tags. Data are arranged by `individual_id` and then `timestamp`.
#'
#' @format A dataframe with 75,000 observations and 5 variables:
#' \describe{
#'   * `individual_id`---a number that defines each individual.
#'   * `timestamp`---a `POSIXct` object that defines the time of each observation.
#'   * `depth`---a number that defines the depth (m) of the individual at each time step.
#' }
#'
#' @source Data were collected by, and belong to, Marine Scotland Science and NatureScot. Data were processed by Edward Lavender. If you wish to use these data, please contact Marine Scotland Science and NatureScot for further information.
#'
#' @references Data collection and processing are described in Lavender (2022). Modelling the movements of flapper skate (*Dipturus intermedius*) in relation to a Scottish Marine Protected Area. University of St Andrews. https://www.doi.org/10.17630/sta/201
"dat_archival"


