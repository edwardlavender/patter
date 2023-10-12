#' @title Datasets: the MEFS project
#'
#' @description These are sample datasets collated by the Movement Ecology of Flapper Skate (MEFS) project (Lavender, 2022). The flapper skate (*Dipturus intermedius*) is a Critically Endangered benthic elasmobranch. As part of the MEFS project, flapper skate were tagged with acoustic transmitters and archival (data storage) tags off the west coast of Scotland in 2016--17. Acoustic transmissions were recorded at an array of passive acoustic telemetry receivers. Concurrent depth time series were recorded by archival tags and recovered from recaptured individuals. For full details, see the references below. The datasets are defined within [`patter`] to illustrate package functions using real-world datasets.
#'
#' @details
#'
#' # Moorings
#'
#' [`dat_moorings`] defines passive acoustic telemetry receiver locations and associated information. This includes the following columns:
#'   * `receiver_id`---an `integer` that distinguishes each unique receiver deployment.
#'   * `receiver_start`---a `Date` that defines the start date of each receiver's deployment.
#'   * `receiver_end`---a `Date` that defines the end date of each receiver's deployment.
#'   * `receiver_easting`---a number that defines the receiver easting (UTM 29N).
#'   * `receiver_northing`---a number that defines the receiver northing (UTM 29N).
#'   * `receiver_lon`---a number that defines the longitude (decimal degrees) of each receiver.
#'   * `receiver_lat`---a number that defines the latitude (decimal degrees) of each receiver.
#'   * `receiver_range`---a number that defines the detection range, which was taken as a constant (750 m) across all receivers.
#'
#' Data are arranged by `receiver_id`.
#'
#' # Acoustics
#'
#' [`dat_acoustics`] contains sample detection time series. This includes the following columns:
#'   * `individual_id`---a unique identifier of the individual that was detected.
#'   * `timestamp`---a `POSIXct` object that defines the time of each observation.
#'   * `receiver_id`---a unique identifier of the receiver at which the individual was detected (see [`dat_moorings`]).
#'
#' Data are arranged by `individual_id`, `timestamp` and then `receiver_id`.
#'
#' # Archival
#'
#' [`dat_archival`] contains sample depth time series. Observations were sampled every 2 minutes. The data includes the following columns:
#'   * `individual_id`---a number that defines each individual (as in [`dat_acoustics`]).
#'   * `timestamp`---a `POSIXct` object that defines the time of each observation.
#'   * `depth`---a number that defines the depth (m) of the individual at each time step.
#'
#' Data are arranged by `individual_id` and then `timestamp`.
#'
#' # Bathymetry
#'
#' [`dat_gebco()`] returns a bathymetry (m) dataset for the west coast of Scotland where MEFS data were collected. This dataset is a [`SpatRaster`] with 264, rows, 190 columns and 1 layer, and the following properties:
#'   * `dimensions`---264, 190, 1 (nrow, ncol, nlyr)
#'   * `resolution`---100, 100  (x, y)
#'   * `extent`---695492.1, 714492.1, 6246657, 6273057  (xmin, xmax, ymin, ymax)
#'   * `coord. ref.`---WGS 84 / UTM zone 29N (EPSG:32629)
#'
#' @source Biologging and biotelemetry data were collected by, and belong to, Marine Scotland Science and NatureScot. Data were processed by Lavender (2022). If you wish to use these data, please contact Marine Scotland Science and NatureScot for further information.
#'
#' Bathymetry data were sourced from GEBCO Compilation Group (2019) GEBCO 2019 Grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e)
#'
#' @references Data collection and processing are described in Lavender (2022). Modelling the movements of flapper skate (*Dipturus intermedius*) in relation to a Scottish Marine Protected Area. University of St Andrews. \url{https://www.doi.org/10.17630/sta/201}
#'
#' For further information on the MEFS project, see:
#'
#' * Lavender, E. et al. (2021). Movement patterns of a Critically Endangered elasmobranch (*Dipturus intermedius*) in a Marine Protected Area. Aquat. Conserv. 32: 348–65. \url{https://www.doi.org/10.1002/aqc.3753}
#' * Lavender, E. et al. (2021). Individual variation and environmental cycles in the vertical movements of a benthic elasmobranch. Mar. Biol. 168: 164. \url{https://www.doi.org/10.1007/s00227-021-03973-1}
#' * Lavender, E. et al. (2022). Behavioural responses of a large, benthic elasmobranch to catch-and-release angling. Front. Mar. Sci. 9: 864344. \url{https://www.doi.org/10.3389/fmars.2022.864344}
#' * Thorburn, J. et al. (2021). Seasonal and ontogenetic variation in depth use by a Critically Endangered benthic elasmobranch and its implications for spatial management. Front. Mar. Sci. 8: 829.\url{https://www.doi.org/10.3389/fmars.2021.656368}
#' * Lavender, E. et al. (2022). Benthic animal-borne sensors and citizen science combine to validate ocean modelling. Sci. Rep. 12: 16613. \url{https://www.doi.org/1038/s41598-022-20254-z}
#' * Lavender, E. et al. (2023). An integrative modelling framework for passive acoustic telemetry. Meth. Ecol. Evol: 14, 2626–2638. \url{https://doi.org/10.1111/2041-210X.14193}
#'
#' @name datasets-mefs
NULL

#' @rdname datasets-mefs
"dat_moorings"

#' @rdname datasets-mefs
"dat_acoustics"

#' @rdname datasets-mefs
"dat_archival"

#' @rdname datasets-mefs
#' @export

dat_gebco <- function() {
  terra::rast(system.file("extdata", "dat_gebco.tif", package = "patter", mustWork = TRUE))
}
