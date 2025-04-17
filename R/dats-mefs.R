#' @title Datasets: the MEFS project
#'
#' @description These are sample datasets collated by the Movement Ecology of Flapper Skate (MEFS) project (Lavender, 2022). The flapper skate (*Dipturus intermedius*) is a Critically Endangered benthic elasmobranch. As part of the MEFS project, flapper skate were tagged with acoustic transmitters and archival (data storage) tags off the west coast of Scotland in 2016--17. Acoustic transmissions were recorded at an array of passive acoustic telemetry receivers. Concurrent depth time series were recorded by archival tags and recovered from recaptured individuals. For full details, see the references below. The datasets are defined within [`patter`] to illustrate package functions using real-world datasets.
#'
#' @param .return A `character` that defines the object returned by [`dat_gebco()`], [`dat_coast()`] or [`dat_mpa()`]:
#' * `SpatRaster` returns a [`SpatRaster`];
#' * `SpatVector` returns a [`SpatVector`];
#' * `character` returns a file path to the raster. This is the only supported option on Linux if `JULIA_SESSION = "TRUE"`;
#'
#' @details
#'
#' # Moorings
#'
#' [`dat_moorings`] defines passive acoustic telemetry receiver locations and associated information. This includes the following columns:
#'   * `receiver_id`---an `integer` vector that defines unique receiver deployments;
#'   * `receiver_start`, `receiver_end`---`Date` vectors that define receiver deployment start and end dates;
#'   * `receiver_x`, `receiver_y`---`numeric` vectors that define receiver coordinates (in UTM 29N);
#'   * `receiver_alpha`, `receiver_beta`, `receiver_gamma`---a `numeric` vectors that define detection probability parameters;
#'
#' Data are arranged by `receiver_id`.
#'
#' # Detections
#'
#' [`dat_detections`] contains sample detection time series. This includes the following columns:
#'   * `individual_id`---an `integer` vector that identifies individuals;
#'   * `timestamp`---a `POSIXct` vector that defines the time of each observation;
#'   * `receiver_id`---the receiver ID (see [`dat_moorings`]);
#'
#' Data are arranged by `individual_id`, `timestamp` and then `receiver_id`.
#'
#' # Archival
#'
#' [`dat_archival`] contains sample depth time series. Observations were sampled every 2 minutes. The data includes the following columns:
#'   * `individual_id`---an `integer` vector that identifies individuals (as in [`dat_detections`]);
#'   * `timestamp`---a `POSIXct` vector that defines the time of each observation;
#'   * `depth`---a `numeric` vector that defines the depth (m) of the individual at each time step;
#'
#' Data are arranged by `individual_id` and then `timestamp`.
#'
#' # Bathymetry
#'
#' [`dat_gebco()`] returns a bathymetry (m) dataset for the west coast of Scotland where MEFS data were collected. This dataset is a [`SpatRaster`] with the following properties:
#'   * `dimensions`---264, 190, 1 (`nrow`, `ncol`, `nlyr`);
#'   * `resolution`---100, 100  (`x`, `y`);
#'   * `extent`---695492.1, 714492.1, 6246657, 6273057  (`xmin`, `xmax`, `ymin`, `ymax`);
#'   * `coord. ref.`---WGS 84 / UTM zone 29N (EPSG:32629);
#'
#' # Coastline
#'
#' [`dat_coast()`] returns a [`SpatVector`] of coastline for the same region.
#'
#' # Marine Protected Area
#'
#' [`dat_mpa()`] returns a [`SpatVector`] of the boundaries of the Loch Sunart to the Sound of Jura Marine Protected Area (MPA) with the following columns:
#'   * `id`---A `character` label for each polygon;
#'   * `open`---A `character` that defines whether or not each `id` is `open` or `closed` to fishing;
#'   * `col`---A `character` that defines a colour (for visualising);
#'
#' @source Biologging and biotelemetry data were collected by, and belong to, Marine Scotland Science and NatureScot. Data were processed by Lavender (2022). If you wish to use these data, please contact Marine Scotland Science and NatureScot for further information.
#'
#' Bathymetry data were sourced from GEBCO Compilation Group (2019) GEBCO 2019 Grid. \url{https://www.doi.org/10.5285/836f016a-33be-6ddc-e053-6c86abc0788e}
#'
#' Coastline data were sourced from [`flapper::dat_coast`](https://edwardlavender.github.io/flapper/reference/dat_coast.html).
#'
#' MPA boundary data were assembled by E. Lavender.
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
"dat_detections"

#' @rdname datasets-mefs
"dat_archival"

#' @rdname datasets-mefs
#' @export

dat_gebco <- function(.return = c("SpatRaster", "character")) {
  .return <- match.arg(.return)
  path <- system.file("extdata", "dat_gebco.tif",
                      package = "patter", mustWork = TRUE)
  if (.return == "character") {
    return(path)
  } else {
    return(terra::rast(path))
  }
}

#' @rdname datasets-mefs
#' @export

dat_coast <- function(.return = c("SpatVector", "character")) {
  .return <- match.arg(.return)
  path <- system.file("extdata", "dat_coast.gpkg",
                      package = "patter", mustWork = TRUE)
  if (.return == "character") {
    return(path)
  } else {
    return(terra::vect(path))
  }
}

#' @rdname datasets-mefs
#' @export

dat_mpa <- function(.return = c("SpatVector", "character")) {
  .return <- match.arg(.return)
  path <- system.file("extdata", "dat_mpa.gpkg",
                      package = "patter", mustWork = TRUE)
  if (.return == "character") {
    return(path)
  } else {
    return(terra::vect(path))
  }
}
