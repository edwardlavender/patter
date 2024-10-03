#' @title Glossary
#'
#' @description This is a glossary of key arguments in [`patter`].
#'
#' # `.map`
#'
#' `.map` is [`SpatRaster`] (supported on Windows or MacOS) or a `character` that defines the path to raster (supported on Windows, MacOS and Linux) to a that defines the area of interest. `NAs` define inhospitable regions (such as land). A planar (Universal Transverse Mercator) projection with coordinates in metres is currently required.
#'
#' # `.model_obs`
#'
#' `.model_obs` is a named `list` of [`ModelObs`] sub-types (named) and associated parameters ([`data.table`] elements).
#'
#' # `.model_move`
#'
#' `.model_move` is a `character` string that defines a [`ModelMove`] instance.
#'
#' # `.state`
#'
#' `.state` is a `character` that defines a [`State`] sub-type.
#'
#' # `.yobs`
#'
#' `.yobs` is a named `list` of observational datasets, one for each data type. Element names must correspond [`ModelObs`] sub-types defined in `Julia`. Each element must be a [`data.table`] with the following columns:
#'
#' * `timestamp`---a `POSIXct` vector of time stamps;
#' * `sensor_id`---an `integer` vector of sensor IDs, such as receivers;
#' * `obs`---a vector of observations;
#' * Additional columns that define the parameters of the observation model, as defined by a [`ModelObs`] structure;
#'
#' No other columns should be included.
#'
#' For real-world analyses, see [`assemble`]`_*()` functions to format datasets as required. Other types of datasets require manual preparation.
#'
#' @seealso [`State`], [`ModelMove`], [`ModelObs`], [`sim_path_walk()`], [`sim_observations()`], [`pf_filter()`]
#' @author Edward Lavender
#' @name glossary
NULL
