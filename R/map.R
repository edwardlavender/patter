#' @title Map: probability-of-use
#' @description This function builds a 'probability-of-use' utilisation distribution.
#'
#' @param .map A [`terra::SpatRaster`] that defines the grid for probability-of-use estimation.
#' @param .coord Coordinates, provided in any format accepted by [`.map_coord()`]
#'
#' @param .plot,... A logical input that defines whether or not to plot the [`terra::SpatRaster`] and additional arguments passed to [`terra::plot()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details Probability-of-use is calculated via [`.map_coord()`] (and [`.map_mark()`]). If a single dataset of unweighted coordinates is provided, probability-of-use is simply the proportion of records in each grid cell. If a time series of unweighted coordinates is provided, probability-of-use is effectively the average proportion of records in each grid cell. This becomes a weighted average if coordinates are weighted. Weights are normalised to sum to one and the result can be interpreted as a utilisation distribution in which cell values define probability-of-use. Maps are sensitive to grid resolution.
#'
#' This function replaces [`flapper::pf_plot_map()`](https://edwardlavender.github.io/flapper/reference/pf_plot_map.html).
#'
#' On Linux, this function cannot be used within a `Julia` session.
#'
#' @return The function returns a named `list` with the following elements:
#' * `ud`: a normalised [`terra::SpatRaster`];
#' @example man/examples/example-map_pou.R
#'
#' @seealso `map_*()` functions build maps of space use:
#' * [`map_pou()`] maps probability-of-use;
#' * [`map_dens()`] maps point density;
#' * [`map_hr`]`_*()` functions map home ranges;
#'
#' All maps are represented as [`terra::SpatRaster`]s.
#'
#' To derive coordinates for mapping patterns of space use for tagged animals, see:
#' * [`coa()`] to calculate centre-of-activity;
#' * [`pf_filter()`] and associates to sample locations using particle filtering;
#'
#' @author Edward Lavender
#' @export

map_pou <-
  function(.map, .coord,
           .plot = TRUE, ...,
           .verbose = getOption("patter.verbose")) {

    #### Initiate
    cats <- cat_setup(.fun = "map_pou", .verbose = .verbose)
    on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

    #### Check user inputs
    # check_dots_used: terra::plot() warnings used
    check_dots_for_missing_period(formals(), list(...))
    check_inherits(.map, "SpatRaster")

    #### Get XYM (cell IDs and marks)
    # TO DO
    # * Add `.shortcut` argument, as in `map_dens()`, to circumvent this step
    cats$cat("... Building XYM...")
    xym <- .map_coord(.map = .map, .coord = .coord, .discretise = TRUE)

    #### Build SpatRaster
    cats$cat("... Building SpatRaster...")
    map <- terra::setValues(.map, 0)
    map <- terra::mask(map, .map)
    map[xym$id] <- xym$mark
    if (.plot) {
      terra::plot(map, ...)
    }

    #### Return outputs
    list(ud = map)

  }

#' @title Map: point density
#' @description [`map_dens()`] creates a smoothed utilisation distribution (UD).
#' @param .map A [`terra::SpatRaster`] that defines the grid on which the UD is represented. If `.coord = NULL`, `.map` also defines the points (and associated weights) that are smoothed (see [`.map_coord()`]). **The coordinate reference system of `.map` must be planar** and specified.
#' @param .im,.owin A pixel image representation of `.map` (see [`as.im.SpatRaster()`] and [`spatstat.geom::im()`]) and an observation window (see [`as.owin.SpatRaster()`], [`as.owin.sf()`] and [`spatstat.geom::owin()`]). If un-supplied, `.owin` is defined automatically from `.map` via [`as.owin.SpatRaster()`], which uses [`as.im.SpatRaster()`] internally (see Details). For faster results, use a rectangular or polygon observation window (see [`as.owin.sf()`]).
#' @param .poly,.bbox,.invert For [`as.owin.sf()`] to construct observation windows from `sf` objects.
#' * `.poly` is an `sf` polygon object;
#' * `.bbox` is the bounding of a simple feature (see [`sf::st_bbox()`]);
#' * `.invert` is a logical variable that defines whether or not to invert `.poly` (e.g., to turn a terrestrial polygon into an aquatic polygon);
#' @param .coord (optional) Coordinates for density estimation, provided in any format accepted by [`.map_coord()`]. **Coordinates must be planar**.
#' @param .discretise If `.coord` is provided, `.discretise` is a `logical` variable that defines whether or not to discretise coordinates on `.map` (see [`.map_coord()`]).
#' @param .sigma,X `.sigma` is a `numeric` value or a `function` that specifies the smoothing bandwidth (passed to [`spatstat.explore::density.ppp()`]'s `sigma` argument). The default option is [`bw.h()`], which sets the bandwidth based on combined variance of summarised coordinates, formatted as a point pattern `X` (see [`spatstat.geom::ppp`]), using the ad-hoc method (Worton, 1989). See `spatstat` functions (e.g., [`spatstat.explore::bw.diggle()`]) for more sophisticated methods.
#' @param .shortcut (optional) A named `list` from a previous call to [`map_dens()`]. If supplied, the function short-cuts straight to smoothing (`.owin`, `.coord` and `.discretise` are silently unused).
#' @param ... Arguments for density estimation, passed to [`spatstat.explore::density.ppp()`], such as `sigma` (i.e., the bandwidth). `at` and `se` are not permitted.
#' @param .fterra A `logical` variable that defines whether or not to parallelise [`terra::resample()`].
#' @param .plot A `logical` variable that defines whether or not to plot the output.
#' @param .tryCatch A `logical` variable that controls error handling:
#' * If `.tryCatch = FALSE`, if density estimation fails with an error, the function fails with the same error.
#' * If `.tryCatch = TRUE`, if density estimation fails with an error, the function produces a warning with the error message and returns `NULL`.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' [`map_dens()`] smooths (a) a [`terra::SpatRaster`] or (b) a set of inputted coordinates.
#'
#' [`.map_coord()`] (and [`.map_mark()`]) are used to define coordinates and weights:
#' * If `.coords` is `NULL`, `.map` cell coordinates are used for density estimation and cell values are used as weights.
#' * If coordinates are supplied, coordinates are optionally re-expressed on `.map` and then used for density estimation. This option is generally faster. Coordinate weights are defined by [`.map_mark()`].
#'
#' Cell coordinates and associated weights are converted to a [`spatstat.geom::ppp()`] object, which is passed, alongside the observation window (`.owin`), to [`spatstat.explore::density.ppp()`] for the estimation. Weights must sum to one.
#'
#' [`as.im.SpatRaster()`], [`as.owin.SpatRaster()`] and [`as.owin.sf()`] are helper functions that convert a [`terra::SpatRaster`] to a pixel image and an observation window (see [`spatstat.geom::owin()`]). [`as.im.SpatRaster()`] is based on `maptools::as.im.RasterLayer()`. [`as.owin.SpatRaster()`] either defines a rectangular window, if there are no NAs on `.map`, or converts `.map` directly to an `owin` object. Gridded observation windows, especially if high resolution, considerably slow down density estimation and may exhaust vector memory. Use rectangular windows, or convert `sf` objects to polygon windows (via [`as.owin.sf()`]) if possible.
#'
#' If `.shortcut` is supplied, the preceding steps can be skipped and the function short-cuts straight to smoothing. Use this option if the preceding steps are slow and you want to trial different smoothing options (such as `sigma` functions).
#'
#' Coordinates and associated weights are smoothed via [`spatstat.explore::density.ppp()`] into an image. Pixel resolution and smoothing parameters such as bandwidth can be controlled via `...` arguments which are passed directly to this function. The default bandwidth is set via [`bw.h()`] (see `.sigma`). The output is translated into a gridded probability density surface (on the geometry defined by `.map`). This process may use [`terra::resample()`], which can be parallelised via `.fterra` (which controls the `threads` argument of that function).
#'
#' This function replaces `flapper::kud*()` and `flapper::pf_kud*()` routines based on `adehabitatHR` (see [here](https://edwardlavender.github.io/flapper/reference/)).
#'
#'  On Linux, these functions cannot be used within a `Julia` session.
#'
#' @references
#' Worton, B. J. (1989). Kernel Methods for Estimating the Utilization Distribution in Home-Range Studies. Ecology 70, 164–168. doi: 10.2307/1938423
#'
#' @return The function returns a named `list`, with the following elements:
#' * `x`: a [`spatstat.geom::ppp`] object that defines points for density estimation;
#' * `D`: a [`spatstat.geom::im`] object of estimated intensities, from [`spatstat.explore::density.ppp()`];
#' * `ud`: a normalised [`terra::SpatRaster`];
#'
#' `D` and `ud` are `NULL` if [`spatstat.explore::density.ppp()`] fails and `.tryCatch = TRUE`.
#'
#' @example man/examples/example-map_dens.R
#' @inherit map_pou seealso
#' @rdname map_dens
#' @export

as.im.SpatRaster <- function(.map) {
  # Check user inputs
  rlang::check_installed("spatstat.geom")
  if (!terra::hasValues(.map))
    abort("The SpatRaster is empty.")
  if (terra::is.rotated(.map)) {
    abort("The SpatRaster is rotated.")
  }
  # Coerce SpatRaster
  rs <- terra::res(.map)
  # Define xmin and ymin (shifted to the cell centre)
  # orig <- sp::bbox(.map)[, 1] + 0.5 * rs
  orig <- terra::ext(.map)[c(1, 3)] + 0.5 * rs
  dm <- dim(.map)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(rs[1], dm[1] - 1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(rs[2], dm[2] - 1))))
  val <- terra::values(.map)
  dim(val) <- dm
  val <- spatstat.geom::transmat(val, from = list(x = "-i", y = "j"), to = "spatstat")
  spatstat.geom::im(val, xcol = xx, yrow = yy)
}

#' @rdname map_dens
#' @export

as.owin.SpatRaster <- function(.map, .im = NULL) {
  if (terra::global(.map, function(x) any(is.na(x)))[1, 1]) {
    # Define the window based on rim if there are NAs
    msg("Observation window is gridded.")
    if (is.null(.im)) {
      .im <- as.im.SpatRaster(.map)
    }
    rwin <- spatstat.geom::as.owin(.im)
  } else {
    # Define the window based on the extent otherwise (for improved speed)
    message("Observation window is rectangular.")
    ext  <- terra::ext(.map)
    rwin <- spatstat.geom::as.owin(W = c(xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4]))
  }
  rwin
}

#' @rdname map_dens
#' @export

as.owin.sf <- function(.poly, .bbox = sf::st_bbox(.poly), .invert = TRUE) {
  rlang::check_installed("sf")
  if (.invert) {
    .poly <- st_invert(.x = .poly, .bbox = .bbox)
  }
  spatstat.geom::as.owin(.poly)
}

#' @rdname map_dens
#' @export

bw.h <- function(X) {
  # Code from adehabitatHR:::.kernelUDs with tweaks
  # & adapted for spatstat.explore
  sdxy <- sqrt(0.5 * (var(X$x) + var(X$y)))
  sdxy * (X$n^(-1/6))
}

#' @rdname map_dens
#' @export

map_dens <- function(.map,
                     .owin = as.owin.SpatRaster(.map),
                     .coord = NULL, .discretise = FALSE,
                     .shortcut = list(), .sigma = bw.h, ...,
                     .fterra = FALSE,
                     .plot = TRUE,
                     .tryCatch = TRUE,
                     .verbose = getOption("patter.verbose")
                     ) {

  #### Initiate
  cats <- cat_setup(.fun = "map_dens", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Check user inputs
  # Check packages
  rlang::check_installed("spatstat.explore")
  rlang::check_installed("spatstat.geom")
  # Check `.map`
  check_inherits(.map, "SpatRaster")
  # Check dots
  # * check_dots_used(): this is not possible
  check_dots_allowed(c("sigma", "at", "se"), ...)
  check_dots_for_missing_period(formals(), list(...))

  #### Process SpatRaster
  # spatstat assumes planar coordinates
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Processing `.map`..."))
  crs <- terra::crs(.map)
  if (is.na(crs) | crs == "") {
    abort("`terra::crs(.map)` must be specified (and should be planar).")
  }
  terra::crs(.map) <- NA

  #### Build objects for density surface estimation
  if (length(.shortcut) > 0L) {

    check_named_list(.shortcut)
    check_names(.shortcut, "x")
    rppp <- .shortcut$x

  } else {

    # Get XYM
    cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Building XYM..."))
    .coord <- .map_coord(.map = .map, .coord = .coord, .discretise = .discretise)

    # Build ppp object
    cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Defining `ppp` object..."))
    rppp <- spatstat.geom::ppp(x = .coord$x, y = .coord$y,
                               window = .owin, marks = .coord$mark)
    if (rppp$n == 0L) {
      abort("There are no valid points within the observation window (perhaps you need to invert this?)")
    }

  }

  #### Estimate density surface
  # Get intensity (expected number of points PER UNIT AREA)
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Estimating density surface..."))
  D <- tryCatch(spatstat.explore::density.ppp(x = rppp,
                                              sigma = .sigma,
                                              weights = expression(marks),
                                              at = "pixels", se = FALSE, ...),
                error = function(e) e)
  if (inherits(D, "error")) {
    if (!.tryCatch) {
      abort(D)
    } else {
      # Return error as warning
      # * Use warning() not warn() to avoid glue() error
      warning(paste("\n", paste(D, collapse = "\n ")),
              call. = FALSE, immediate. = TRUE)
      return(list(x = rppp, D = NULL, ud = NULL))
    }
  }

  #### Rasterise intensity surface
  # Translate intensity into expected number of points PER PIXEL
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Scaling density surface..."))
  terra::crs(.map) <- crs
  dens <- terra::rast(D)
  terra::crs(dens) <- crs
  dens <- dens * terra::cellSize(dens, unit = "m")
  # Translate expect counts into proportion of points per pixel
  dens <- dens / terra::global(dens, "sum", na.rm = TRUE)[1, 1]
  if (!terra::compareGeom(dens, .map, stopOnError = FALSE, messages = FALSE)) {
    cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": * Resampling density surface onto `.map`..."))
    dens <- terra::resample(dens, .map, method = "near", threads = .fterra)
    # dens <- terra::mask(dens, .map)
    dens <- dens / terra::global(dens, "sum", na.rm = TRUE)[1, 1]
  }
  stopifnot(isTRUE(all.equal(1, terra::global(dens, "sum", na.rm = TRUE)[1, 1])))

  #### Plot density
  if (.plot) {
    ext <- terra::ext(.map)
    terra::plot(dens, xlim = ext[1:2], ylim = ext[3:4])
  }

  # Return map
  list(x = rppp, D = D, ud = dens)

}

#' @title Map: animal home ranges
#' @description These functions extract 'home range' estimates from a [`terra::SpatRaster`] that describes the intensity of movements within an area.
#'
#' @param .map A [`terra::SpatRaster`] (utilisation distribution).
#' @param .prop For [`map_hr_prop()`], `.prop` is a number that defines the range proportion.
#' @param .add A logical variable that defines whether or not to add a polygon of the range to an existing map.
#' @param ... If `.add = TRUE`, `...` is a place holder for additional arguments passed to [`terra::plot()`].
#'
#' @details These functions are modelled on [`flapper::map_hr_*()`](https://edwardlavender.github.io/flapper/reference/get_hr.html) functions, where full details are provided.
#'
#' On Linux, these functions cannot be used within a `Julia` session.
#'
#' @return The functions return a [`terra::SpatRaster`]. Cells with a value of one are inside the specified range boundaries; cells with a value of zero are beyond range boundaries. If `.add` is `TRUE`, the boundaries are added to an existing plot.
#'
#' @example man/examples/example-map_hr.R
#' @inherit map_pou seealso
#' @author Edward Lavender
#' @name map_hr
NULL

#' @name map_hr
#' @export

map_hr_prop <- function(.map, .prop = 0.5, .add = FALSE, ...) {
  # Check dots
  # * check_dots_used: terra::plot() errors used
  check_dots_allowed("add", ...)
  check_dots_for_missing_period(formals(), list(...))
  if (length(.prop) != 1L) {
    abort("`.prop` should be a single number (proportion).")
  }
  if (.prop <= 0 | .prop > 1) {
    if (.prop == 0) {
      abort("`.prop` equals zero.")
    } else {
      abort("`.prop` should be a proportion (between zero and one).")
    }
  }
  .map <- terra::classify(.map, cbind(0, NA))
  map <- raster.vol(.map, p = .prop, sample = FALSE)
  if (.add) {
    poly <- terra::as.polygons(map == 1)
    if (any(poly[[1]] == 1)) {
      poly <- poly[poly[[1]] == 1]
      terra::plot(poly, add = TRUE, ...)
    } else {
      warn("UD is empty (zero only).")
    }
  }
  invisible(map)
}

#' @name map_hr
#' @export

map_hr_core <- function(.map, .add = FALSE, ...) {
  map_hr_prop(.map = .map, .prop = 0.5, .add = .add, ...)
}

#' @name map_hr
#' @export

map_hr_home <- function(.map, .add = FALSE, ...) {
  map_hr_prop(.map = .map, .prop = 0.95, .add = .add, ...)
}

#' @name map_hr
#' @export

map_hr_full <- function(.map, .add = FALSE, ...) {
  map_hr_prop(.map = .map, .prop = 1, .add = .add, ...)
}
