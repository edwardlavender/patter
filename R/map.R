#' @title Map: probability-of-use
#' @description This function builds a 'probability-of-use' utilisation distribution.
#'
#' @param .map A [`SpatRaster`] that defines the grid for probability-of-use estimation.
#' @param .coord Coordinates, provided in any format accepted by [`.map_coord()`]
#'
#' @param .plot,... A logical input that defines whether or not to plot the [`SpatRaster`] and additional arguments passed to [`terra::plot()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details Probability-of-use is calculated via [`.map_coord()`] (and [`.map_mark()`]). If a single dataset of unweighted coordinates is provided, probability-of-use is simply the proportion of records in each grid cell. If a time series of unweighted coordinates is provided, probability-of-use is effectively the average proportion of records in each grid cell. This becomes a weighted average if coordinates are weighted. Weights are normalised to sum to one and the result can be interpreted as a utilisation distribution in which cell values define probability-of-use. Maps are sensitive to grid resolution.
#'
#' This function replaces [`flapper::pf_plot_map()`](https://edwardlavender.github.io/flapper/reference/pf_plot_map.html).
#'
#' @return The function returns a [`SpatRaster`].
#' @example man/examples/example-map_pou.R
#'
#' @seealso `map_*()` functions build maps of space use:
#' * [`map_pou()`] maps probability-of-use;
#' * [`map_dens()`] maps point density;
#' * [`map_hr`]`_*()` functions map home ranges;
#'
#' All maps are represented as [`SpatRaster`]s.
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
    map

  }

#' @title Map: point density
#' @description [`map_dens()`] creates a smoothed utilisation distribution (UD).
#' @param .map A [`SpatRaster`] that defines the grid on which the UD is represented. If `.coord = NULL`, `.map` also defines the points (and associated weights) that are smoothed (see [`.map_coord()`]). **The coordinate reference system of `.map` must be planar** and specified.
#' @param .im,.owin (optional) A pixel image representation of `.map` (see [`as.im.SpatRaster()`] and [`spatstat.geom::im()`]) and an observation window (see [`as.owin.SpatRaster()`], [`as.owin.sf()`] and [`spatstat.geom::owin()`]). These objects may be computed automatically from `.map` (with rectangular or gridded observation windows used by default, depending on whether or not `.map` contains `NA`s), but this option can be over-ridden. For faster results, use a rectangular or polygon observation window (see [`as.owin.sf()`]). If `.coord` is supplied, `.im` is necessarily (re)-defined internally (see Details).
#' @param .poly,.bbox,.invert For [`as.owin.sf()`] to construct observation windows from `sf` objects.
#' * `.poly` is an `sf` polygon object;
#' * `.bbox` is the bounding of a simple feature (see [`sf::st_bbox()`]);
#' * `.invert` is a logical variable that defines whether or not to invert `.poly` (e.g., to turn a terrestrial polygon into an aquatic polygon);
#' @param .coord (optional) Coordinates for density estimation, provided in any format accepted by [`.map_coord()`]. **Coordinates must be planar**.
#' @param .discretise If `.coord` is provided, `.discretise` is a `logical` variable that defines whether or not to discretise coordinates on `.map` (see [`.map_coord()`]).
#' @param ... Arguments for density estimation, passed to [`spatstat.explore::density.ppp()`], such as `sigma` (i.e., the bandwidth). `at` and `se` are not permitted.
#' @param .plot A `logical` variable that defines whether or not to plot the output.
#' @param .use_tryCatch A `logical` variable that controls error handling:
#' * If `.use_tryCatch = FALSE`, if density estimation fails with an error, the function fails with the same error.
#' * If `.use_tryCatch = TRUE`, if density estimation fails with an error, the function produces a warning with the error message and returns `NULL`.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' [`map_dens()`] smooths (a) a [`SpatRaster`] or (b) a set of inputted coordinates.
#'
#' [`.map_coord()`] (and [`.map_mark()`]) are used to define coordinates and weights:
#' * If `.coords` is `NULL`, `.map` cell coordinates are used for density estimation and cell values are used as weights.
#' * If coordinates are supplied, coordinates are optionally re-expressed on `.map` and then used for density estimation. This option is generally faster. Coordinate weights are defined by [`.map_mark()`].
#'
#' Cell coordinates are converted to a [`spatstat.geom::ppp()`] object, which is passed, alongside the observation window (`.owin`) and a pixel-image representation of the weights to [`spatstat.explore::density.ppp()`] for the estimation. Weights must sum to one.
#'
#' [`as.im.SpatRaster()`], [`as.owin.SpatRaster()`] and [`as.owin.sf()`] are helper functions that convert a [`SpatRaster`] to a pixel image and an observation window (see [`spatstat.geom::owin()`]). [`as.im.SpatRaster`] is based on `maptools::as.im.RasterLayer()`. [`as.owin.SpatRaster()`] either defines a rectangular window, if there are no NAs on `.map`, or converts `.map` directly to an `owin` object. Gridded observation windows, especially if high resolution, considerably slow down density estimation and may exhaust vector memory. Use rectangular windows, or convert `sf` objects to polygon windows (via [`as.owin.sf()`]) if possible.
#'
#' Coordinates and associated weights are smoothed via [`spatstat.explore::density.ppp()`] into an image. Pixel resolution and smoothing parameters such as bandwidth can be controlled via `...` arguments which are passed directly to this function. The output is translated into a gridded probability density surface (on the geometry defined by `.map`).
#'
#' This function replaces `flapper::kud*()` and `flapper::pf_kud*()` routines based on `adehabitatHR` (see [here](https://edwardlavender.github.io/flapper/reference/)).
#'
#' @return The function returns a normalised [`SpatRaster`] (or `NULL` if [`spatstat.explore::density.ppp()`] fails and `.use_tryCatch = TRUE`).
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

map_dens <- function(.map,
                     .im = NULL, .owin = NULL,
                     .coord = NULL, .discretise = FALSE, ...,
                     .plot = TRUE,
                     .use_tryCatch = TRUE,
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
  check_dots_allowed(c("at", "se"), ...)
  check_dots_for_missing_period(formals(), list(...))

  #### Process SpatRaster
  # spatstat assumes planar coordinates
  cats$cat("... Processing `.map`...")
  crs <- terra::crs(.map)
  if (is.na(crs)) {
    abort("`terra::crs(.map)` must be specified (and should be planar).")
  }
  terra::crs(.map) <- NA
  # Define pixel image & window
  # * The pixel image represents the study area
  # * We need this to define the observation window, which must be based on .map
  # * If `.coord` is NULL, we will also use the pixel image for the estimation
  # * But if `.coord` is supplied, we need to redefine the image used for estimation
  if (is.null(.im)) {
    .im <- as.im.SpatRaster(.map)
  }
  if (is.null(.owin)) {
    .owin <- as.owin.SpatRaster(.map, .im = .im)
  }

  #### Get XYM
  cats$cat("... Building XYM...")
  # Define coordinates and weights for density estimation
  use_coord <- !is.null(.coord)
  .coord <- .map_coord(.map = .map, .coord = .coord, .discretise = .discretise)
  # Represent weights on SpatRaster, if required
  if (use_coord) {
    .im <- terra::rasterize(x = as.matrix(.coord[, c("x", "y"), drop = FALSE]),
                            y = .map,
                            values = .coord$mark)
    .im <- as.im.SpatRaster(.im)
  }

  ## Build ppp object
  cats$cat("... Defining `ppp` object...")
  rppp <- spatstat.geom::ppp(x = .coord$x, y = .coord$y,
                             window = .owin, marks = .coord$mark)
  if (rppp$n == 0L) {
    abort("There are no valid points within the observation window (perhaps you need to invert this?)")
  }

  #### Estimate density surface
  # Get intensity (expected number of points PER UNIT AREA)
  cats$cat("... Estimating density surface...")
  dens <- tryCatch(spatstat.explore::density.ppp(rppp, weights = .im,
                                                 at = "pixels", se = FALSE, ...),
                   error = function(e) e)
  if (inherits(dens, "error")) {
    if (!.use_tryCatch) {
      abort(dens)
    } else {
      warn(paste("\n", paste(dens, collapse = "\n ")))
      return(NULL)
    }
  }
  # Translate intensity into expected number of points PER PIXEL
  cats$cat("... Scaling density surface...")
  terra::crs(.map) <- crs
  dens <- terra::rast(dens)
  terra::crs(dens) <- crs
  dens <- dens * terra::cellSize(dens, unit = "m")
  # Translate expect counts into proportion of points per pixel
  dens <- dens / terra::global(dens, "sum", na.rm = TRUE)[1, 1]
  if (!terra::compareGeom(dens, .map, stopOnError = FALSE, messages = FALSE)) {
    cats$cat("... ... Resampling density surface onto `.map`...")
    dens <- terra::resample(dens, .map, method = "near")
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
  dens

}

#' @title Map: animal home ranges
#' @description These functions extract 'home range' estimates from a [`SpatRaster`] that describes the intensity of movements within an area.
#'
#' @param .map A [`SpatRaster`] (utilisation distribution).
#' @param .prop For [`map_hr_prop()`], `.prop` is a number that defines the range proportion.
#' @param .add A logical variable that defines whether or not to add a polygon of the range to an existing map.
#' @param ... If `.add = TRUE`, `...` is a place holder for additional arguments passed to [`terra::plot()`].
#'
#' @details These functions are modelled on [`flapper::map_hr_*()`](https://edwardlavender.github.io/flapper/reference/get_hr.html) functions, where full details are provided.
#'
#' @return The functions return a [`SpatRaster`]. Cells with a value of one are inside the specified range boundaries; cells with a value of zero are beyond range boundaries. If `.add` is `TRUE`, the boundaries are added to an existing plot.
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
