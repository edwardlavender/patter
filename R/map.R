#' @title Map: map probability-of-use
#' @description This function builds a 'probability-of-use' utilisation distribution.
#'
#' @param .map A [`SpatRaster`] that defines the grid for probability-of-use estimation. `NAs` on `.map` are used as a mask.
#' @param .coord Coordinates, provided in any format accepted by [`.map_coord()`]
#'
#' @param .plot A logical input that defines whether or not to plot the [`SpatRaster`].
#' @param ... If `.plot = TRUE`, `...` is a place holder for additional arguments passed to [`terra::plot()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details Probability-of-use is the proportion of samples of each unique cell (out of the total number of samples across all time steps).
#'
#' @return The function returns a [`SpatRaster`] (utilisation distribution) in which cell values define probability-of-use.
#' @example man/examples/map_pou-examples.R
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward()`];
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_files()`];
#'
#' * The backward pass is implemented by [`pf_backward_*()`];
#'
#' * Movement paths are built from PF outputs via `pf_path()` functions:
#'     * [`pf_path()`] reconstructs paths;
#'     * [`pf_path_pivot()`] supports path reconstruction;
#'
#' * To reconstruct maps of space use, see:
#'     * [`pf_coord()`] to extract particle coordinates;
#'     * [`map_pou()`] for probability-of-use maps;
#'     * [`map_dens()`] for smooth utilisation distributions;
#'     * [`get_hr()`] for home range estimates;
#'
#' @author Edward Lavender
#' @export

map_pou <-
  function(.map, .coord, .plot = TRUE, ..., .verbose = TRUE) {

    #### Initiate
    t_onset <- Sys.time()
    cat_log <- cat_init(.verbose = .verbose)
    cat_log(call_start(.fun = "map_pou", .start = t_onset))
    on.exit(cat_log(call_end(.fun = "map_pou", .start = t_onset, .end = Sys.time())), add = TRUE)

    #### Check user inputs
    check_inherits(.map, "SpatRaster")
    rlang::check_dots_used()

    #### Get XYM (cell IDs and marks)
    cat_log("... Processing `.coord`...")
    xym <- .map_coord(.map = .map, .coord = .coord)

    #### Build SpatRaster
    map <- terra::setValues(.map, 0)
    map <- terra::mask(map, .map)
    map[xym$cell_id] <- xym$mark
    if (.plot) {
      terra::plot(map, ...)
    }

    #### Return outputs
    map

  }

#' @title Map: map point density
#' @description [`map_dens()`] creates a smoothed density map (e.g., of particle samples).
#' @param .map A [`SpatRaster`] that defines the grid for density estimation and, if `.coord = NULL`, the points (and associated weights) that are smoothed. Weights must sum to one. The coordinate reference system of `.map` must be planar and specified.
#' @param .im,.owin A pixel image representation of `.map` (see [`as.im.SpatRaster()`] and [`spatstat.geom::im()`]) and an observation window (see [`as.owin.SpatRaster()`], [`as.owin.sf()`] and [`spatstat.geom::owin()`]). These objects may be computed automatically from `.map` (with rectangular or gridded observation windows used by default, depending on whether or not `.map` contains `NA`s), but this option can be over-ridden. For faster results, use a rectangular or polygon observation window (see [`as.owin.sf()`]). If `.coord` is supplied, `.im` is necessarily (re)-defined internally (see Details).
#' @param .poly,.bbox,.invert For [`as.owin.sf`] to construct observation windows from `sf` objects.
#' * `.poly` is an `sf` polygon object;
#' * `.bbox` is the bounding of a simple feature (see [`sf::st_bbox()`]);
#' * `.invert` is a logical variable that defines whether or not to invert `.poly` (e.g., to turn a terrestrial polygon into an aquatic polygon);
#' @param .coord (optional) A [`matrix`], [`data.frame`] or [`data.table`] with x and y coordinates, in columns named `x` and `y` or `cell_x` and `cell_y`. `x` and `y` columns are used preferentially. Coordinates must be planar.  A `timestep` column can also be included if there are multiple possible locations at each time step. A `mark` column can be included with coordinate weights; otherwise, equal weights are assumed (see Details). Other columns are ignored.
#' @param .plot A `logical` variable that defines whether or not to plot the output.
#' @param .use_tryCatch A `logical` variable that controls error handling:
#' * If `.use_tryCatch = FALSE`, if density estimation fails with an error, the function fails with the same error.
#' * If `.use_tryCatch = TRUE`, if density estimation fails with an error, the function produces a warning with the error message and returns `NULL`.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#' @param ... Arguments passed to [`spatstat.explore::density.ppp()`], such as `sigma` (i.e., the bandwidth).
#'
#' @details
#'
#' [`map_dens()`] smooths (a) a [`SpatRaster`] or (b) a set of inputted coordinates:
#' * If `.coords` is `NULL`, `.map` cell coordinates are used for density estimation and cell values are used as weights.
#' * If coordinates are supplied, coordinates are re-expressed on `.map` and then used for density estimation. This option is generally faster. Equal weights are assumed unless specified. Default or supplied weights are normalised to sum to one at each time step. The total weight of each location within time steps is calculated and then these weights are aggregated by location across the whole time series and renomalised. See the internal [`.map_mark()`] function for full details.
#'
#' Cell coordinates are converted to a [`spatstat.geom::ppp()`] object, which is passed, alongside the observation window (`.owin`) and an image of the weights to [`spatstat.explore::density.ppp()`] for the estimation. Weights must sum to one.
#'
#' [`as.im.SpatRaster()`], [`as.owin.SpatRaster()`] and [`as.owin.sf()`] are helper functions that convert a [`SpatRaster`] to a pixel image and an observation window (see [`spatstat.geom::owin()`]). [`as.im.SpatRaster`] is based on `maptools::as.im.RasterLayer()`. [`as.owin.SpatRaster()`] either defines a rectangular window, if there are no NAs on `.map`, or converts `.map` directly to an `owin` object. Gridded observation windows, especially if high resolution, considerably slow down density estimation and may exhaust vector memory. Use rectangular windows, or convert `sf` objects to polygon windows (via `as.owin.sf()`]) if possible.
#'
#' Coordinates and associated weights are smoothed via [`spatstat.explore::density.ppp()`] into an image. Pixel resolution and smoothing parameters such as bandwidth can be controlled via `...` arguments which are passed directly to this function. The output is translated into a gridded probability density surface (on the geometry defined by `.map`).
#'
#' @return The function returns a normalised [`SpatRaster`] (or `NULL` if [`spatstat.explore::density.ppp()`] fails and `.use_tryCatch = TRUE`).
#'
#' @example man/examples/map_dens-examples.R
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward()`];
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_files()`];
#'
#' * The backward pass is implemented by [`pf_backward_*()`];
#'
#' * Movement paths are built from PF outputs via `pf_path()` functions:
#'     * [`pf_path()`] reconstructs paths;
#'     * [`pf_path_pivot()`] supports path reconstruction;
#'
#' * To reconstruct maps of space use, see:
#'     * [`pf_coord()`] to extract particle coordinates;
#'     * [`map_pou()`] for probability-of-use maps;
#'     * [`map_dens()`] for smooth utilisation distributions;
#'     * [`get_hr()`] for home range estimates;

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
  if (.invert) {
    .poly <- .st_invert(.x = .poly, .bbox = .bbox)
  }
  spatstat.geom::as.owin(.poly)
}

#' @rdname map_dens
#' @export

map_dens <- function(.map,
                     .im = NULL, .owin = NULL,
                     .coord = NULL,
                     .plot = TRUE,
                     .use_tryCatch = TRUE,
                     .verbose = TRUE,
                     ...) {

  #### Check user inputs
  # Check packages
  t_onset <- Sys.time()
  rlang::check_installed("spatstat.explore")
  rlang::check_installed("spatstat.geom")
  # Check `.map`
  check_inherits(.map, "SpatRaster")
  # Check dots (`at` and `se` are not currently supported)
  check_dots_allowed(c("at", "se"))
  check_dots_for_missing_period(formals(), list(...))

  #### Set up messages
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "map_dens", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "map_dens", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Process SpatRaster
  # spatstat assumes planar coordinates
  cat_log("... Processing `.map`...")
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

  #### Get ppp
  cat_log("... Building `ppp` object...")

  ## (A) Define coordinates & weights from the SpatRaster (e.g., POU grid)
  if (is.null(.coord)) {
    cat_log("... ... Using `.map`...")
    .coord <- terra::as.data.frame(.map, xy = TRUE, na.rm = TRUE)
    colnames(.coord) <- c("x", "y", "mark")
    .coord <- .coord[which(!is.na(.coord$mark) & .coord$mark != 0), ]
    marks <- .coord[, 3]
    if (!isTRUE(all.equal(sum(marks), 1))) {
      abort("Weights on `.map` should sum to one since `.coord` = NULL.")
    }
  } else {

    ## (B) Define coordinates & weights from `.coord` input
    # i) Define coordinates data.table with x and y columns
    cat_log("... ... Using `.coord`...")
    if (inherits(.coord, "matrix") |
        inherits(.coord, "data.frame") & !inherits(.coord, "data.table")) {
      .coord <- as.data.table(.coord)
    }
    check_inherits(.coord, "data.table")
    contains_xy      <- all(c("x", "y") %in% colnames(.coord))
    contains_cell_xy <- all(c("cell_x", "cell_y") %in% colnames(.coord))
    if (contains_xy) {
      if (contains_cell_xy) {
        msg("`.coord` contains both (`x`, `y`) and (`cell_x`, `cell_y`) coordinates: (`x`, `y`) coordinates used.")
        cell_x <- cell_y <- NULL
        .coord[, cell_x := NULL]
        .coord[, cell_y := NULL]
      }
    } else {
      if (contains_cell_xy) {
        .coord <-
          .coord |>
          rename(x = "cell_x", y = "cell_y") |>
          as.data.table()
      } else {
        abort("`.coord` should contain `x` and `y` (or `cell_x` and `cell_y`) coordinates.")
      }
    }
    # ii) Define cell IDs (if un-supplied) for .map_mark()
    if (is.null(.coord$cell_id)) {
      cell_id <- NULL
      .coord[, cell_id := terra::cellFromXY(.map, cbind(.coord$x, .coord$y))]
    }
    # iii) Define `.coord` with weights for each cell_id
    .coord <- .map_mark(.coord)
    # iv) Define weights on raster image
    marks <- .coord$mark
    .im <- terra::rasterize(x = as.matrix(.coord[, c("x", "y"), drop = FALSE]),
                            y = .map,
                            values = marks)
    .im <- as.im.SpatRaster(.im)
  }

  ## Build ppp object
  cat_log("... ... Defining `ppp` object...")
  rppp <- spatstat.geom::ppp(x = .coord$x, y = .coord$y,
                             window = .owin, marks = marks)
  if (rppp$n == 0L) {
    abort("There are no valid points within the observation window (perhaps you need to invert this?)")
  }

  #### Estimate density surface
  # Get intensity (expected number of points PER UNIT AREA)
  cat_log("... Estimating density surface...")
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
  cat_log("... Scaling density surface...")
  terra::crs(.map) <- crs
  dens <- terra::rast(dens)
  terra::crs(dens) <- crs
  dens <- dens * terra::cellSize(dens, unit = "m")
  # Translate expect counts into proportion of points per pixel
  dens <- dens / terra::global(dens, "sum", na.rm = TRUE)[1, 1]
  if (!terra::compareGeom(dens, .map, stopOnError = FALSE, messages = FALSE)) {
    cat_log("... ... Resampling density surface onto `.map`...")
    dens <- terra::resample(dens, .map, method = "near")
    # dens <- terra::mask(dens, .map)
    dens <- dens / terra::global(dens, "sum", na.rm = TRUE)[1, 1]
  }
  stopifnot(all.equal(1, terra::global(dens, "sum", na.rm = TRUE)[1, 1]))

  #### Plot density
  if (.plot) {
    ext <- terra::ext(.map)
    terra::plot(dens, xlim = ext[1:2], ylim = ext[3:4])
  }

  # Return map
  dens

}

#' @title Get animal 'home ranges'
#' @description These functions extract 'home range' estimates from a [`SpatRaster`] that describes the intensity of movements within an area.
#'
#' @param .map A [`SpatRaster`] (utilisation distribution).
#' @param .prop For [`map_hr_prop()`], `.prop` is a number that defines the range proportion.
#' @param .add A logical variable that defines whether or not to add a polygon of the range to an existing map.
#' @param ... If `.add = TRUE`, `...` is a place holder for additional arguments passed to [`terra::plot()`].
#'
#' @details These functions are modelled on the [`map_hr_*()`](https://edwardlavender.github.io/flapper/reference/map_hr.html) functions in the [`flapper`](https://github.com/edwardlavender/flapper) package, where full details are provided. The `spatialEco` package is required.
#'
#' @return The functions return a [`SpatRaster`]. Cells with a value of one are inside the specified range boundaries; cells with a value of zero are beyond range boundaries. If `.add` is `TRUE`, the boundaries are added to an existing plot.
#'
#' @examples
#' #### Set up example
#' # Define hypothetical input SpatRaster
#' require(terra)
#' r <- rast()
#' n <- ncell(r)
#' i <- 2e4
#' r[i] <- 1
#' r <- distance(r)
#' r <- r / global(r, "sum")[1, 1]
#' plot(r)
#'
#' #### Examples
#' map <- map_hr_full(r, .add = TRUE, lwd = 5)
#' map <- map_hr_home(r, .add = TRUE, border = "blue")
#' map <- map_hr_core(r, .add = TRUE, border = "orange")
#' map <- map_hr_prop(r, .prop = 0.2, .add = TRUE, border = "red")
#'
#' @seealso
#' * For reconstructing movement paths and patterns of space use, see [`pf_forward()`];
#' * For mapping utilisation distributions, see [`map_pou()`] and [`map_dens()`];
#'
#' @author Edward Lavender
#' @name map_hr
NULL

#' @name map_hr
#' @export

map_hr_prop <- function(.map, .prop = 0.5, .add = FALSE, ...) {
  rlang::check_installed("spatialEco")
  check_dots_for_missing_period(formals(), list(...))
  rlang::check_dots_used()
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
  map <- spatialEco::raster.vol(.map, p = .prop, sample = FALSE)
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
