#' @title PF: particle coordinates
#' @description This function collects particle samples and extracts coordinates.
#'
#' @param .history Particle samples from the particle filter, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object. This must contain a column that defines accepted cell samples at each time step (`cell_now`).
#' * An ordered list of file paths (from [`pf_setup_files()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .bathy The bathymetry [`SpatRaster`].
#' @param .obs,.cols (optional) A [`data.table`] and a character vector of column names in `.obs` to match onto the output. `.obs` must contain a `timestep` column for matching.
#'
#' @details
#' This function is not memory safe.
#'
#' @return The function returns a [`data.table`] that defines timesteps, sampled locations and, optionally, information from `.obs`, with the following columns:
#' * `timestep`---an `integer` vector that defines the time step;
#' * `cell_id`---an `integer` vector that defines the cell ID on `.bathy`;
#' * `cell_x`, `cell_y`, `cell_z`---`double`s that define cell coordinates;
#'
#' If `.obs` is supplied, the output also contains any columns specified in `.cols`.
#'
#' @examples
#' p <- dat_pfb()
#' pf_coords(p$history, dat_gebco())
#'
#' @seealso
#' TO DO
#'
#' @author Edward Lavender
#' @export

pf_coords <- function(.history, .bathy, .obs = NULL, .cols = NULL) {

  # Check user inputs
  # * `.history` is checked via .pf_history_dt()
  # * Check remaining inputs
  if (missing(.bathy)) {
    abort("`.bathy` is required for `pf_coords()`.")
  }
  .pf_path_pivot_checks(.obs, .cols)

  # Define particle coordinates
  p <-
    .history |>
    .pf_history_dt() |>
    rename(cell_id = .data$cell_now) |>
    mutate(cell_xy = terra::xyFromCell(.bathy, .data$cell_id),
           cell_x = .data$cell_xy[, 1],
           cell_y = .data$cell_xy[, 2],
           cell_z = terra::extract(.bathy, .data$cell_id)) |>
    select("timestep", "cell_id", "cell_x", "cell_y", "cell_z") |>
    as.data.table()

  # Add columns from `.obs` by matching by timestep (from `pf_path_pivot()`)
  if (!is.null(.obs)) {
    for (col in .cols) {
      p[, (col) := .obs[[col]][match(p$timestep, .obs$timestep)]]
      if (any(is.na(p[[col]]))) {
        warn("There are NAs in the {col} column in the output.",
             .envir = environment())
      }
    }
  }

  # Return data.table
  p

}

#' @title PF: map probability-of-use
#' @description This function builds a 'probability-of-use' utilisation distribution from (processed) particle samples.
#' @param .history The (processed) particle samples, provided as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object. This must contain a column that defines cell samples at each time step named `cell_now`.
#' * A `character` string that defines the directory in which particle samples were written (as parquet files).
#' @param .bathy A [`SpatRaster`] that defines the grid for the utilisation distribution. `NAs` on `.bathy` are used as a mask.
#' @param .plot A logical input that defines whether or not to plot the [`SpatRaster`].
#' @param ... If `.plot = TRUE`, `...` is a place holder for additional arguments passed to [`terra::plot()`].
#' @details Probability-of-use is the proportion of samples of each unique cell (out of the total number of samples across all time steps).
#' @return The function returns a [`SpatRaster`] (utilisation distribution) in which cell values define probability-of-use.
#' @example man/examples/pf_map_pou-examples.R
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward()`];
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_setup_files()`];
#'
#' * The backward pass is implemented by [`pf_backward()`];
#'
#' * Movement paths are built from PF outputs via `pf_path()` functions:
#'     * [`pf_path()`] reconstructs paths;
#'     * [`pf_path_pivot()`] supports path reconstruction;
#'
#' * To reconstruct maps of space use, see:
#'     * [`pf_coords()`] to extract particle coordinates;
#'     * [`pf_map_pou()`] for probability-of-use maps;
#'     * [`pf_map_dens()`] for smooth utilisation distributions;
#'     * [`get_hr()`] for home range estimates;
#'
#' @author Edward Lavender
#' @export

pf_map_pou <-
  function(.history, .bathy, .plot = TRUE, ...) {

    #### Check user inputs
    check_inherits(.history, c("character", "list"))

    #### Calculate cell weights
    pou <-
      .history |>
      .pf_history_dt(.collect = FALSE) |>
      rename(cell_id = "cell_now") |>
      collect() |>
     .pf_map_weights()

    #### Build SpatRaster
    map <- terra::setValues(.bathy, 0)
    map <- terra::mask(map, .bathy)
    map[pou$cell_id] <- pou$mark
    if (.plot) {
      terra::plot(map, ...)
    }

    #### Return outputs
    map

  }

#' @title PF: map point density
#' @description [`pf_map_dens()`] creates a smoothed density map (e.g., of particle samples).
#' @param .xpf A [`SpatRaster`] that defines the grid for density estimation and, if `.coord = NULL`, the points (and associated weights) that are smoothed. Weights must sum to one. The coordinate reference system of `.xpf` must be planar and specified.
#' @param .im,.owin A pixel image representation of `.xpf` (see [`as.im.SpatRaster()`] and [`spatstat.geom::im()`]) and an observation window (see [`as.owin.SpatRaster()`] and [`spatstat.geom::owin()`]). These objects may be computed automatically from `.xpf`, but this option can be over-ridden. For faster results, use a rectangular or polygon observation window. If `.coord` is supplied, `.im` is necessarily (re)-defined internally (see Details).
#' @param .coord (optional) A [`matrix`], [`data.frame`] or [`data.table`] with x and y coordinates, in columns named `x` and `y` or `cell_x` and `cell_y`. `x` and `y` columns are used preferentially. Coordinates must be planar.  A `timestep` column can also be included if there are multiple possible locations at each time step. A `mark` column can be included with coordinate weights; otherwise, equal weights are assumed (see Details). Other columns are ignored.
#' @param .plot A `logical` variable that defines whether or not to plot the output.
#' @param .use_tryCatch A `logical` variable that controls error handling:
#' * If `.use_tryCatch = FALSE`, if density estimation fails with an error, the function fails with the same error.
#' * If `.use_tryCatch = TRUE`, if density estimation fails with an error, the function produces a warning with the error message and returns `NULL`.
#' @param .verbose,.txt Controls on function prompts and messages (see [`pf_forward()`].
#' @param ... Arguments passed to [`spatstat.explore::density.ppp()`], such as `sigma` (i.e., the bandwidth).
#'
#' @details
#'
#' [`pf_map_dens()`] smooths (a) a [`SpatRaster`] or (b) a set of inputted coordinates:
#' * If `.coords` is `NULL`, `.xpf` cell coordinates are used for density estimation and cell values are used as weights.
#' * If coordinates are supplied, coordinates are re-expressed on `.xpf` and then used for density estimation. Equal weights are assumed unless specified. Default or supplied weights are normalised to sum to one at each time step. The total weight of each location within time steps is calculated and then these weights are aggregated by location across the whole time series and renomalised. See the internal [`.pf_map_weights()`] function for full details.
#'
#' Cell coordinates are converted to a [`spatstat.geom::ppp()`] object, which is passed, alongside the observation window (`.owin`) and an image of the weights to [`spatstat.explore::density.ppp()`] for the estimation. Weights must sum to one.
#'
#' [`as.im.SpatRaster`] and [`as.owin.SpatRaster`] are helper functions that convert a [`SpatRaster`] to a pixel image and an observation window (see [`spatstat.geom::owin()`]). The former is based on `maptools::as.im.RasterLayer()`. The latter either defines a rectangular window, if there are no NAs on `.xpf`, or converts `.xpf` directly to an `owin` object. Gridded observation windows, especially if high resolution, considerably slow down density estimation and may exhaust vector memory. Use rectangular windows, or convert `sf` objects to polygon windows (via `spatstat.geom::as.owin()`]) if possible.
#'
#' Coordinates and associated weights are smoothed via [`spatstat.explore::density.ppp()`] into an image. Pixel resolution and smoothing parameters such as bandwidth can be controlled via `...` arguments which are passed directly to this function. The output is translated into a gridded probability density surface (on the geometry defined by `.xpf`).
#'
#' @return The function returns a normalised [`SpatRaster`] (or `NULL` if [`spatstat.explore::density.ppp()`] fails and `.use_tryCatch = TRUE`).
#'
#' @example man/examples/pf_map_dens-examples.R
#'
#' @seealso
#' * The PF (forward simulation) is implemented by [`pf_forward()`];
#'
#' * PF is supported by:
#'     * Setup helpers, namely [`pf_setup_files()`];
#'
#' * The backward pass is implemented by [`pf_backward()`];
#'
#' * Movement paths are built from PF outputs via `pf_path()` functions:
#'     * [`pf_path()`] reconstructs paths;
#'     * [`pf_path_pivot()`] supports path reconstruction;
#'
#' * To reconstruct maps of space use, see:
#'     * [`pf_coords()`] to extract particle coordinates;
#'     * [`pf_map_pou()`] for probability-of-use maps;
#'     * [`pf_map_dens()`] for smooth utilisation distributions;
#'     * [`get_hr()`] for home range estimates;
#'
#' @author Edward Lavender
#' @name pf_map_dens

#' @rdname pf_map_dens
#' @export

as.im.SpatRaster <- function(.xpf) {
  # Check user inputs
  rlang::check_installed("spatstat.geom")
  if (!terra::hasValues(.xpf))
    abort("The SpatRaster is empty.")
  if (terra::is.rotated(.xpf)) {
    abort("The SpatRaster is rotated.")
  }
  # Coerce SpatRaster
  rs <- terra::res(.xpf)
  # Define xmin and ymin (shifted to the cell centre)
  # orig <- sp::bbox(.xpf)[, 1] + 0.5 * rs
  orig <- terra::ext(.xpf)[c(1, 3)] + 0.5 * rs
  dm <- dim(.xpf)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(rs[1], dm[1] - 1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(rs[2], dm[2] - 1))))
  val <- terra::values(.xpf)
  dim(val) <- dm
  val <- spatstat.geom::transmat(val, from = list(x = "-i", y = "j"), to = "spatstat")
  spatstat.geom::im(val, xcol = xx, yrow = yy)
}

#' @rdname pf_map_dens
#' @export

as.owin.SpatRaster <- function(.xpf, .im = NULL) {
  if (terra::global(.xpf, function(x) any(is.na(x)))[1, 1]) {
    # Define the window based on rim if there are NAs
    msg("Observation window is gridded.")
    if (is.null(.im)) {
      .im <- as.im.SpatRaster(.xpf)
    }
    rwin <- spatstat.geom::as.owin(.im)
  } else {
    # Define the window based on the extent otherwise (for improved speed)
    message("Observation window is rectangular.")
    ext  <- terra::ext(.xpf)
    rwin <- spatstat.geom::as.owin(W = c(xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4]))
  }
  rwin
}

#' @rdname pf_map_dens
#' @export

pf_map_dens <- function(.xpf,
                    .im = NULL, .owin = NULL,
                    .coord = NULL,
                    .plot = TRUE,
                    .use_tryCatch = TRUE,
                    .verbose = TRUE, .txt = "",
                    ...) {

  #### Check user inputs
  # Check packages
  t_onset <- Sys.time()
  rlang::check_installed("spatstat.explore")
  rlang::check_installed("spatstat.geom")
  # Check `.xpf`
  check_inherits(.xpf, "SpatRaster")
  # Check dots (`at` and `se` are not currently supported)
  check_dots_allowed(c("at", "se"))
  check_dots_for_missing_period(formals(), list(...))

  #### Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_map_dens() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_map_dens() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Process SpatRaster
  # spatstat assumes planar coordinates
  cat_to_cf("... Processing `.xpf`...")
  crs <- terra::crs(.xpf)
  if (is.na(crs)) {
    abort("`terra::crs(.xpf)` must be specified (and should be planar).")
  }
  terra::crs(.xpf) <- NA
  # Define pixel image & window
  # * The pixel image represents the study area
  # * We need this to define the observation window, which must be based on .xpf
  # * If `.coord` is NULL, we will also use the pixel image for the estimation
  # * But if `.coord` is supplied, we need to redefine the image used for estimation
  if (is.null(.im)) {
    .im <- as.im.SpatRaster(.xpf)
  }
  if (is.null(.owin)) {
    .owin <- as.owin.SpatRaster(.xpf, .im = .im)
  }

  #### Get ppp
  cat_to_cf("... Building `ppp` object...")

  ## (A) Define coordinates & weights from the SpatRaster (e.g., POU grid)
  if (is.null(.coord)) {
    cat_to_cf("... ... Using `.xpf`...")
    .coord <- terra::as.data.frame(.xpf, xy = TRUE)
    colnames(.coord) <- c("x", "y", "mark")
    .coord <- .coord[which(!is.na(.coord$mark) & .coord$mark != 0), ]
    marks <- .coord[, 3]
    if (!isTRUE(all.equal(sum(marks), 1))) {
      abort("Weights on `.xpf` should sum to one since `.coord` = NULL.")
    }
  } else {

    ## (B) Define coordinates & weights from `.coord` input
    # i) Define coordinates data.table with x and y columns
    cat_to_cf("... ... Using `.coord`...")
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
    # ii) Define cell IDs (if un-supplied) for .pf_map_weights()
    if (is.null(.coord$cell_id)) {
      cell_id <- NULL
      .coord[, cell_id := terra::cellFromXY(.xpf, cbind(.coord$x, .coord$y))]
    }
    # iii) Define `.coord` with weights for each cell_id
    .coord <- .pf_map_weights(.coord)
    # iv) Define weights on raster image
    marks <- .coord$mark
    .im <- terra::rasterize(x = as.matrix(.coord[, c("x", "y"), drop = FALSE]),
                            y = .xpf,
                            values = marks)
    .im <- as.im.SpatRaster(.im)
  }

  ## Build ppp object
  cat_to_cf("... ... Defining `ppp` object...")
  rppp <- spatstat.geom::ppp(x = .coord$x, y = .coord$y,
                             window = .owin, marks = marks)
  if (rppp$n == 0L) {
    abort("There are no valid points within the observation window (perhaps you need to invert this?)")
  }

  #### Estimate density surface
  # Get intensity (expected number of points PER UNIT AREA)
  cat_to_cf("... Estimating density surface...")
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
  cat_to_cf("... Scaling density surface...")
  terra::crs(.xpf) <- crs
  dens <- terra::rast(dens)
  terra::crs(dens) <- crs
  dens <- dens * terra::cellSize(dens, unit = "m")
  # Translate expect counts into proportion of points per pixel
  dens <- dens / terra::global(dens, "sum", na.rm = TRUE)[1, 1]
  if (!terra::compareGeom(dens, .xpf, stopOnError = FALSE, messages = FALSE)) {
    cat_to_cf("... ... Resampling density surface onto `.xpf`...")
    dens <- terra::resample(dens, .xpf, method = "near")
    # dens <- terra::mask(dens, .xpf)
    dens <- dens / terra::global(dens, "sum", na.rm = TRUE)[1, 1]
  }
  stopifnot(all.equal(1, terra::global(dens, "sum", na.rm = TRUE)[1, 1]))

  #### Plot density
  if (.plot) {
    ext <- terra::ext(.xpf)
    terra::plot(dens, xlim = ext[1:2], ylim = ext[3:4])
  }

  # Return map
  dens

}
