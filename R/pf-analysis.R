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
#' @author Edward Lavender
#' @export

pf_coords <- function(.history, .bathy, .obs = NULL, .cols = NULL) {

  # Check user inputs & read history files (if required)
  if (inherits(.history[[1]], "data.frame")) {
    check_names(.history[[1]], "cell_now")
  } else {
    .history <-
      .history |>
      lapply(arrow::read_parquet)
  }
  .pf_path_pivot_checks(.obs, .cols)

  # Define time steps
  timestep <- NULL
  for (i in seq_len(length(.history))) {
    .history[[i]][, timestep := i]
  }

  # Define particle coordinates
  p <-
    .history |>
    rbindlist() |>
    dplyr::rename(cell_id = .data$cell_now) |>
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


#' @title PF: path reconstruction
#' @description This function implements the path-reconstruction algorithm.
#' @param .history Particle samples from the particle filter, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object. This must contain columns that define cell samples at each time step (`cell_now`) alongside previous samples (`cell_past`).
#' * An ordered list of file paths (from [`pf_setup_files()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .bathy (optional) If `.return = "long"`, a bathymetry [`SpatRaster`] can be supplied to define cell coordinates (see [`pf_path_pivot()`]).
#' @param .obs,.cols (optional) If `.return = "long"`, `.obs` and `.cols` are a [`data.table`] and a `character` vector of column names in `.obs` to match onto the output (see [`pf_path_pivot()`]).
#' @param .verbose,.txt Arguments to monitor function progress (see [`pf_forward()`]).
#' @param .return A `character` that defines the return format:
#' * `long` specifies a long-format [`data.table`] that defines path IDs, time steps and associated locations (see [`pf_path_pivot()`]).
#' * `wide` specifies a wide-format [`data.table`], with:
#'    * one row for each path;
#'    * one column for each time step (named `x1`, `x2`, etc.);
#'
#' @details The path reconstruction algorithm 'chains' sequential particle samples into movement paths. This function evolved from [`pf_simplify()`](https://edwardlavender.github.io/flapper/reference/pf_simplify.html) in the [`flapper`](https://github.com/edwardlavender/flapper) package. This implementation uses the fast [`collapse::join()`] function.
#'
#' @return The function returns a long- or wide-format [`data.table`] (see `.return`).
#'
#' @example man/examples/pf_path-examples.R
#'
#' @seealso
#' * [`pf_forward()`] and [`pf_backward()`] implement particle filtering.
#' * [`pf_pou()`] maps probability-of-use from particle samples.
#' * [`pf_path()`] builds movement paths from particle samples and [`pf_path_pivot()`] converts wide-format paths into long-format.
#' @author Edward Lavender
#' @export

pf_path <- function(.history,
                    .bathy = NULL, .obs = NULL, .cols = NULL,
                    .verbose = TRUE, .txt = "",
                    .return = c("long", "wide")){

  # Check user inputs
  t_onset <- Sys.time()
  check_inherits(.history, "list")
  rlang::check_installed("collapse")
  .return <- match.arg(.return)
  if (.return == "long") {
    .pf_path_pivot_checks(.obs, .cols)
  }

  # Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_path() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_path() call ended (@ ", Sys.time(), ").")), add = TRUE)

  # Set up chain
  cat_to_cf("... Setting up...")
  if (inherits(.history[[1]], "data.frame")) {
    check_names(.history[[1]], c("cell_past", "cell_now"))
    read <- FALSE
  } else {
    read <- TRUE
  }

  # Define history[[1]]
  cat_to_cf("... Processing history[[1]]...")
  if (read) {
    .history[[1]] <- arrow::read_parquet(.history[[1]])
  }
  .history[[1]] <-
    .history[[1]] |>
    select(x0 = "cell_past", x1 = "cell_now")

  # Define chain text
  cat_to_cf("... Defining chain text...")
  txt <- .pf_path_chain(.history, .read = read)

  # Implement chain
  cat_to_cf("... Evaluating chain text...")
  .pb <- progress::progress_bar$new(total = length(.history))
  .pb$tick(0)
  paths <- eval(parse(text = txt))
  paths$x0 <- NULL

  # Reorientate paths (long format)
  if (.return == "long") {
    cat_to_cf("... Reorientating matrix via pf_path_pivot()...")
    paths <- pf_path_pivot(paths, .bathy = .bathy,
                           .obs = .obs, .cols = .cols)
  }

  # Return outputs
  paths
}


#' @title PF: pivot path matrices
#' @description This function converts paths in 'wide format' to paths in 'long format'.
#'
#' @param .mat A wide-format [`data.table`], from [`pf_path()`] with `.return = "wide"`, in which:
#' * columns (`x1`, `x2`, etc.) represent time steps;
#' * rows represent paths;
#' @param .bathy (optional) The bathymetry [`SpatRaster`] (used to extract cell coordinates).
#' @param .obs,.cols (optional) A [`data.table`] and a `character` vector of column names in `.obs` to match onto the output. `.obs` must contain a `timestep` column for matching.
#'
#' @details
#' This function uses the fast [`collapse::pivot()`] function.
#'
#'
#' @return The function returns a [`data.table`] with at least three columns:
#' * `path_id`---An `integer` that defines each path (row in `.mat`);
#' * `timestep`---An `integer` that defines each time step (column in `.mat`);
#' * `cell_id`---An integer that defines each cell (value in `.mat`);
#'
#' If `.bathy` is supplied, the [`data.table`] contains three additional columns:
#' * `cell_x`---A `double` that defines the cell's x coordinate;
#' * `cell_y`---A `double` that defines the cell's y coordinate;
#' * `cell_z`---A `double` that defines the cell's z coordinate;
#'
#' If `.obs` is supplied, the output also contains any columns specified in `.cols`.
#'
#' @examples
#' # Define a hypothetical set of paths in wide-format
#' require(data.table)
#' mat <- matrix(1:25, ncol = 5)
#' mat <- as.data.table(mat)
#' colnames(mat) <- c("x1", "x2", "x3", "x4", "x5")
#'
#' # Convert to long format
#' pf_path_pivot(mat)
#'
#' @author Edward Lavender
#' @export

pf_path_pivot <- function(.mat, .bathy = NULL,
                          .obs = NULL, .cols = NULL) {
  # Check user inputs
  .pf_path_pivot_checks(.obs, .cols)
  # Pivot paths
  p <-
    .mat |>
    collapse::pivot() |>
    as.data.table() |>
    mutate(timestep = as.integer(rep(seq_len(ncol(.mat)), each = nrow(.mat))),
           path_id = as.integer(rep(seq_len(nrow(.mat)), ncol(.mat)))) |>
    select("path_id", "timestep", cell_id = "value") |>
    arrange(.data$path_id, .data$timestep) |>
    as.data.table()
  # Add cell coordinates & depth
  if (!is.null(.bathy)) {
    p <-
      p |>
      mutate(cell_x = terra::xFromCell(.bathy, .data$cell_id),
             cell_y = terra::yFromCell(.bathy, .data$cell_id),
             cell_z = terra::extract(.bathy, .data$cell_id)) |>
      as.data.table()
  }
  # Add columns from .obs by matching by timestep
  if (!is.null(.obs)) {
    for (col in .cols) {
      p[, (col) := .obs[[col]][match(p$timestep, .obs$timestep)]]
      if (any(is.na(p[[col]]))) {
        warn("There are NAs in the {col} column in the output.",
             .envir = environment())
      }
    }
  }
  # Return outputs
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
#' @example man/examples/pf-pou-examples.R
#' @author Edward Lavender
#' @export

pf_pou <-
  function(.history, .bathy, .plot = TRUE, ...) {

    #### Check user inputs
    check_inherits(.history, c("character", "list"))

    #### Tabulate cell frequencies
    # Prepare samples from parquet files
    if (inherits(.history, "character")) {
      check_dir(.history)
      if (!all((list.files(.history) |> tools::file_ext()) == "parquet")) {
        abort("`.history` contains non parquet files.")
      }
      samples <-
        arrow::open_dataset(.history, format = "parquet") # |> arrow::to_duckdb()
    } else {
      # Prepare samples from pf_*() list
      check_names(.history[[1]], req = "cell_now")
      samples <- .history |> rbindlist()
    }
    # Calculate POU from samples
    pou <-
      samples |>
      dplyr::select(cell = "cell_now") |>
      dplyr::count(.data$cell) |>
      dplyr::collect() |>
      dplyr::mutate(pr = .data$n / sum(.data$n))

    #### Build SpatRaster
    map <- terra::setValues(.bathy, 0)
    map <- terra::mask(map, .bathy)
    map[pou$cell] <- pou$pr
    if (.plot) {
      terra::plot(map, ...)
    }

    #### Return outputs
    map

  }

#' @title PF: map point density
#' @description This function creates a smoothed density map (e.g., of particle samples).
#' @param .xpf A [`SpatRaster`] that defines the grid for density estimation and, if `.coord = NULL`, the points (and associated weights) that are smoothed. The coordinate reference system of `.xpf` must be planar and specified.
#' @param .coord (optional) A two-column `matrix` of x and y planar coordinates. Coordinates are assumed to have equal weight (see Details).
#' @param .plot A `logical` variable that defines whether or not to plot the output.
#' @param .use_tryCatch A `logical` variable that controls error handling:
#' * If `.use_tryCatch = FALSE`, if density estimation fails with an error, the function fails with the same error.
#' * If `.use_tryCatch = TRUE`, if density estimation fails with an error, the function produces a warning with the error message and returns `NULL`.
#' @param .verbose,.txt Controls on function prompts and messages (see [`acs()`]).
#' @param ... Arguments passed to [`spatstat.explore::density.ppp()`], such as `sigma` (i.e., the bandwidth).
#'
#' @details This function smooths (a) a [`SpatRaster`] or (b) a set of inputted coordinates:
#' * If `.coords` is `NULL`, `.xpf` cell coordinates are used for density estimation and cell values are used as weights.
#' * If coordinates are supplied, coordinates are re-expressed on `.xpf` and then used for density estimation. Equal weights are assumed, unless there are duplicate coordinates in which case weights are defined in line with the relative frequency of coordinate pairs.
#'
#' Coordinates and associated weights are smoothed via [`spatstat.explore::density.ppp()`] into an image. Smoothing parameters such as bandwidth can be controlled via `...` arguments which are passed directly to this function. The output is translated into a gridded probability density surface (on the geometry defined by `.xpf`).
#'
#' @return The function returns a normalised [`SpatRaster`] (or `NULL` if [`spatstat.explore::density.ppp()`] fails and `.use_tryCatch = TRUE`).
#'
#' @example man/examples/pf_dens-examples.R
#'
#' @author Edward Lavender
#' @export

pf_dens <- function(.xpf,
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

  #### Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_dens() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_dens() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Process SpatRaster
  # spatstat assumes planar coordinates
  cat_to_cf("... Processing `.xpf`...")
  crs <- terra::crs(.xpf)
  if (is.na(terra::crs(.xpf))) {
    abort("`terra::crs(.xpf)` must be specified (and should be planar).")
  }
  terra::crs(.xpf) <- NA
  # Set up window
  cat_to_cf("... Converting `.xpf` to an `im` object...")
  rim  <- as.im.SpatRaster(.xpf)
  cat_to_cf("... Defining the window...")
  if (terra::global(.xpf, function(x) any(is.na(x)))[1, 1]) {
    # Define the window based on rim if there are NAs
    cat_to_cf("... ... Using `.xpf`...")
    rwin <- spatstat.geom::as.owin(rim)
  } else {
    # Define the window based on the extent otherwise (for improved speed)
    cat_to_cf("... ... Using `.xpf` extent only...")
    ext  <- terra::ext(.xpf)
    rwin <- spatstat.geom::as.owin(W = c(xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4]))
  }

  #### Get ppp
  cat_to_cf("... Building `ppp` object...")
  # (A) Define coordinates & weights from the SpatRaster
  if (is.null(.coord)) {
    cat_to_cf("... ... Using `.xpf`...")
    .coord <- terra::as.data.frame(.xpf, xy = TRUE)
    colnames(.coord) <- c("x", "y", "mark")
    marks <- .coord[, 3]
  } else {
    # (B) Define coordinates & weights from `.coord` input
    cat_to_cf("... ... Using `.coord`...")
    if (inherits(.coord, "matrix")) {
      .coord <- as.data.frame(.coord[, 1:2])
      colnames(.coord) <- c("x", "y")
    }
    check_inherits(.coord, "data.frame")
    if (ncol(.coord) < 2L) {
      abort("`.coord` should contain x and y coordinates.")
    }
    if (ncol(.coord) > 2L) {
      warn("The first two columns in `.coord` have been taken as x and y coordinates.")
      .coord <- .coord[, 1:2, drop = FALSE]
    }
    check_names(.coord, req = c("x", "y"))
    # Drop duplicate coordinates & adjust marks (weights) accordingly
    # ... This assumes that coordinates that are uniquely defined on `.coord`
    # ... are uniquely defined on .xpf (see definition of weights below).
    nrw_0 <- nrow(.coord)
    .coord <-
      .coord |>
      mutate(cell = terra::cellFromXY(.xpf, cbind(.data$x, .data$y)),
             mark = rep(1/nrow(.coord), nrow(.coord))) |>
      group_by(.data$cell) |>
      mutate(mark = sum(.data$mark)) |>
      slice(1L) |>
      ungroup() |>
      as.data.frame()
    stopifnot(all.equal(1, sum(.coord$mark)))
    nrw_1 <- nrow(.coord)
    if (nrw_0 != nrw_1) {
      warn("`coords` contains duplicate coordinates on `.xpf` that have been processed.")
    }
    # Define marks/weights
    marks <- .coord$mark
    rim <- terra::rasterize(x = as.matrix(.coord[, c("x", "y"), drop = FALSE]),
                            y = .xpf,
                            values = .coord$mark)
    rim <- as.im.SpatRaster(rim)
  }
  cat_to_cf("... ... Defining `ppp` object...")
  rppp <- spatstat.geom::ppp(x = .coord$x, y = .coord$y,
                             window = rwin, marks = marks)

  #### Estimate density surface
  # Get intensity (expected number of points PER UNIT AREA)
  cat_to_cf("... Estimating density surface...")
  dens <- tryCatch(spatstat.explore::density.ppp(rppp, weights = rim,
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
  dens <- terra::rast(dens)
  terra::crs(dens) <- crs
  dens <- dens * terra::cellSize(dens, unit = "m")
  # Translate expect counts into proportion of points per pixel
  dens <- dens/terra::global(dens, "sum", na.rm = TRUE)[1, 1]
  stopifnot(all.equal(1, terra::global(dens, "sum", na.rm = TRUE)[1, 1]))

  #### Plot density
  if (.plot) {
    ext <- terra::ext(.xpf)
    terra::plot(dens, xlim = ext[1:2], ylim = ext[3:4])
  }

  # Return map
  dens

}
