#' @title Plots: plot coordinates (`x`, `y`, `t`)
#' @description This function maps `xyt` data (i.e. coordinate (`x`, `y`) locations for selected time steps (`t`) or entire time series) and can be used to create animations.
#'
#' @param .map A [`terra::SpatRaster`] that defines the study area (see [`glossary`]).
#' @param .coord A [`data.table::data.table`] of coordinates, including `x`, `y` and `timestep` columns. Point graphical parameters (`pch`, `col`, `bg`, `cex`, `lwd`, `lty`, `lwd`) can be included as columns to customise point appearance. (Graphical parameters provided here silently overwrite any elements of the same name in `.add_points`.)
#' @param .steps `NULL` or an `integer` vector of the time steps for which to map coordinates (e.g., `.steps = 1:5L`). `NULL` specifies all time steps.
#' @param .png (optional) A named `list`, passed to [`grDevices::png()`], to save plots to file. `filename` should be the directory in which to write files. Files are named `{.steps[1]}.png, {.steps[2]}.png, ..., {.steps[N]}.png`. `.png` should be supplied if `.cl` is supplied via `...`.
#' @param .add_surface,.add_points Named `list`s for plot customisation.
#' * `.add_surface` is passed to [`terra::plot()`], excluding `x` and `main`.
#' * `.add_points` is passed to [`graphics::points()`], excluding `x` and `y`.
#' @param .add_layer (optional) A `function` used to add additional layer(s) to the plot. The function must a single (unnamed) `integer` value for the time step (even if ignored). An example function is `function(...) points(x, y)` where `x` and `y` are (for example) receiver coordinates.
#' @param .prompt A `logical` variable that defines whether or not to prompt the user for input between plots. This is only used in interactive mode if `.png = NULL` (and there are multiple time steps).
#' @param ... Additional argument(s) passed to [`cl_lapply()`], such as `.cl`.
#'
#' @details For each `.step`, [`terra::plot()`] is used to plot `.map`.  Coordinates in `.coord` are added onto the grid via [`graphics::points()`]. (Note that coordinates derived from particle algorithms are equally weighted thanks to resampling.)
#'
#' This function replaces [`flapper::pf_plot_history()`](https://edwardlavender.github.io/flapper/reference/pf_plot_history.html).
#'
#' On Linux, this function cannot be used within a Julia session.
#'
#' @return The function is called for its side effects. It returns `invisible(TRUE)`.
#' @example man/examples/example-plot_xyt.R
#' @seealso
#' Routines that coordinate time series (`xyt` data) include:
#' * [`coa()`], which implements the COA algorithm;
#' * [`pf_filter()`], which implements particle filtering;
#' * [`pf_smoother_two_filter()`], which implements particle smoothing;
#' @author Edward Lavender
#' @export

plot_xyt <- function(.map,
                     .coord,
                     .steps = NULL,
                     .png = NULL,
                     .add_surface = list(),
                     .add_points = list(),
                     .add_layer = NULL,
                     .prompt = FALSE, ...) {

  # Define time steps
  check_names(.coord, c("x", "y", "timestep"))
  if (is.null(.steps)) {
    .steps <- sort(unique(.coord$timestep))
  }

  # Define plot layers
  point_pars     <- c("pch", "col", "bg", "cex", "lwd", "lty", "lwd")
  point_pars     <- point_pars[which(rlang::has_name(.coord, point_pars))]
  do_point_pars  <- ifelse(length(point_pars) > 0L, TRUE, FALSE)
  do_add_layer   <- !is.null(.add_layer)

  # Define png options
  sink   <- NULL
  do_png <- FALSE
  if (!is.null(.png)) {
    check_named_list(.png)
    do_png <- TRUE
    if (!rlang::has_name(.png, "filename")) {
      .png$filename <- getwd()
    }
    sink          <- .png$filename
    .png_defaults <- list(height = 10, width = 10, units = "in", res = 300)
    .png          <- list_merge(.png_defaults, .png)
  }
  # Loop over time steps and make plots
  cl_lapply(.steps, ...,
            .fun = function(t, .chunkargs = .map) {

    # Optionally write image to file
    if (do_png) {
      .png$filename <- file.path(sink, paste0(t, ".png"))
      do.call(grDevices::png, .png)
    }

    # Define layer(s)
    .add_surface$x <- .chunkargs
    .add_surface$main <- paste0("Time ", t)
    pos <- which(.coord$timestep == t)

    .add_points$x <- .coord$x[pos]
    .add_points$y <- .coord$y[pos]
    if (do_point_pars) {
      .add_points[point_pars] <- lapply(point_pars, \(p) .coord[[p]][pos])
    }

    # Make plot
    do.call(terra::plot, .add_surface)
    do.call(points, .add_points)
    if (do_add_layer) {
      .add_layer(t)
    }

    # Save image
    if (do_png) {
      grDevices::dev.off()
    }

    # Prompt user
    if (.prompt && length(.steps) > 1L && t < max(.steps) && is.null(.png) && interactive()) {
      readline(prompt = "Press [enter] to continue or [Esc] to exit...")
    }

    NULL

  })

  invisible(TRUE)
}
