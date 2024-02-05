#' @title PF: plot particle histories
#' @description This function maps particle histories for selected time steps or entire time series and can be used to create animations.
#'
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$spatial$bathy`, which defines the grid onto which particle samples are mapped;
#' @param .forward Particle samples, provided in any format accepted by [`.pf_history_list()`]. Particle samples should be from [`pf_forward()`] if `.backward` is supplied. `cell_now`, `x_now` and `y_now` columns are required.
#' @param .backward (optional) If .`forward` contains particle samples from the forward run, `.backward` can contain particle samples from the backward pass (e.g., [`pf_backward_killer()`]), provided in any format accepted by [`.pf_history_list()`]. The `cell_now` column is required.
#' @param .steps `NULL` or an `integer` vector of the time steps for which to map particle samples. `NULL` specifies all time steps.
#' @param .png (optional) A named `list`, passed to [`grDevices::png()`], to save plots to file. `filename` should be the directory in which to write files. Files are named `{.steps[1]}.png, {.steps[2]}.png, ..., {.steps[N]}.png`. `.png` should be supplied if `.cl` is supplied via `...`.
#' @param .add_surface,.add_forward Named `list`s for plot customisation.
#' * `.add_surface` is passed to [`terra::plot()`], excluding `x` and `main`.
#' * `.add_forward` is passed to [`graphics::points()`], excluding `x` and `y`.
#' @param .add_layer (optional) A `function` used to add additional layer(s) to the plot. The function must a single (unnamed) `integer` value for the time step (even if ignored). An example function is `function(...) points(x, y)` where `x` and `y` are (for example) receiver coordinates.
#' @param .prompt A `logical` variable that defines whether or not to prompt the user for input between plots. This is only used in interactive mode if `.png = NULL` (and there are multiple time steps).
#' @param ... Additional argument(s) passed to [`cl_lapply()`], such as `.cl`.
#'
#' @details For each `.step`, [`terra::plot()`] is used to plot `.dlist$spatial$bathy`. Particle samples in `.forward` are added onto the grid via [`graphics::points()`]. If `.backward` is supplied, particles in `.forward` should be derived from the forward run. Particles in `.forward` but not `.backward` (i.e., dead ends) are shown in red (unless `.add_forward$col` is overridden).
#'
#' This function replaces [`flapper::pf_plot_history()`](https://edwardlavender.github.io/flapper/reference/pf_plot_history.html).
#'
#' @return The function is called for its side effects. It returns `invisible(TRUE)`.
#' @example man/examples/pf_plot_history-examples.R
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @export

pf_plot_history <- function(.dlist,
                            .forward,
                            .backward = NULL,
                            .steps = NULL,
                            .png = NULL,
                            .add_surface = list(),
                            .add_forward = list(),
                            .add_layer = NULL,
                            .prompt = FALSE, ...
                            ) {

  # Check user inputs
  check_dlist(.dlist = .dlist,
              .spatial = "bathy")

  # Define particle histories & whether or not they need to be read from file
  .forward      <- .pf_history_list(.forward)
  .backward     <- .pf_history_list(.backward)
  forward_read  <- .pf_history_read(.forward)
  backward_read <- .pf_history_read(.backward)

  # Define time steps
  if (is.null(.steps)) {
    .steps <- seq_len(length(.forward))
  }

  # Define plot layers
  .add_surface$x <- .dlist$spatial$bathy
  do_backward    <- !is.null(.backward)
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
  cl_lapply(.steps, ..., .fun = function(t) {

    # Optionally write image to file
    if (do_png) {
      .png$filename <- file.path(sink, paste0(t, ".png"))
      do.call(grDevices::png, .png)
    }

    # Update surface
    .add_surface$main <- paste0("Time ", t)

    # Define particle coordinates for plotting
    fwd <- .pf_history_elm(.history = .forward,
                           .elm = t,
                           .read = forward_read,
                           .cols = c("cell_now", "x_now", "y_now"))
    add_fwd   <- .add_forward
    add_fwd$x <- fwd$x_now
    add_fwd$y <- fwd$y_now
    # Colour dead ends from the forward run in red
    if (do_backward) {
      bwd <- .pf_history_elm(.history = .backward,
                             .elm = t,
                             .read = backward_read,
                             .cols = "cell_now")
      if (is.null(.add_forward$col)) {
        add_fwd$col <- c("red", "black")[(fwd$cell_now %in% bwd$cell_now) + 1]
      }
    }

    # Make plot
    do.call(terra::plot, .add_surface)
    do.call(points, add_fwd)
    if (do_add_layer) {
      .add_layer(t)
    }

    # Save image
    if (do_png) {
      grDevices::dev.off()
    }

    # Prompt user
    if (.prompt && length(.steps) > 1L && is.null(.png) && interactive()) {
      readline(prompt = "Press [enter] to continue or [Esc] to exit...")
    }

    NULL

  })

  invisible(TRUE)
}
