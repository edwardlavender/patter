#' @title Julia: set objects in Julia
#' @description These are internal functions that assign objects to variable names in Julia.
#' @author Edward Lavender
#' @name julia_set

#' @rdname julia_set
#' @keywords internal

# Set the `env` variable in Julia
set_map <- function(x) {
  stopifnot(inherits(x, "SpatRaster"))
  file <- terra::sources(x)
  if (file == "") {
    file <- tempfile(fileext = ".tif")
    terra::writeRaster(x, file)
  }
  julia_command(glue('env = GeoArrays.read("{file}")'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set the vector of initial states (xinit) in Julia
# * This is required to simulate movement paths & for the particle filter
set_initial_states <- function(.state = "StateXY",
                               .xinit = NULL,
                               .map = NULL,
                               .n = 100L) {

  #### If unprovided, sample `.xinit`
  if (is.null(.xinit)) {
    x <- y <- z <- angle <- NULL
    # Define initial coordinates (x, y)
    .xinit <-
      .map |>
      terra::spatSample(size = .n,
                        method = "random",
                        na.rm = TRUE,
                        xy = TRUE) |>
      as.data.table()
    colnames(.xinit) <- c("x", "y", "z")
    # Define additional state dimensions (as required)
    # * User defined State structures require specification of `.xinit` & a `Patter.jget_xinit()` method
    if (.state == "StateXY") {
      .xinit <- .xinit[, list(x, y)]
    } else if (.state == "StateXYZD") {
      .xinit[, z := z * runif(.N)]
      .xinit[, angle := runif(.N) * 2 * pi]
    } else {
      abort("For custom states, you need to provide `.xinit`.")
    }

    ### If provided, re-sample `.xinit` .`n` times (if required)
  } else {
    check_inherits(.xinit, "data.frame")
    if (nrow(.xinit) != .n) {
      .xinit <- .xinit[sample.int(.N, size = .n, replace = TRUE)]
    }
  }

  #### Export `.xinit` to Julia
  julia_assign("xinit_df", .xinit)
  julia_command(glue('xinit = Patter.jget_xinit({.state}, xinit_df);'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a movement mode ('move') in Julia
set_move <- function(cmd) {
  julia_command(glue('move = {cmd};'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set simulated path(s) in Julia
set_path <- function(.n_step) {
  julia_command(glue('path = sim_walk(xinit = xinit, move = move, nt = {.n_step});'))
  nothing()
}
