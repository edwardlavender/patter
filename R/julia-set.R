#' @title Julia: set objects in Julia
#' @description These are internal functions that assign objects to variable names in Julia.
#' @author Edward Lavender
#' @name julia_set

#' @rdname julia_set
#' @keywords internal

# Set the map (`env`) in Julia
# * `env` is the name used by move_*() functions
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

# Set the vector of initial states (`xinit`) in Julia
# * This is required for `sim_path_walk()` and `pf_filter()`
set_states_init <- function(.state = "StateXY",
                            .map = NULL,
                            .xinit = NULL,
                            .n = 100L) {

  #### If un-provided, sample `.xinit`
  if (is.null(.xinit)) {

    # Check user inputs
    if (is.null(.map)) {
      abort("`.map` is required if `.xinit` is NULL.")
    }
    if (!(.state %in% c("StateXY", "StateXYZD"))) {
      abort("For custom states, you need to provide `.xinit`.")
    }

    # Define initial coordinates (x, y)
    map_value <- x <- y <- z <- angle <- NULL
    .xinit <-
      .map |>
      terra::spatSample(size = .n,
                        method = "random",
                        na.rm = TRUE,
                        xy = TRUE,
                        values = TRUE) |>
      as.data.table()
    colnames(.xinit) <- c("x", "y", "map_value")
    .xinit <- .xinit[, list(map_value, x, y)]

    # Define additional state dimensions (as required)
    # * User defined State structures require specification of `.xinit` & a `Patter.julia_get_xinit()` method
    if (.state == "StateXYZD") {
      .xinit[, z := map_value * runif(.N)]
      .xinit[, angle := runif(.N) * 2 * pi]
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
  julia_command(glue('xinit = Patter.julia_get_xinit({.state}, xinit_df);'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a movement model (`move`) in Julia
set_move <- function(cmd) {
  julia_command(glue('move = {cmd};'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a timeline in Julia
set_timeline <- function(.timeline) {
  .timeline <- julia_timeline(.timeline)
  julia_assign("timeline", .timeline)
  nothing()
}


#' @rdname julia_set
#' @keywords internal

# Set simulated path(s) in Julia
set_path <- function() {
  julia_check_exists("xinit", "move", "timeline")
  julia_command(glue('paths = simulate_path_walk(xinit = xinit, move = move, timeline = timeline);'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of sensor parameter DataFrame(s) in Julia
# * Required for `sim_yobs()`
set_parameters <- function(.parameters) {
  check_inherits(.parameters, "list")
  .parameters <- unname(.parameters)
  julia_assign("parameters", .parameters)
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of model type strings in Julia
# * Required for `sim_yobs()` via `set_models()`
set_model_types <- function(.models) {
  julia_assign("model_type_strings", .models)
  julia_command('model_types = Patter.julia_get_model_types(model_type_strings);')
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of ModelObs structures (`models`) in Julia
# * Required for `sim_yobs()`
set_models <- function(.models) {
  set_model_types(.models)
  julia_check_exists("parameters", "model_types")
  julia_command('models = Patter.julia_get_models(parameters, model_types);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Simulate a dictionary of observations (`yobs`) in Julia
set_yobs <- function() {
  julia_check_exists("paths", "models", "timeline")
  julia_command('yobs = simulate_yobs(paths = paths, models = models, timeline = timeline);')
  nothing()
}
