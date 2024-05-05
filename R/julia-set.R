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
  julia_command(glue('env = GeoArrays.read("{file}");'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set the vector of initial states (`xinit`) in Julia
# * This is required for `sim_path_walk()` and `pf_filter()`
set_states_init <- function(.xinit, .state) {
  # Export initial states to Julia as `xinit`
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

# Set a Vector of model type strings in Julia
# * Required for `set_yobs_*()` via `set_models()`
set_model_types <- function(.models) {
  julia_assign("model_type_strings", .models)
  julia_command('model_types = Patter.julia_get_model_types(model_type_strings);')
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of ModelObs structures (`models`) in Julia
# * Required for `set_yobs_*()`
set_models <- function(.models) {
  set_model_types(.models)
  julia_check_exists("parameters", "model_types")
  julia_command('models = Patter.julia_get_models(parameters, model_types);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of sensor parameter DataFrame(s) in Julia
# * Required for `set_yobs_via_sim()`
set_parameters <- function(.parameters) {
  check_inherits(.parameters, "list")
  .parameters <- unname(.parameters)
  julia_assign("parameters", .parameters)
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a dictionary of observations (`yobs`) via `simulate_obs()` in Julia
set_yobs_via_sim <- function() {
  julia_check_exists("paths", "models", "timeline")
  julia_command('yobs = simulate_yobs(paths = paths, models = models, timeline = timeline);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of datasets DataFrames in Julia
# * Required for `set_yobs_via_datasets()`
set_datasets <- function(.datasets) {
  # Check .datasets is a list
  check_inherits(.datasets, "list")
  # Check dataset names & fix time stamps
  timestamp <- NULL
  lapply(.datasets, function(.dataset) {
    # Check required names
    check_names(.dataset, c("timestamp", "sensor_id", "obs"))
    # Check rows
    if (fnrow(.dataset) == 0L) {
      abort("There are no rows in the dataset.")
    }
    # Format time stamps (by reference) for correct export to Julia
    .dataset[, timestamp := julia_timeline(timestamp)]
  })
  # Export datasets
  .datasets <- unname(.datasets)
  julia_assign("datasets", .datasets)
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a dictionary of observations (`yobs`) using real datasets in Julia
set_yobs_via_datasets <- function(.datasets, .models) {
  set_datasets(.datasets)
  set_models(.models)
  julia_command('yobs = assemble_yobs(datasets, models);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Run the particle filter in Julia
set_particles <- function(.n_move, .n_resample, .n_record) {
  # Check inputs
  julia_check_exists("timeline", "xinit", "yobs", "move")
  .n_move     <- as.integer(.n_move)
  .n_resample <- as.integer(.n_resample)
  .n_record   <- as.integer(.n_record)
  # Run the filter
  julia_command(
    glue(
      '
      particles = particle_filter(timeline = timeline,
                                  xinit = xinit,
                                  yobs = yobs,
                                  move = move,
                                  n_move = {.n_move},
                                  resample_ess = {.n_resample},
                                  n_record = {.n_record})
    '
    )
  )
  nothing()
}
