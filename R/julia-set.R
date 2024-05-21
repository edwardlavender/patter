#' @title Julia: set objects in Julia
#' @description These are functions that assign objects to variable names in Julia.
#' @details
#' The following functions are exported:
#' * [`set_seed()`] sets a seed in both `R` and `Julia`.
#'    - It is often a good idea to call [`set_seed()`] at the start of your workflow.
#' * [`set_map()`] exports a [`SpatRaster`] map of the study area to a variable named `env` in `Julia` (see [`ModelMove`]).
#'    - Export the map at the start of your workflow.
#'
#' Other functions are internal.
#'
#' @return Exported functions return `invisible(NULL)`.
#' @example man/example/example-julia_set.R
#' @author Edward Lavender
#' @name julia_set

#' @rdname julia_set
#' @export

# Set a seed in R and Julia
set_seed <- function(.seed = 123L) {
  set.seed(.seed)
  if (julia_works(.action = warn)) {
    julia_command(glue('Random.seed!({.seed});'))
  }
  nothing()
}

#' @rdname julia_set
#' @export

# Set the map (`env`) in Julia
# * `env` is the name used by move_*() functions
set_map <- function(.x) {
  stopifnot(inherits(.x, "SpatRaster"))
  file <- terra::sources(.x)
  if (file == "") {
    file <- tempfile(fileext = ".tif")
    terra::writeRaster(.x, file)
  }
  julia_command(glue('env = GeoArrays.read("{file}");'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set Julia threads
set_threads <- function(.threads) {
  if (.threads != "auto" &&
      Sys.getenv("JULIA_NUM_THREADS") != "" &&
      Sys.getenv("JULIA_NUM_THREADS") != .threads) {
    warn("Restart R to update the number of threads in Julia.")
  }
  Sys.setenv(JULIA_NUM_THREADS = .threads)
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
set_move <- function(.cmd) {
  julia_command(glue('move = {.cmd};'))
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
  set_model_types(.models)
  julia_command('yobs = assemble_yobs(datasets, model_types);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Run the particle filter in Julia
# * This defines a `pf_forward` or `pf_backward` object depending on `.direction`
set_pf_filter <- function(.n_move, .n_resample, .n_record, .direction) {
  # Check inputs
  julia_check_exists("timeline", "xinit", "yobs", "move")
  .n_move     <- as.integer(.n_move)
  .n_resample <- paste0(as.integer(.n_record), ".0")
  .n_record   <- as.integer(.n_record)
  # Define output name
  output <- name_particles(.fun = "pf_filter", .direction = .direction)
  # Run the filter
  julia_command(
    glue(
      '
      {output} = particle_filter(timeline = timeline,
                                 xinit = xinit,
                                 yobs = yobs,
                                 move = move,
                                 n_move = {.n_move},
                                 n_record = {.n_record},
                                 n_resample = {.n_resample},
                                 direction = "{.direction}");
    '
    )
  )
  invisible(output)
}

# Set the box within which movements are always valid
# * Required for pf_smoother_*()
set_mobility_box <- function(.box) {
  if (is.null(.box)) {
    julia_command("box = nothing;")
  } else {
    julia_assign("box", .box)
    julia_command("box = Patter.ext(box);")
  }
  nothing()
}

# Run the two-filter smoother in Julia
set_smoother_two_filter <- function(.nMC) {
  output <- name_particles(.fun = "pf_smoother_two_filter")
  fwd    <- name_particles(.fun = "pf_filter", .direction = "forward")
  bwd    <- name_particles(.fun = "pf_filter", .direction = "backward")
  .nMC   <- as.integer(.nMC)
  julia_check_exists(fwd, bwd, "move", "box")
  cmd    <- glue('{output} = two_filter_smoother(xfwd = {fwd}.state, xbwd = {bwd}.state, move = move, box = box, nMC = {.nMC});')
  julia_command(cmd)
  nothing()
}
