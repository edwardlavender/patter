#' @title `Julia`: set objects in `Julia`
#' @description These are functions that assign objects to variable names in `Julia`.
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

# Set a movement model (`model_move`) in Julia
set_model_move <- function(.model_move) {
  julia_command(glue('model_move = {.model_move};'))
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
  julia_check_exists("xinit", "model_move", "timeline")
  julia_command(glue('paths = simulate_path_walk(xinit = xinit, move = model_move, timeline = timeline);'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of model type strings in Julia
# * Required for `set_yobs_*()` via `set_model_obs()`
set_model_obs_types <- function(.model_obs) {
  julia_assign("model_obs_strings", .model_obs)
  julia_command('model_obs_types = Patter.julia_get_model_types(model_obs_strings);')
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of ModelObs structures (`models`) in Julia
# * Required for `set_yobs_*()`
set_model_obs <- function(.model_obs) {
  set_model_obs_types(.model_obs)
  julia_check_exists("model_obs_pars", "model_obs_types")
  julia_command('models = Patter.julia_get_models(model_obs_pars, model_obs_types);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of sensor parameter DataFrame(s) in Julia
# * Required for `set_yobs_via_sim()`
set_model_obs_pars <- function(.model_obs_pars) {
  check_inherits(.model_obs_pars, "list")
  .model_obs_pars <- unname(.model_obs_pars)
  julia_assign("model_obs_pars", .model_obs_pars)
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
set_yobs_via_datasets <- function(.datasets, .model_obs) {
  set_datasets(.datasets)
  set_model_obs_types(.model_obs)
  julia_command('yobs = assemble_yobs(datasets, model_obs_types);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Run the particle filter in Julia
# * This defines a `pff` or `pfb` object depending on `.direction`
set_pf_filter <- function(.n_move, .n_resample, .n_record, .direction) {
  # Check inputs
  julia_check_exists("timeline", "xinit", "yobs", "model_move")
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
                                 move = model_move,
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
set_smoother_two_filter <- function(.n_particle, .n_sim) {
  output <- name_particles(.fun = "pf_smoother_two_filter")
  fwd    <- name_particles(.fun = "pf_filter", .direction = "forward")
  bwd    <- name_particles(.fun = "pf_filter", .direction = "backward")
  # Define the number of particles for the smoother
  # * If NULL, we use all particles from the forward simulation
  if (is.null(.n_particle)) {
    .n_particle <- julia_eval(glue('size({fwd}.state)[1]'))
  }
  .n_particle <- as.integer(.n_particle)
  .n_sim      <- as.integer(.n_sim)
  # Run smoother
  julia_check_exists("timeline", fwd[], bwd, "model_move", "box")
  cmd    <- glue('{output} = two_filter_smoother(timeline = timeline,
                                                 xfwd = {fwd}.state[1:{.n_particle}, :],
                                                 xbwd = {bwd}.state[1:{.n_particle}, :],
                                                 move = model_move,
                                                 box = box,
                                                 nMC = {.n_sim});')
  julia_command(cmd)
  invisible(output)
}
