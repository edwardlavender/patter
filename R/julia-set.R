#' @title `Julia`: set objects in `Julia`
#' @description These are internal functions that assign objects to variable names in `Julia`.
#' @author Edward Lavender
#' @name julia_set

#' @rdname julia_set
#' @keywords internal

# Set Julia threads
set_JULIA_NUM_THREADS <- function(JULIA_NUM_THREADS) {
  # Get JULIA_NUM_THREADS
  JULIA_NUM_THREADS <- julia_option(JULIA_NUM_THREADS)
  # On unix, set JULIA_NUM_THREADS = "auto"
  # (Otherwise, Julia is launched with one thread only)
  if (os_unix() & is.null(JULIA_NUM_THREADS)) {
    JULIA_NUM_THREADS <- "auto"
  }
  # On Windows, JULIA_NUM_THREADS should be NULL or set system-wide
  if (os_windows()) {
    if (is.null(JULIA_NUM_THREADS)) {
      msg("On Windows, set `JULIA_NUM_THREADS` system-wide to improve performance (see https://github.com/edwardlavender/patter/issues/11).")
    } else {
      # Verify that JULIA_NUM_THREADs is set system-wide (not only in R)
      # (System-wide variables are captured by Sys.getenv())
      try({
        # Obtain system-wide environment variables via registry
        # Note this check appears to work on some, but not all, systems
        # * Hence we use a message not a warning
        # (system("cmd.exe /c set", intern = TRUE) inherits environment variables from R)
        SET <- system("reg query \"HKLM\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment\" /s", intern = TRUE)
        if (!any(grepl("JULIA_NUM_THREADS", SET))) {
          msg("On Windows, `JULIA_NUM_THREADS` should be set system-wide (see https://github.com/edwardlavender/patter/issues/11).")
        }
      }, silent = TRUE)
    }
  }
  # Update JULIA_NUM_THREADS setting
  # * This is implemented on unix only
  # * On Windows, JULIA_NUM_THREADS is already set if relevant
  if (os_unix() & !is.null(JULIA_NUM_THREADS)) {
    Sys.setenv(JULIA_NUM_THREADS = JULIA_NUM_THREADS)
  }
  invisible(JULIA_NUM_THREADS)
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

# Set the state type e.g., StateXY
set_state_type <- function(.state) {
  check_inherits(.state, "character")
  julia_command(glue('state_type = {.state};'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set xinit (NULL or a DataFrame)
set_xinit <- function(.xinit) {
  if (is.null(.xinit)) {
    julia_command('xinit = nothing;')
  } else {
    julia_assign("xinit", .xinit)
  }
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a movement model (`model_move`) in Julia
set_model_move <- function(.model_move) {
  check_inherits(.model_move, "character")
  julia_command(glue('model_move = {.model_move};'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set simulated path(s) in Julia
set_path <- function() {
  julia_check_exists("xinit", "model_move", "timeline")
  julia_command(glue('paths = simulate_path_walk(xinit = xinit, model_move = model_move, timeline = timeline);'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of model type strings in Julia
# * Required for set_states_init()
set_model_obs_types <- function(.datasets) {
  # Permit missing
  # * This makes it possible to re-run the filter without re-exporting yobs
  # * (see `set_yobs_vect()`)
  if (missing(.datasets)) {
    return(nothing())
  }
  # Otherwise, set .datasets
  check_named_list(.datasets)
  if (length(.datasets) == 0L) {
    julia_command('model_obs_types = nothing;')
  } else {
    model_obs_types <- names(.datasets)
    julia_assign("model_obs_strings", model_obs_types)
    julia_command('model_obs_types = Patter.julia_get_model_obs_types(model_obs_strings);')
  }
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of ModelObs structures (`model_obs`) in Julia
# * Required for sim_observations()
set_model_obs <- function(.model_obs) {
  # Check .model_obs is a named list
  # * names: ModelObs types
  # * elements: ModelObs parameters
  check_named_list(.model_obs)
  # Set ModelObs types
  set_model_obs_types(.model_obs)
  # Set ModelObs parameters
  model_obs_pars <- unname(.model_obs)
  julia_assign("model_obs_pars", model_obs_pars)
  # Define model_obs structures for data simulation
  julia_command('model_obs = Patter.julia_get_model_obs(model_obs_pars, model_obs_types);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a Vector of datasets DataFrames in Julia
# * yobs: missing  -> re-use .yobs already set in Julia
# * yobs: list()   -> set .yobs to nothing
# * yobs: supplied -> set yobs
set_yobs_vect <- function(.timeline, .yobs) {
  if (missing(.yobs)) {
    return(nothing())
  }
  # Check .datasets is a named list
  .yobs <- list_compact(.yobs)
  check_named_list(.yobs)
  if (length(.yobs) == 0L) {
    julia_command('yobs_vect = nothing;')
    return(nothing())
  }
  # Check dataset names & fix time stamps
  timestamp <- NULL
  lapply(.yobs, function(.dataset) {
    # Check required names
    check_names(.dataset, c("timestamp", "sensor_id", "obs"))
    # Check rows
    if (fnrow(.dataset) == 0L) {
      abort("There are no rows in the dataset.")
    }
    # Format time stamps (by reference) for correct export to Julia
    .dataset[, timestamp := julia_timeline(timestamp)]
  })
  # Check time zone
  tzs <- c(tz(.timeline), sapply(.yobs, \(d) tz(d$timestamp)))
  if (length(unique(tzs)) != 1L) {
    abort("There is a mismatch between the time zones of `.timeline` and/or `.yobs` `timestamp`s ({str_items(tzs, .quo = '\"')}).",
          .envir = environment())
  }
  # Export datasets
  .yobs <- unname(.yobs)
  julia_assign("yobs_vect", .yobs)
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a dictionary of observations (`yobs`) using real datasets in Julia
# * yobs: missing  -> re-use .yobs already set in Julia
# * yobs: list()   -> set .yobs to empty Dict
# * yobs: supplied -> set yobs
set_yobs_dict <- function(.yobs) {
  if (missing(.yobs)) {
    return(nothing())
  }
  if (length(.yobs) == 0L) {
    julia_command('yobs = Dict();')
  } else {
    julia_check_exists("model_obs_types", "yobs_vect")
    julia_command('yobs = assemble_yobs(datasets = yobs_vect, model_obs_types = model_obs_types);')
  }
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set a dictionary of observations (`yobs`) via `simulate_obs()` in Julia
set_yobs_dict_via_sim <- function() {
  julia_check_exists("paths", "model_obs", "timeline")
  julia_command('yobs = simulate_yobs(paths = paths, model_obs = model_obs, timeline = timeline);')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

set_n_particle <- function(.n_particle) {
  .n_particle <- as.integer(.n_particle)
  julia_assign("n_particle", .n_particle)
  nothing()
}

#' @rdname julia_set
#' @keywords internal

set_direction <- function(.direction = c("forward", "backward")) {
  .direction <- match.arg(.direction)
  julia_command(glue('direction = "{.direction}";'))
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Simulate states and update xinit in Julia
set_states_init <- function(.timeline, .state, .xinit, .model_move, .yobs, .n_particle, .direction) {

  # Set Julia objects incl. observations
  # set_map()
  set_timeline(.timeline)
  set_state_type(.state)
  set_xinit(.xinit)
  set_model_move(.model_move)
  set_yobs_vect(.timeline = .timeline, .yobs = .yobs)
  set_model_obs_types(.yobs)
  set_n_particle(.n_particle)
  set_direction(.direction)

  # Simulate initial states (DataFrame)
  julia_command(
    '
      xinit_df = simulate_states_init(map = env_init,
                                      timeline = timeline,
                                      state_type = state_type,
                                      xinit = xinit,
                                      model_move = model_move,
                                      datasets = yobs_vect,
                                      model_obs_types = model_obs_types,
                                      n_particle = n_particle,
                                      direction = direction,
                                      output = "DataFrame");
      '
  )

  # Translate initial states (State Vector)
  julia_command('xinit = Patter.julia_get_xinit(state_type, xinit_df);')

  # Return states to R as data.table
  # as.data.table(julia_eval("xinit_df"))
  nothing()

}

#' @rdname julia_set
#' @keywords internal

# Set t_resample
set_t_resample <- function(.t_resample) {
  # Force integer/integer vector
  if (!is.null(.t_resample)) {
    .t_resample <- as.integer(.t_resample)
  }
  # Assign t_resample
  # * NULL            -> nothing
  # * A single number -> Int
  # * A vector        -> Vector{Int}
  julia_assign("t_resample", .t_resample)
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Set batch_fwd, batch_bwd or batch_smo
# * Use different object names to enable smoothing
set_batch <- function(.batch, .type = c("fwd", "bwd", "smo")) {
  # Check file vector
  .type <- match.arg(.type)
  if (!is.null(.batch)) {
    check_dir_exists(dirname(.batch[1]))
    if (length(.batch) != length(unique(.batch))) {
      abort("Each `.batch` element should be a unique `.jld2` file.")
    }
    if (!all(tools::file_ext(.batch) == "jld2")) {
      abort("`.batch` elements should be `.jld2` files.")
    }
    if (any(file.exists(.batch))) {
      warn("Existing `.batch` files will be overwritten.")
    }
  }
  # Set batch Vector{String} or nothing (if .batch = NULL)
  batch_vector <- glue("batch_{.type}")
  julia_assign(batch_vector, .batch)
  if (length(.batch) == 1L) {
    julia_command(glue("{batch_vector} = [{batch_vector}];"))
  }
  invisible(batch_vector)
}

#' @rdname julia_set
#' @keywords internal

set_progress <- function(.progress) {
  .progress <- list_merge(julia_progress(), .progress)
  julia_assign("pb_enabled", .progress$enabled)
  julia_assign("pb_dt", .progress$dt)
  julia_assign("pb_showspeed", .progress$showspeed)
  # Set selected Progress options
  # * output is controlled via Patter.progress_control
  # * color is not supported from R
  # * Other options are not implemented from R
  julia_command('
  progress =
    Patter.progress_control(enabled   = pb_enabled,
                            dt        = pb_dt,
                            showspeed = pb_showspeed);
  ')
  nothing()
}

#' @rdname julia_set
#' @keywords internal

set_verbose <- function(.verbose) {
  julia_assign("verbose", .verbose)
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Run the particle filter in Julia
# * This defines a `fwd` or `bwd` object depending on `.direction`
set_pf_filter <- function(.n_move,
                          .n_resample,
                          .t_resample,
                          .n_record,
                          .n_iter,
                          .direction,
                          .batch,
                          .progress,
                          .verbose) {
  # Check inputs
  julia_check_exists("timeline", "xinit", "yobs", "model_move")
  .n_move     <- as.integer(.n_move)
  .n_resample <- paste0(as.integer(.n_resample), ".0")
  .n_record   <- as.integer(.n_record)
  .n_iter     <- as.integer(.n_iter)
  set_t_resample(.t_resample)
  batch_vector <- set_batch(.batch, .type = ifelse(.direction == "forward", "fwd", "bwd"))
  set_progress(.progress)
  set_verbose(.verbose)
  # Define output name
  output <- name_particles(.fun = "pf_filter", .direction = .direction)
  # Run the filter
  julia_command(
    glue(
      '
      {output} = particle_filter(timeline   = timeline,
                                 xinit      = xinit,
                                 yobs       = yobs,
                                 model_move = model_move,
                                 n_move     = {.n_move},
                                 n_record   = {.n_record},
                                 n_resample = {.n_resample},
                                 t_resample = t_resample,
                                 n_iter     = {.n_iter},
                                 direction  = "{.direction}",
                                 batch      = {batch_vector},
                                 progress   = progress,
                                 verbose    = verbose);
    '
    )
  )
  invisible(output)
}

#' @rdname julia_set
#' @keywords internal

# Set cache for two-filter smoother (true/false)
# * Use julia_assign() to handle T, TRUE, F, FALSE
set_cache <- function(.cache) {
  check_inherits(.cache, "logical")
  julia_assign("cache", .cache)
  nothing()
}

#' @rdname julia_set
#' @keywords internal

# Run the two-filter smoother in Julia
set_smoother_two_filter <- function(.n_particle, .n_sim, .cache, .batch, .progress, .verbose) {

  #### Define output names
  output <- name_particles(.fun = "pf_smoother_two_filter")
  fwd    <- name_particles(.fun = "pf_filter", .direction = "forward")
  bwd    <- name_particles(.fun = "pf_filter", .direction = "backward")

  #### Define inputs to smoother
  .n_particle <- julia_n_particle(.n_particle)
  .n_sim      <- as.integer(.n_sim)
  set_cache(.cache)
  batch_vector <- set_batch(.batch, .type = "smo")
  set_progress(.progress)
  set_verbose(.verbose)
  if (is.null(.batch)) {
    julia_command(glue('xfwd_for_smo = {fwd}.states;')) # {fwd}.states[1:{.n_particle}, :]
    julia_command(glue('xbwd_for_smo = {bwd}.states;')) # {bwd}.states[1:{.n_particle}, :]
  } else {
    julia_command('xfwd_for_smo = batch_fwd;')
    julia_command('xbwd_for_smo = batch_bwd;')
  }

  #### Run smoother
  julia_check_exists("timeline", fwd, bwd, "model_move", "vmap", "cache")
  cmd    <- glue('{output} = particle_smoother_two_filter(timeline   = timeline,
                                                          xfwd       = xfwd_for_smo,
                                                          xbwd       = xbwd_for_smo,
                                                          model_move = model_move,
                                                          vmap       = vmap,
                                                          n_particle = {.n_particle},
                                                          n_sim      = {.n_sim},
                                                          cache      = cache,
                                                          batch      = {batch_vector},
                                                          progress   = progress,
                                                          verbose    = verbose);')
  julia_command(cmd)
  invisible(output)
}
