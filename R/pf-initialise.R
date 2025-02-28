#' @title Initialise the particle filter
#' @description This is an internal function that initialises the particle filter:
#' * Initial states are set via [`set_states_init()`];
#' * Observations are set via [`set_yobs_dict()`];
#'
#' @author Edward Lavender
#' @keywords internal

pf_filter_init <- function(.timeline,
                           .state = "StateXY",
                           .xinit = NULL,
                           .model_move,
                           .yobs,
                           .n_particle,
                           .direction,
                           .verbose = getOption("patter.verbose")) {

  #### Initiate
  cats <- cat_setup(.fun = "pf_filter_init", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Simulate initial states
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Setting initial states..."))
  set_states_init(.timeline = .timeline,
                  .state = .state,
                  .xinit = .xinit,
                  .model_move = .model_move,
                  .yobs = .yobs,
                  .n_particle = .n_particle,
                  .direction = .direction)

  #### Set filter observations
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Setting observations dictionary..."))
  set_yobs_dict(.yobs = .yobs)
  nothing()

}
