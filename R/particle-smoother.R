#' @title Two-filter smoother
#' @description This function implements the two-filter smoother.
#' @param .map,.mobility (optional) 'Mobility box' arguments.
#' @param .nMC An `integer` that defines the number of Monte Carlo simulations.
#' @param .verbose .verbose User output control (see [`patter-progress`] for supported options).
#' @details This function wraps [`Patter.two_filter_smoother()`](https://edwardlavender.github.io/Patter.jl/).
#' @returns
#'
#' @inherit assemble seealso
#' @author Edward Lavender
#' @export

pf_smoother_two_filter <- function(.map = NULL,
                                   .mobility = .map,
                                   .nMC = 100L,
                                   .verbose = getOption("patter.verbose")) {

  #### Set up messages
  t_onset <- Sys.time()
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_smoother_two_filter", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_smoother_two_filter", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Define smoother arguments
  cat_log(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Setting smoother arguments..."))
  # two_filter_smoother(xfwd, xbwd, move, box, nMC)
  # * xfwd, xbwd and move exist in Julia
  # * box and nMC are defined below

  #### (optional) Define mobility box
  if (!is.null(.mobility) & !is.null(.map)) {
    # `.mobility` can be provided to define a box within which movements are always valid for 2D states
    pf_obj <- name_particles(.fun = "pf_filter", .direction = "forward")
    .state <- as.character(julia_eval(glue('typeof({pf_obj}.state[1]);')))
    if (.state != "StateXY") {
      warn(".mobility implemented but the State is not StateXY.")
    }
    # Define the box in Julia
    set_mobility_box(spatMobilityBox(.map, .mobility = .mobility))
  }

  #### Run the smoother
  cat_log(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Running smoother..."))
  pf_obj <- set_smoother_two_filter(.nMC)

  #### Get particles in R
  # * TO DO: amend Julia code
  # * TO DO: generalise pf_filter() R-side code
  # cat_log(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Collating outputs..."))
  # out <- julia_eval(glue('Patter.r_get_particles({pf_obj});'))
  # out

}
