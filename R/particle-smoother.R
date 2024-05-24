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
                                   .n_particle = NULL,
                                   .n_sim = 100L,
                                   .verbose = getOption("patter.verbose")) {

  #### Initiate
  cats <- cat_setup(.fun = "pf_smoother_two_filter", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Define smoother arguments
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Setting smoother arguments..."))
  # two_filter_smoother(xfwd, xbwd, move, box, nMC)
  # * xfwd, xbwd and move exist in Julia
  # * box and nMC are defined below

  #### (optional) Define mobility box
  box <- NULL
  if (!is.null(.mobility) & !is.null(.map)) {
    # `.mobility` can be provided to define a box within which movements are always valid for 2D states
    pf_obj <- name_particles(.fun = "pf_filter", .direction = "forward")
    .state <- as.character(julia_eval(glue('typeof({pf_obj}.state[1]);')))
    if (.state != "StateXY") {
      warn("`.mobility` implemented but the `State` is not \"StateXY\".")
    }
    # Define the box in `Julia`
    box <- spatMobilityBox(.map, .mobility = .mobility)
  }
  set_mobility_box(box)

  #### Run the smoother
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Running smoother..."))
  pf_obj <- set_smoother_two_filter(.n_particle = .n_particle, .n_sim = .n_sim)

  #### Get particles in R
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Collating outputs..."))
  pf_particles(.xinit = NULL, .pf_obj = pf_obj)

}
