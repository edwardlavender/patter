#' @title PF: two-filter smoother
#' @description This function implements the two-filter particle smoother (Fearnhead et al., [2010](https://doi.org/10.1093/biomet/asq013)).
#' @param .map,.mobility (optional) 'Mobility box' arguments for two-dimensional states and 'fully hospitable' maps.
#'
#' * `.map` is a [`SpatRaster`] that defines the study area of the simulation (see [`pf_filter()`]).
#' * `.mobility` is a `numeric` value that defines the maximum moveable distance between two time steps (e.g., `.timeline[1]` and `.timeline[2]` in [`pf_filter()`]).
#'
#' `.map` and `.mobility` should be supplied if [`pf_filter()`] was implemented with `.state = "StateXY"` and there are no `NA`s on the `.map`.
#'
#' @param .n_particle (optional) An `integer` that defines the number of particles to smooth.
#' * If specified, a sub-sample of `.n_particle`s is used.
#' * Otherwise, `.n_particle = NULL` uses all particles from the filter.
#'
#' @param .n_sim An `integer` that defines the number of Monte Carlo simulations.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#' @details
#' The two-filter smoother smooths particle samples from the particle filter ([`pf_filter()`]). Particles from a forward and backward filter run are required in the `Julia` workspace (as defined by [`pf_filter()`]). The backend function [`Patter.two_filter_smoother()`](https://edwardlavender.github.io/Patter.jl/) does the work. Essentially, the function runs a simulation backwards in time and re-samples particles in line with the probability density of movements between each combination of states from the backward filter at time `t` and states from the forward filter at time `t - 1`. The time complexity of the algorithm is thus \eqn{O(TN^2)}. The probability density of movements is evaluated by [`Patter.logpdf_step()`](https://edwardlavender.github.io/Patter.jl/) and [`Patter.logpdf_move()`](https://edwardlavender.github.io/Patter.jl/). If individual states are two-dimensional (see [`StateXY`]) and there are no `NAs` on the `.map`, a 'mobility `box`' can be defined to simplify probability calculations. The `box` is defined by the extent of the `.map`, shrunk by `.mobility`. Within this region, the probability density of movement between two states can be calculated directly. Otherwise, a Monte Carlo simulation, of `.n_sim` iterations, is required to compute the normalisation constant (accounting for movements into inhospitable areas, or beyond the boundaries of the study area).
#'
#' @references Fearnhead, P. et al. (2010). A sequential smoothing algorithm with linear computational cost. Biometrika 97, 447–464. \url{https://doi.org/10.1093/biomet/asq013}.
#' @returns The function returns a [`pf_particles-class`] object.
#'
#' @example man/example/example-pf_smoother_two_filter.R
#' @inherit assemble seealso
#' @author Edward Lavender
#' @export

pf_smoother_two_filter <- function(.map = NULL,
                                   .mobility = NULL,
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
