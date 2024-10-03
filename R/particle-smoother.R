#' @title PF: two-filter smoother
#' @description [`pf_smoother_two_filter()`] function implements the two-filter particle smoother (Fearnhead et al., [2010](https://doi.org/10.1093/biomet/asq013)).
#' @param .map,.mobility,.vmap,.plot,... (optional) 'Validity map' arguments for [`set_map()`], used for two-dimensional states.
#'
#' * `.map` is a [`SpatRaster`] that defines the study area of the simulation (see [`pf_filter()`]).
#' * `.mobility` is a `numeric` value that defines the maximum moveable distance between two time steps (e.g., `.timeline[1]` and `.timeline[2]` in [`pf_filter()`]).
#' * `.vmap` is a [`SpatRaster`] (supported on Windows or MacOS), or a file path to a raster (supported on MacOS, Windows and Linux), that defines the validity map (see [`set_map()`]). This can be supplied, from a previous implementation of [`set_vmap()`] or the internal function [`spatVmap()`], instead of `.map` and `.mobility` to avoid re-computation.
#' * `.plot` is a `logical` variable that defines whether or not to plot the map.
#' * `...` is a placeholder for additional arguments, passed to [`terra::plot()`], if `.plot = TRUE`.
#'
#' @param .n_particle (optional) An `integer` that defines the number of particles to smooth.
#' * If specified, a sub-sample of `.n_particle`s is used.
#' * Otherwise, `.n_particle = NULL` uses all particles from the filter.
#'
#' @param .n_sim An `integer` that defines the number of Monte Carlo simulations.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#' @details
#' The two-filter smoother smooths particle samples from the particle filter ([`pf_filter()`]). Particles from a forward and backward filter run are required in the `Julia` workspace (as defined by [`pf_filter()`]). The backend function [`Patter.two_filter_smoother()`](https://edwardlavender.github.io/Patter.jl/) does the work. Essentially, the function runs a simulation backwards in time and re-samples particles in line with the probability density of movements between each combination of states from the backward filter at time `t` and states from the forward filter at time `t - 1`. The time complexity of the algorithm is thus \eqn{O(TN^2)}. The probability density of movements is evaluated by [`Patter.logpdf_step()`](https://edwardlavender.github.io/Patter.jl/) and [`Patter.logpdf_move()`](https://edwardlavender.github.io/Patter.jl/). If individual states are two-dimensional (see [`StateXY`]), a validity map can be pre-defined in `Julia` via [`set_vmap()`] to speed up probability calculations. The validity map is defined as the set of valid (non-`NA` or bordering) locations on the `.map`, shrunk by `.mobility`. Within this region, the probability density of movement between two states can be calculated directly. Otherwise, a Monte Carlo simulation, of `.n_sim` iterations, is required to compute the normalisation constant (accounting for movements into inhospitable areas, or beyond the boundaries of the study area).
#'
#' @references Fearnhead, P. et al. (2010). A sequential smoothing algorithm with linear computational cost. Biometrika 97, 447–464. \url{https://doi.org/10.1093/biomet/asq013}.
#' @returns
#' * [`set_vmap()`] returns the validity map (a [`SpatRaster`]), invisibly;
#' * [`pf_smoother_two_filter()`] returns a [`pf_particles-class`] object;
#'
#' @example man/examples/example-pf_smoother_two_filter.R
#' @inherit assemble seealso
#' @author Edward Lavender
#' @name pf_smoother_two_filter
NULL

#' @rdname pf_smoother_two_filter
#' @export

# Set the map within which 2D movements are always valid
set_vmap  <- function(.map = NULL, .mobility = NULL, .vmap = NULL, .plot = FALSE, ...) {
  # Compute vmap, if un-supplied
  if (is.null(.vmap)) {
    if (!is.null(.map) & !is.null(.mobility)) {
      .vmap <- spatVmap(.map = .map, .mobility = .mobility, .plot = FALSE)
    } else {
      if ((is.null(.map) & !is.null(.mobility)) | (!is.null(.map) & is.null(.mobility))) {
        stop("`.map` and `.mobility` should either both be supplied or both be `NULL`.")
      }
    }
  } else {
    if (!is.null(.map) | !is.null(.mobility)) {
      warn("`.map` and `.mobility` arguments are ignored when `.vmap` is supplied.")
    }
  }
  # Set vmap in Julia
  if (is.null(.vmap)) {
    .vmap <- NULL
    julia_command("vmap = nothing;")
  } else {
    if (.plot) {
      terra::plot(.vmap, ...)
    }
    set_map(.vmap, .as_Raster = FALSE, .as_GeoArray = "vmap")
  }
  invisible(.vmap)
}

#' @rdname pf_smoother_two_filter
#' @export

pf_smoother_two_filter <- function(.n_particle = NULL,
                                   .n_sim = 100L,
                                   .verbose = getOption("patter.verbose")) {

  #### Initiate
  cats <- cat_setup(.fun = "pf_smoother_two_filter", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Validate vmap
  # vmap should be pre-defined and exported to Julia via set_vmap() if required
  # * If vmap is undefined, it is set to `nothing`
  # * Otherwise, we validate that StateXY
  if (!julia_exists("vmap")) {
    julia_command('vmap = nothing;')
  } else {
    if (julia_eval('isnothing(vmap)')) {
      pf_obj <- name_particles(.fun = "pf_filter", .direction = "forward")
      .state <- as.character(julia_eval(glue('typeof({pf_obj}.state[1]);')))
      if (.state != "StateXY") {
        warn("`vmap` is defined but `State` is not \"StateXY\".")
      }
    }
  }

  #### Run the smoother
  # two_filter_smoother(;timeline, xfwd, xbwd, model_move, vmap, n_sim)
  # * `timeline`, `xfwd`, `xbwd`, `model_move` and `vmap` exist in Julia
  # * `n_sim` is defined below
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Running smoother..."))
  pf_obj <- set_smoother_two_filter(.n_particle = .n_particle, .n_sim = .n_sim)

  #### Get particles in R
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Collating outputs..."))
  pf_particles(.pf_obj = pf_obj)

}
