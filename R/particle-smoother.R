#' @title PF: two-filter smoother
#' @description [`pf_smoother_two_filter()`] function implements the two-filter particle smoother (Fearnhead et al., [2010](https://doi.org/10.1093/biomet/asq013)).
#' @param .map,.mobility,.vmap,.plot,... (optional) 'Validity map' arguments for [`set_map()`], used for two-dimensional states.
#'
#' * `.map` is a [`SpatRaster`] that defines the study area of the simulation (see [`pf_filter()`]). On Linux, this argument can only be used safely if `JULIA_SESSION = "FALSE"`.
#' * `.mobility` is a `numeric` value that defines the maximum moveable distance between two time steps (e.g., `.timeline[1]` and `.timeline[2]` in [`pf_filter()`]).
#' * `.vmap` is a [`SpatRaster`] (supported on Windows or MacOS), or a file path to a raster (supported on MacOS, Windows and Linux), that defines the validity map (see [`set_map()`]). This can be supplied, from a previous implementation of [`set_vmap()`] or the internal function [`spatVmap()`], instead of `.map` and `.mobility` to avoid re-computation.
#' * `.plot` is a `logical` variable that defines whether or not to plot the map.
#' * `...` is a placeholder for additional arguments, passed to [`terra::plot()`], if `.plot = TRUE`.
#'
#' The validity map is only set in Julia if `JULIA_SESSION = "TRUE"`.
#'
#' On Linux, the validity map cannot be created and set in the same `R` session. Running the function with `.map` and `.mobility` will create, but not set, the map. Write the map to file and then rerun the function with `.vmap` specified to set the map safely in `Julia`.
#'
#' @param .n_particle (optional) An `integer` that defines the number of particles to smooth.
#' * If specified, a sub-sample of `.n_particle`s is used.
#' * Otherwise, `.n_particle = NULL` uses all particles from the filter.
#'
#' @param .n_sim An `integer` that defines the number of Monte Carlo simulations.
#' @param .cache A `logical` variable that defines whether or not to pre-compute and cache movement-density normalisation constants for each unique particle.
#' @param .batch (optional) Batching controls:
#' * If [`pf_filter()`] was implemented with `.batch = NULL`, leave `.batch = NULL` here.
#' * Otherwise, `.batch` must be specified. Pass a `character` vector of `.jld2` file paths to write particles in sequential batches to file (as `Julia` `Matrix{<:State}` objects) to `.batch`; for example: `./smo-1.jld2, ./smo-2.jld2, ...`. You must use the same number of batches as in [`pf_filter()`]. `.batch` is implemented as in that function.
#'
#' @param .collect A `logical` variable that defines whether or not to collect outputs from the `Julia` session in `R`.
#' @param .progress Progress controls (see [`patter-progress`] for supported options). If enabled, one progress bar is shown for each `.batch`.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#' @details
#' The two-filter smoother smooths particle samples from the particle filter ([`pf_filter()`]). Particles from a forward and backward filter run are required in the `Julia` workspace (as defined by [`pf_filter()`]). The backend function [`Patter.particle_smoother_two_filter()`](https://edwardlavender.github.io/Patter.jl/) does the work. Essentially, the function runs a simulation backwards in time and re-samples particles in line with the probability density of movements between each combination of states from the backward filter at time `t` and states from the forward filter at time `t - 1`. The time complexity of the algorithm is thus \eqn{O(TN^2)}. The probability density of movements is evaluated by [`Patter.logpdf_step()`](https://edwardlavender.github.io/Patter.jl/) and [`Patter.logpdf_move()`](https://edwardlavender.github.io/Patter.jl/). If individual states are two-dimensional (see [`StateXY`]), a validity map can be pre-defined in `Julia` via [`set_vmap()`] to speed up probability calculations. The validity map is defined as the set of valid (non-`NA` or bordering) locations on the `.map`, shrunk by `.mobility`. Within this region, the probability density of movement between two states can be calculated directly. Otherwise, a Monte Carlo simulation, of `.n_sim` iterations, is required to compute the normalisation constant (accounting for movements into inhospitable areas, or beyond the boundaries of the study area). For movement models for which the density only depends on the particle states, set `.cache = TRUE` to pre-compute and cache normalisation constants for improved speed.
#'
#' @references Fearnhead, P. et al. (2010). A sequential smoothing algorithm with linear computational cost. Biometrika 97, 447–464. \url{https://doi.org/10.1093/biomet/asq013}.
#'
#' @returns
#' * [`set_vmap()`]:
#'      * [`set_vmap()`] returns the validity map (a [`SpatRaster`]), invisibly;
#' * [`pf_smoother_two_filter()`]:
#'     * [`Patter.particle_smoother_two_filter()`](https://github.com/edwardlavender/Patter.jl) creates a NamedTuple in the `Julia` session (named `ptf`). If `.batch = NULL`, the NamedTuple contains particles (`states`) ; otherwise, the `states` element is `nothing` and `states` are written to `.jld2` files (as a  variable named `xsmo`). If `.collect = TRUE`, [`pf_smoother_two_filter()`] collects the outputs in `R` as a [`pf_particles-class`] object (the `states` element is `NULL` is `.batch` is used). Otherwise, `invisible(NULL)` is returned.
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
    if (julia_session()) {
      julia_command("vmap = nothing;")
    }
  } else {
    if (.plot) {
      terra::plot(.vmap, ...)
    }
    if (julia_session()) {
      set_map(.vmap, .as_Raster = FALSE, .as_GeoArray = "vmap")
    }
  }
  invisible(.vmap)
}

#' @rdname pf_smoother_two_filter
#' @export

pf_smoother_two_filter <- function(.n_particle = NULL,
                                   .n_sim = 100L,
                                   .cache = TRUE,
                                   .batch = NULL,
                                   .collect = TRUE,
                                   .progress = julia_progress(),
                                   .verbose = getOption("patter.verbose")) {

  #### Initiate
  t1   <- Sys.time()
  cats <- cat_setup(.fun = "pf_smoother_two_filter", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Validate vmap
  # vmap should be pre-defined and exported to Julia via set_vmap() if required
  # * If vmap is undefined, it is set to `nothing`
  # * Otherwise, we validate that StateXY or StateXYZ
  if (!julia_exists("vmap")) {
    julia_command('vmap = nothing;')
  } else {
    if (julia_eval('!isnothing(vmap)') && is.null(.batch)) {
      pf_obj <- name_particles(.fun = "pf_filter", .direction = "forward")
      .state <- as.character(julia_eval(glue('typeof({pf_obj}.states[1]);')))
      if (!(.state %in% c("StateXY", "StateCXY"))) {
        warn("`vmap` is defined but `State` is not \"StateXY\" or \"StateXYZ\". If your model includes depth (`z`), set `vmap` to `NULL`.")
      }
    }
  }

  #### Run the smoother
  # Patter.particle_smoother_two_filter(;timeline, xfwd, xbwd, model_move, vmap, n_sim, cache, batch)
  # * `timeline`, `xfwd`, `xbwd`, `model_move` and `vmap` exist in Julia
  # * `n_sim` & cache is defined below
  cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Running smoother..."))
  pf_obj <- set_smoother_two_filter(.n_particle = .n_particle,
                                    .n_sim      = .n_sim,
                                    .cache      = .cache,
                                    .batch      = .batch,
                                    .progress   = .progress,
                                    .verbose    = .verbose)

  #### Get particles in R
  if (.collect) {
    cats$cat(paste0("... ", call_time(Sys.time(), "%H:%M:%S"), ": Collating outputs..."))
    out <- pf_particles(.pf_obj = pf_obj, .call_start = t1)
  } else {
    out <- nothing()
  }
  out

}
