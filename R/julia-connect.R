#' @title Julia: connect `R` to `Julia`
#' @description This function connects `R` to `Julia`.
#'
#' @param ... Arguments, such as `JULIA_HOME`, passed to [`JuliaCall::julia_setup()`] (excluding `verbose`, which is handled below).
#' @param JULIA_PROJ (optional) A `character` string that defines the directory of a `Julia` Project.
#'
#' If `missing`, the function scans:
#' * The global option, `JULIA_PROJ`;
#' * The environmental variable, `JULIA_PROJ`;
#'
#' If `missing` and unfound, `JULIA_PROJ = NULL` is used with a [`warning`].
#'
#' If `NULL`, a `Julia` Project is not used and the default environment is used (e.g., `~/.julia/environments/v1.10/Project.toml`).
#'
#' @param .update A `logical` variable that defines whether or not to update installed `Julia` packages.
#' @param .threads A `character` (`"auto"`) or an `integer` that defines the number of threads used by multi-threaded operations in `Julia`. This can only be set once per `R` session.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details [`patter`] is an `R` front-end for the [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) package. This requires a local installation of `Julia`. This function connects `R` to the local `Julia` installation, sets up [`JuliaCall`], which provides the integration between `R` and `Julia`, and [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). Internally, the steps are as follows:
#' * [`JuliaCall`] is set up via [`JuliaCall::julia_setup()`], using `.threads` threads.
#' * The `Julia` installation is validated.
#' * A local `Julia` Project is generated in `JULIA_PROJ` (if specified and required) and activated. We recommend using [`patter`] within an RStudio Project, with a `Julia` directory at the top-level that contains the `Julia` project.
#' * [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) and supporting dependencies are installed or updated (if required) and loaded (optionally in the local `Julia` Project).
#'
#' You should run this function once per `R` session.
#'
#' To update the number of threads, restart `R` and re-run the function with an updated `.threads` argument.
#'
#' @return The function returns the `Julia` interface invisibly (see [`JuliaCall::julia_setup()`]).
#'
#' @example man/examples/example-julia_connect.R
#' @author Edward Lavender
#' @export

julia_connect <- function(...,
                          JULIA_PROJ,
                          .update = FALSE,
                          .threads = "auto",
                          .verbose = getOption("patter.verbose")) {

  #### Initiate
  cats <- cat_setup(.fun = "julia_connect", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Set up Julia
  cats$cat("... Running `Julia` setup via `JuliaCall::julia_setup()`...")
  set_threads(.threads = .threads)
  julia <- julia_setup(..., verbose = .verbose)

  #### Test Julia
  cats$cat("... Validating Julia installation...")
  julia_works(.action = abort)

  #### (optional) Use Julia Project
  JULIA_PROJ <- julia_proj_path(JULIA_PROJ)
  if (!is.null(JULIA_PROJ)) {
    cats$cat("... Setting up Julia project...")
    julia_proj_generate(JULIA_PROJ)
    julia_proj_activate(JULIA_PROJ)
  }

  #### Install & load packages (optionally within the Julia Project)
  cats$cat("... Handling dependencies...")
  pkgs <- c("Patter", "DataFrames", "Distributions", "GeoArrays", "JLD2", "Random")
  # Install Patter
  # * TO DO
  # * Update Patter installation
  # * Using the development version here for convenience
  julia_command('Pkg.develop(path = "/Users/lavended/Documents/work/projects/particle-filters/patter/packages/Patter.jl")')
  julia_packages(.packages = pkgs, .update = .update)

  #### Validate Julia settings
  nthreads <- julia_threads(.threads)
  cats$cat(paste0("... Julia set up with ", nthreads, " threads."))

  #### Return outputs
  invisible(julia)

}

