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
#' If `missing` and unfound, `JULIA_PROJ = NULL` is used with a [`message`].
#'
#' If `NULL`, a `Julia` Project is not used and the default environment is used (e.g., `~/.julia/environments/v1.10/Project.toml`).
#'
#' @param .pkg_config (optional) A `character` string of `Julia` code, evaluated by [`julia_code()`], that configures `Julia` prior to dependency management.
#' @param .pkg_update A `logical` variable that defines whether or not to update installed `Julia` packages.
#' @param .threads A `character` (`"auto"`) or an `integer` that defines the number of threads used by multi-threaded operations in `Julia`. This can only be set once per `R` session. **It is currently ignored on Windows** (see below).
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details [`patter`] is an `R` front-end for the [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) package. This requires a local installation of `Julia`. This function connects `R` to the local `Julia` installation, sets up [`JuliaCall`], which provides the integration between `R` and `Julia`, and [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). Internally, the steps are as follows:
#' * [`JuliaCall`] is set up via [`JuliaCall::julia_setup()`].
#' * The number of threads is set via the `JULIA_NUM_THREADS` environment variable:
#'    - If `.threads = NULL`, `JULIA_NUM_THREADS` is set to `"auto"` if unset or left unchanged otherwise;
#'    - Otherwise, `JULIA_NUM_THREADS` is set to `.threads`;
#'    - The number of threads can only be set once per `R` session;
#'    - On Windows, setting `JULIA_NUM_THREADS` as an environment variable in `.Renviron` or using `.threads` does not work. See this [issue](https://github.com/edwardlavender/patter/issues/11) for a workaround.
#' * The `Julia` installation is validated.
#' * A local `Julia` Project is generated in `JULIA_PROJ` (if specified and required) and activated. We recommend using [`patter`] within an RStudio Project, with a `Julia` directory at the top-level that contains the `Julia` project.
#' * If specified, `.pkg_config` is run via [`julia_code()`].
#' * [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) and supporting dependencies are installed or updated (if required) and loaded (optionally in the local `Julia` Project). If the environment variable `PATTER.JL_DEV = "path/to/local/clone/of/Patter.jl"` is set, [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) is installed from a local source as a development dependency (via `Pkg.develop()`); otherwise, [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) is installed from the remote.
#'
#' You should run this function once per `R` session.
#'
#' To update the number of threads, restart `R` and re-run the function with an updated `.threads` argument.
#'
#' @return The function returns the `Julia` interface invisibly (see [`JuliaCall::julia_setup()`]).
#'
#' @example man/examples/example-julia_connect.R
#' @seealso See [`julia_validate()`] to validate the `R`---`Julia` interface.
#' @author Edward Lavender
#' @export

julia_connect <- function(...,
                          JULIA_PROJ,
                          .pkg_config = NULL,
                          .pkg_update = FALSE,
                          .threads = NULL,
                          .verbose = getOption("patter.verbose")) {

  #### Initiate
  cats <- cat_setup(.fun = "julia_connect", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)

  #### Set up Julia
  cats$cat("... Running `Julia` setup via `JuliaCall::julia_setup()`...")
  .threads <- set_threads(.threads = .threads)
  julia    <- julia_setup(..., verbose = .verbose)

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
  if (!is.null(.pkg_config)) {
    julia_code(.pkg_config)
  }
  pkgs <- c("Patter",
            "DataFrames", "Distributions",
            "Rasters", "ArchGDAL",
            "GeoArrays",
            "JLD2", "Random")
  julia_packages(.packages = pkgs, .update = .pkg_update)

  #### Validate Julia settings
  nthreads <- julia_threads(.threads)
  cats$cat(paste0("... `Julia` set up with ", nthreads, " thread(s)."))

  #### Return outputs
  invisible(julia)

}

#' @title Julia: validate the `R`---`Julia` interface
#' @description Load and attach [`patter`], connect to `Julia` via [`julia_connect()`] and then run [`julia_validate()`] to validate the `R`---`Julia` interface works.
#' @details
#' This function validates the `R`---`Julia` interface with an example [`SpatRaster`] that is exported to `Julia` and then modified in `R`. If the function returns nothing, you should be good to go. On some systems, we have observed segmentation faults that crash `R` when the map is exported to `Julia` and/or modified in `R`. Please report issues.
#'
#' @return The function returns `invisible(NULL)`, unless an error is experienced.
#' @example man/examples/julia_validate.R
#' @seealso See [`julia_connect()`] to connect `R` to `Julia`.
#' @author Edward Lavender
#' @export

julia_validate <- function() {
  # Test that Julia works
  julia_works()
  # Define a map
  map <- dat_gebco()
  # Export the map to Julia
  set_map(map)
  if (!julia_exists("env")) {
    abort("Failed to export an example SpatRaster to `Julia`.")
  }
  # Test use of terra to modify map
  map <- terra::classify(map, cbind(0, NA))
  nothing()
}
