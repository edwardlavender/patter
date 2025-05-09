#' @title Julia: connect `R` to `Julia`
#' @description This function connects `R` to `Julia`.
#'
#' @param JULIA_HOME,JULIA_PROJ,JULIA_NUM_THREADS,JULIA_PATTER_SOURCE (optional) `Julia` options, provided as function arguments, global options or environment variables.
#' * `JULIA_HOME`---A `character` string that defines the location of the `Julia` installation (see [`JuliaCall::julia_setup()`]). Usually, this is not required.
#' * `JULIA_PROJ`---A `character` string that defines the directory of a `Julia` Project. If unspecified, the default environment (e.g., `~/.julia/environments/v1.10/Project.toml`) is used with a [`message`] instead of a local `Julia` project.
#' * `JULIA_NUM_THREADS`---On MacOS or Linux, `JULIA_NUM_THREADS` is a `character` (`"auto"`) or an `integer` that defines the number of threads used by multi-threaded operations in `Julia`. This defaults to `"auto"` (not `1`). This can only be set once per `R` session. On Windows, `JULIA_NUM_THREADS` must be set system-wide and use of this argument produces a [`warning`]. See this [GitHub Issue](https://github.com/edwardlavender/patter/issues/11) for instructions.
#' * `JULIA_PATTER_SOURCE`---For advanced use only: a `character` string that defines the source of [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). This may be:
#'    * An absolute file path to a local copy of the package, in which case it is added as a development dependency;
#'    * A Git branch name (e.g., `"main"`, `"dev"`) or a commit SHA (e.g., `"b7c2fda733f80fcfd8770058cded7e0946b3abc0"`);
#'    * A valid installation URL (e.g., `"https://github.com/edwardlavender/Patter.jl.git"`);
#'
#' If missing, `JULIA_PATTER_SOURCE` defaults to `"https://github.com/edwardlavender/Patter.jl.git"`.
#'
#' If `JULIA_PATTER_SOURCE` changes, force the update by `.pkg_update`.
#'
#' @param .pkg_config,.pkg_install,.pkg_update,.pkg_load (optional) `Julia` package options.
#' * `.pkg_config` is a `character` string of `Julia` code, evaluated by [`julia_code()`], that configures `Julia` prior to dependency management.
#' * `.pkg_install`---A `character` vector of accessory `Julia` packages for install.
#' * `.pkg_update`---Package update control:
#'      * `NULL` or `FALSE` suppresses package updates;
#'      * `TRUE` updates all installed packages;
#'      * A `character` vector updates named packages;
#' * `.pkg_load`---Package loading (`using`) control:
#'      * `NULL` or `FALSE` loads required packages only;
#'      * `TRUE` loads all installed packages
#'      * A `character` vector loads required and additionally named packages;
#'
#' Note that for [`Patter.jl`](https://github.com/edwardlavender/Patter.jl), `.pkg_update` does not respect the current branch of the installation and always defaults to `"https://github.com/edwardlavender/Patter.jl.git"`. Specify `JULIA_PATTER_SOURCE` to update on a different branch.
#'
#' @param .socket A `logical` variable that defines whether or not to reconnect to `Julia`.
#' * Use `FALSE` (default) to skip re-running the function (i.e., if `JULIA_SESSION = "TRUE"`);
#' * Use `TRUE` to force a reconnect. This is required for nodes in a socket cluster, which inherit `JULIA_SESSION = "TRUE"` but which are not connected to `Julia`;
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#' @param ... Additional arguments passed to [`JuliaCall::julia_setup()`] (excluding `verbose`).
#'
#' @details [`patter`] is an `R` front-end for the [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) package. This requires a local installation of `Julia`. This function connects `R` to the local `Julia` installation, sets up [`JuliaCall`](https://github.com/JuliaInterop/JuliaCall), which provides the integration between `R` and `Julia`, and [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). Internally, the steps are as follows:
#' * [`JuliaCall`](https://github.com/JuliaInterop/JuliaCall) is set up via [`JuliaCall::julia_setup()`].
#' * The environment variable `JULIA_SESSION` is set to `"TRUE"`.
#' * The number of threads is set, if possible, via `JULIA_NUM_THREADS`.
#' * The `Julia` installation is validated.
#' * A local `Julia` Project is generated in `JULIA_PROJ` (if specified and required) and activated. We recommend using [`patter`] within an RStudio Project, with a `Julia` directory at the top-level that contains the `Julia` project.
#' * If specified, `.pkg_config` is run via [`julia_code()`].
#' * [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) and supporting dependencies are added or updated (if required) and loaded (optionally in the local `Julia` Project).
#'
#' To set `JULIA_*` options (e.g., `JULIA_HOME`, `JULIA_PROJ`, `JULIA_NUM_THREADS`), it is recommended to use `.Rprofile` or `.Renviron`. To set these options in `.Rprofile`, follow these instructions:
#'
#' ```
#' # (1) Open .Rprofile
#' usethis::edit_r_profile()
#'
#' # (2) Add the following to .Rprofile
#'
#' # Use a local `Julia` project:
#' if (!requireNamespace("here", quietly = TRUE)) {
#'   install.packages("here")
#' }
#' julia_proj <- here::here("Julia")
#' if (!dir.exists(julia_proj)) {
#'   dir.create(julia_proj)
#' }
#' Sys.setenv(JULIA_PROJ = here::here("Julia"))
#'
#' # Set `JULIA_NUM_THREADS` (on MacOS/Linux):
#' # - In general, more threads faster, up to a point;
#' # - On Windows, follow the alternative instructions linked above;
#' # - (You may also set [`data.table::data.table`] threads here via `OMP_NUM_THREADS`)
#' Sys.setenv(JULIA_NUM_THREADS = parallel::detectCores() - 1L)
#'
#' # (3) Save .Rprofile and restart R:
#'
#' # (4) Run `julia_connect()` in the `R` console as usual:
#' # There is no need to specify `JULIA_*` arguments e.g., JULIA_PROJ
#' # There are inherited from .Rprofile
#' julia_connect()
#' ```
#'
#' You should run [`julia_connect()`] once per `R` session (and additionally for every socket in a socket cluster, if necessary).
#'
#' To update the number of threads, restart `R` and re-run the function with an updated `JULIA_NUM_THREADS` argument.
#'
#' @return The function returns the `Julia` interface invisibly (see [`JuliaCall::julia_setup()`]). If `JULIA_SESSION` is already `"TRUE"` and `.socket = FALSE`, `invisible(NULL)` is returned.
#'
#' @example man/examples/example-julia_connect.R
#' @seealso See [`julia_validate()`] to validate the `R`---`Julia` interface.
#' @author Edward Lavender
#' @export

julia_connect <- function(JULIA_HOME,
                          JULIA_PROJ,
                          JULIA_NUM_THREADS,
                          JULIA_PATTER_SOURCE,
                          .pkg_config = NULL,
                          .pkg_install = NULL,
                          .pkg_update = NULL,
                          .pkg_load = NULL,
                          .socket = !interactive(),
                          .verbose = getOption("patter.verbose"), ...) {

  #### Initiate
  # Skip reconnection
  if (!.socket && julia_session()) {
    message("`Julia` already connected. Set `.socket = TRUE` to reconnect.")
    return(nothing())
  }
  # Set up messages
  cats <- cat_setup(.fun = "julia_connect", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)
  # Warn on Linux
  if (os_linux() && any(c("sf", "terra") %in% loadedNamespaces())) {
    warn("This is a Linux machine and one or more geospatial name spaces (e.g., `terra`) are loaded! Connecting to `Julia`, which simultaneously uses `Julia`'s geospatial dependencies (e.g., `GeoArrays`), is likely to crash the machine. Please report your experience. Pausing for 20 s so that you can optionally kill this process...")
    Sys.sleep(20)
  }

  #### Set up Julia
  cats$cat("... Running `Julia` setup via `JuliaCall::julia_setup()`...")
  JULIA_NUM_THREADS <- set_JULIA_NUM_THREADS(JULIA_NUM_THREADS)
  julia             <- julia_setup(..., verbose = .verbose)
  Sys.setenv("JULIA_SESSION" = "TRUE")

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
  julia_pkg_setup(JULIA_PATTER_SOURCE,
                  .pkg_install = .pkg_install,
                  .pkg_update = .pkg_update,
                  .pkg_load = .pkg_load)

  #### Validate Julia settings
  nthreads <- julia_threads(JULIA_NUM_THREADS)
  cats$cat(paste0("... `Julia` set up with ", nthreads, " thread(s)."))

  #### Return outputs
  invisible(julia)

}

#' @title Julia: validate the `R`---`Julia` interface
#' @description Load and attach [`patter`], connect to `Julia` via [`julia_connect()`] and then run [`julia_validate()`] to validate the `R`---`Julia` interface works.
#' @details
#' This function validates the `R`---`Julia` interface with an example [`terra::SpatRaster`] that is exported to `Julia` and then modified in `R`. If the function returns nothing, you should be good to go. On some systems, we have observed segmentation faults that crash `R` when the map is exported to `Julia` and/or modified in `R`. Please report issues.
#'
#' @return The function returns `invisible(NULL)`, unless an error is experienced.
#' @example man/examples/example-julia_validate.R
#' @seealso See [`julia_connect()`] to connect `R` to `Julia`.
#' @author Edward Lavender
#' @export

julia_validate <- function() {
  # Test that Julia works
  julia_works()
  # Define a map
  if (!os_linux()) {
    map <- dat_gebco(.return = "SpatRaster")
  } else {
    map <- dat_gebco(.return = "character")
  }
  # Export the map to Julia
  set_map(map)
  if (!julia_exists("env")) {
    abort("Failed to export an example SpatRaster to `Julia`.")
  }
  # Test use of terra to modify map
  if (!os_linux()) {
    map <- terra::classify(map, cbind(0, NA))
  }
  nothing()
}
