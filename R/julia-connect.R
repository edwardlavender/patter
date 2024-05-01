#' @title Julia: connect `R` to `Julia`
#' @description This function connects `R` to `Julia`.
#'
#' @param ... Arguments, such as `JULIA_HOME`, passed to [`JuliaCall::julia_setup()`].
#' @param JULIA_PROJ A `character` string that defines the directory of a `Julia` Project.
#' @param .update A `logical` variable that defines whether or not to update installed `Julia` packages.
#' @param .threads A `character` (`"auto"`) or an `integer` that defines the number of threads used by multi-threaded operations in `Julia`. This can only be set once per `R` session.
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details [`patter`] is an `R` front-end for the [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) package. This requires a local installation of Julia. This function connects R to the local `Julia` installation, sets up [`JuliaCall`], which provides the integration between `R` and `Julia`, and [`Patter.jl`](https://github.com/edwardlavender/Patter.jl). Internally, the steps are as follows:
#' * [`JuliaCall`] is set up via [`JuliaCall::julia_setup()`], using `.threads` threads.
#' * The Julia installation is validated.
#' * A local Julia Project is generated in `JULIA_PROJ` (if required) and activated. We recommend using [`patter`] within an RStudio Project. By default, the Julia Project is created in a `Julia` directory at the top-level of an RStudio Project.
#' * [`Patter.jl`](https://github.com/edwardlavender/Patter.jl) and supporting dependencies are installed or updated (if required) and loaded in the local Julia Project.
#'
#' You should run this function once per `R` session.
#'
#' To update the number of threads, restart `R` and re-run the function with an updated `.threads` argument.
#'
#' @return The function returns the Julia interface invisibly (see [`JuliaCall::julia_setup()`]).
#'
#' @author Edward Lavender
#' @export

julia_connect <- function(...,
                          JULIA_PROJ = here::here("Julia"),
                          .update = FALSE,
                          .threads = "auto",
                          .verbose = getOption("patter.verbose")) {

  #### Set up messages
  t_onset <- Sys.time()
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "julia_connect", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "julia_connect", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Set up Julia
  cat_log("... Running Julia setup...")
  if (.threads != "auto" && Sys.getenv("JULIA_NUM_THREADS") != "" && Sys.getenv("JULIA_NUM_THREADS") != .threads) {
    warn("Restart R to update the number of threads in Julia.")
  }
  Sys.setenv(JULIA_NUM_THREADS = .threads)
  julia <- julia_setup(...)

  #### Test Julia
  cat_log("... Validating Julia installation...")
  two <- tryCatch(julia_eval("1 + 1"), error = function(e) e)
  if (inherits(two, "error")) {
    abort("Test julia_eval('1 + 1') failed: \n {two$message}", .envir = environment())
  }
  if (!isTRUE(all.equal(two, 2))) {
    abort("Test julia_eval('1 + 1') returns {two}.", .envir = environment())
  }

  #### Generate Julia Project (if necessary)
  if (!dir.exists(JULIA_PROJ) && !file.exists(file.path(JULIA_PROJ, "Project.toml"))) {
    cat_log("... Generating Julia project...")
    julia_command(glue('Pkg.generate("Julia");'))
  }

  #### Activate Julia Project
  # TO DO
  # * Update Patter installation
  # * Using the development version here for convenience
  cat_log("... Activating Julia project...")
  julia_command("using Revise")
  julia_command(glue('Pkg.activate("{JULIA_PROJ}");'))
  julia_command('Pkg.develop(path = "/Users/lavended/Documents/work/projects/particle-filters/patter/packages/Patter.jl")')

  #### Install packages within Julia Project
  cat_log("... Validating dependencies...")
  pkgs <- c("Patter", "Distributions", "GeoArrays")
  lapply(pkgs, function(pkg) {
    # Check whether or not we need to install or update the package
    install <- ifelse(julia_installed_package(pkg) == "nothing", TRUE, FALSE)
    update  <- ifelse(isFALSE(install) & .update, TRUE, FALSE)
    pkg     <- ifelse(pkg == "Patter", "https://github.com/edwardlavender/Patter.jl.git", pkg)
    # Run installation/update
    if (install) {
      julia_install_package(pkg)
    }
    if (update) {
      julia_update_package(pkg)
    }
    NULL
  })

  #### Load dependencies & close
  cat_log("... Loading dependencies...")
  lapply(pkgs, \(pkg) julia_library(pkg))

  #### Validate Julia settings
  nthreads <- julia_eval("Threads.nthreads()")
  if (.threads != "auto" && nthreads != .threads) {
    warn("`JULIA_NUM_THREADS` could not be set via `.threads`.")
  }
  cat_log(paste0("... Julia set up with ", nthreads, " threads."))

  #### Return outputs
  invisible(julia)

}

