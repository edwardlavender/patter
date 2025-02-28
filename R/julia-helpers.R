#' @title Julia: helpers
#' @description A set of `Julia` helper functions.
#'
#' @author Edward Lavender
#' @name julia_helper

#' @rdname julia_helper
#' @keywords internal

julia_session <- function() {
  Sys.getenv("JULIA_SESSION") == "TRUE"
}

#' @rdname julia_helper
#' @keywords internal

# Test if Julia works
julia_works <- function(.action = abort) {
  works <- isTRUE(try(julia_eval('true'), silent = TRUE))
  if (isFALSE(works)) {
    .action("Julia is not connected.")
  }
  works
}

#' @rdname julia_helper
#' @keywords internal

# Get the value of a julia option, if specified
julia_option <- function(VALUE) {

  OPTION <- deparse(substitute(VALUE))

  #### Get VALUE(s)
  # Get inputted value
  if (missing(VALUE)) {
   VALUE <- NULL
  }
  value_input  <- VALUE
  # Get global option value (set or NULL)
  value_option <- getOption(OPTION, default = NULL)
  # Get environmental variable value (set or NULL)
  value_env    <- Sys.getenv(OPTION)
  if (!nzchar(value_env)) {
    value_env <- NULL
  }

  #### Validate VALUEs
  if (!all(is.null(value_input), is.null(value_option), is.null(value_env))) {
    if (length(unique(c(value_input, value_option, value_env))) != 1L) {
      warn("There are multiple values for `{OPTION}`.", .envir = environment())
    }
  }

  #### Set VALUE
  # Use VALUE, if specified
  # Otherwise, try global option & then environment variable
  if (is.null(VALUE) && !is.null(value_option)) {
    VALUE <- value_option
  }
  if (is.null(VALUE) && !is.null(value_env)) {
    VALUE <- value_env
  }

  #### Return VALUE
  VALUE

}

#' @rdname julia_helper
#' @keywords internal

# Find the path to a Julia Project
# * If missing, search global options & environmental variables
# * If specified, return as inputted
julia_proj_path <- function(JULIA_PROJ) {
  # Get JULIA_PROJ
  JULIA_PROJ <- julia_option(JULIA_PROJ)
  if (is.null(JULIA_PROJ)) {
    msg("Using Julia's global environment.")
  } else {
    # Normalise path
    # * This is required for correct parsing on windows in downstream functions:
    # * julia_proj_generate()
    # * julia_proj_activate()
    JULIA_PROJ <- normalizePath(JULIA_PROJ, winslash = "/", mustWork = FALSE)
  }
  JULIA_PROJ
}

#' @rdname julia_helper
#' @keywords internal

# Generate a Julia project (if required)
julia_proj_generate <- function(JULIA_PROJ) {
  if (!dir.exists(JULIA_PROJ) &&
      !file.exists(file.path(JULIA_PROJ, "Project.toml"))) {
    julia_command(glue('Pkg.generate("{JULIA_PROJ}");'))
  }
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Activate a Julia Project
julia_proj_activate <- function(JULIA_PROJ) {
  # julia_command("using Revise")
  julia_command(glue('Pkg.activate("{JULIA_PROJ}");'))
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Generate a temporary Julia project path
julia_proj_temp <- function() {
  x <- file.path(tempdir(), "Julia")
  normalizePath(x, winslash = "/", mustWork = FALSE)
}

#' @rdname julia_helper
#' @keywords internal

# List all Julia packages, incl:
# * Patter.jl
# * Required dependencies (incl. Patter.jl)
# * Additional installed packages
julia_pkg_list_full <- function(.pkg_install) {
  pkg_required  <- julia_pkg_list_req()
  pkg_requested <- .pkg_install
  pkg_installed <- julia_pkg_list_installed()
  sort(unique(c(pkg_required, pkg_requested, pkg_installed)))
}

#' @rdname julia_helper
#' @keywords internal

# List required Julia dependencies (incl. Patter.jl)
julia_pkg_list_req <- function() {
  c("ArchGDAL",
    "DataFrames",
    "Distributions",
    "GeoArrays",
    "JLD2",
    "Patter",
    "Pkg",
    "Rasters",
    "Random")
}

#' @rdname julia_helper
#' @keywords internal

# List all installed Julia dependencies
julia_pkg_list_installed <- function() {
  sort(julia_eval('collect(keys(Pkg.project().dependencies));'))
}

#' @rdname julia_helper
#' @keywords internal

# List Julia dependencies for update (incl. Patter)
# * .pkg_update = FALSE (or NULL): don't update, returns NULL
# * .pkg_update = TRUE: update all, returns all required & installed dependencies
# * .pkg_update = character vector of package names: update selectively
julia_pkg_list_update <- function(.pkg_update) {
  if (inherits(.pkg_update, "logical")) {
    if (isFALSE(.pkg_update)) {
      return(NULL)
    } else {
      return(julia_pkg_list_full(NULL))
    }
  }
  .pkg_update
}

#' @rdname julia_helper
#' @keywords internal

# List Julia dependencies for loading (incl. Patter)
# .pkg_load = FALSE (or NULL), load required dependencies only
# .pkg_load = TRUE, load all dependencies
# .pkg_load = character vector: load selected dependencies
julia_pkg_list_load <- function(.pkg_load) {
  pkg_required <- julia_pkg_list_req()
  if (is.null(.pkg_load) | isFALSE(.pkg_load)) {
    pkg_extra    <- NULL
  } else {
    pkg_extra <- julia_pkg_list_full(NULL)
  }
  sort(unique(c(pkg_required, pkg_extra)))
}

#' @rdname julia_helper
#' @keywords internal

# Get the source of Patter.jl (file path, URL)
julia_pkg_patter_source <- function(JULIA_PATTER_SOURCE) {
  # Get option
  JULIA_PATTER_SOURCE <- julia_option(JULIA_PATTER_SOURCE)
  # Set default
  # * For simplicity, default to main (whether installed/uninstalled)
  # * If un-installed, default to main
  # * Otherwise, default to installed path/URL
  default <- "https://github.com/edwardlavender/Patter.jl.git"
  if (is.null(JULIA_PATTER_SOURCE)) {
    JULIA_PATTER_SOURCE <- default
    return(default)
  }
  # (A) JULIA_PATTER_SOURCE may be a directory
  # * If so, an absolute path should be set
  # * We do not explicitly test for this criterion
  # * But this option is not implemented if a single word (e.g. dev) is provided
  # * (We assume a single word refers to a github branch rather than a directory,
  # ... even if such a folder exists)
  if (!grepl("^[A-Za-z]+$", JULIA_PATTER_SOURCE) & dir.exists(JULIA_PATTER_SOURCE)) {
    JULIA_PATTER_SOURCE <-
      normalizePath(JULIA_PATTER_SOURCE, winslash = "/", mustWork = TRUE)
  } else {
    # (B) JULIA_PATTER_SOURCE may be a URL/branch name/commit ("main", "dev", {commit})
    # * We assume any string that does not contain 'Patter.jl.git' is a partial string
    if (!grepl("Patter.jl", JULIA_PATTER_SOURCE)) {
      JULIA_PATTER_SOURCE <- paste0(default, "#", JULIA_PATTER_SOURCE)
    }
    # (optional) Validate URL, if online
    # * This is not currently implemented to minimise dependencies
    # if (rlang::is_installed("RCurl") && online() && !RCurl::url.exists(JULIA_PATTER_SOURCE)) {
    #  abort("`JULIA_PATTER_SOURCE` ({JULIA_PATTER_SOURCE}) is not a valid directory or URL.",
    #        .environ = environment())
    # }
  }
  JULIA_PATTER_SOURCE
}

#' @rdname julia_helper
#' @keywords internal

# Install Patter
# * JULIA_PATTER_SOURCE is the Julia option
# * .pkg_update is a character vector of packages for update, from julia_pkg_update_list()
julia_pkg_install_Patter <- function(JULIA_PATTER_SOURCE, .pkg_update) {
  # Get JULIA_PATTER_SOURCE
  JULIA_PATTER_SOURCE <- julia_pkg_patter_source(JULIA_PATTER_SOURCE)
  add <- FALSE
  # Install or update Patter
  if (julia_installed_package("Patter") == "nothing" | "Patter" %in% .pkg_update) {
    # (A) Add Patter.jl as a local development dependency
    if (dir.exists(JULIA_PATTER_SOURCE)) {
      julia_command(glue('Pkg.develop(path = "{JULIA_PATTER_SOURCE}");'))
    } else {
      # Add Patter.jl from remote if not installed
      # * Pkg.add(url = "...#dev") installs #dev#main (undesired)
      # * Hence, we split the JULIA_PATTER_SOURCE into url and branch
      # * And set PackageSpec()
      spec     <- strsplit(JULIA_PATTER_SOURCE, "#")[[1]]
      url      <- spec[1]
      revision <- spec[2]
      if (is.na(revision)) {
        revision <- "main"
      }
      julia_command(glue('Pkg.add(PackageSpec(url = "{url}", rev = "{revision}"))'))
    }
  }
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Install additional Julia packages
# * .pkg_install: A list of packages for install, from julia_pkg_list_full()
# * .pkg_update: A corresponding list that defines which of those packages to update, from julia_pkg_list_update()
julia_pkg_install_deps <- function(.pkg_install, .pkg_update) {
  # Drop Patter.jl, which is handled separately
  .pkg_install <- .pkg_install[!(.pkg_install %in% "Patter")]
  # Iteratively install & update dependencies as required
  lapply(.pkg_install, function(.pkg) {
    # Choose whether or not to install packages
    # * For packages in Julia's standard library (e.g., Random),
    # * ... julia_installed_package() returns 'nothing'
    # * But these packages do not require install & this is suppressed (for speed)
    if (.pkg %in% c("Pkg", "Random")) {
      install <- FALSE
    } else {
      install  <- ifelse(julia_installed_package(.pkg) == "nothing", TRUE, FALSE)
    }
    update   <- ifelse(isFALSE(install) & .pkg %in% .pkg_update, TRUE, FALSE)
    # Run installation/update
    if (install) {
      julia_install_package(.pkg)
    }
    if (update) {
      julia_update_package(.pkg)
    }
    NULL
  })
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Load Julia packages
julia_pkg_library <- function(.pkg_load) {
  lapply(.pkg_load, \(.pkg) julia_library(.pkg))
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Extract the version of Patter.jl
# * Note this requires Julia Pkg is imported
julia_pkg_version_Patter.jl <- function() {
  # Use tryCatch as Pkg.pkgversion requires Julia 1.9
  tryCatch({
    version <- julia_eval('string(Pkg.pkgversion(Patter))')
    package_version(version)
  },
  error = function(e) NA)
}

#' @rdname julia_helper
#' @keywords internal

# Check {patter} and {Patter.jl} version compatibility
julia_pkg_compat <- function() {
  # Get package versions
  patter_version    <- utils::packageVersion("Patter")
  Patter.jl_version <- julia_pkg_version_Patter.jl()
  if (!is.na(Patter.jl_version)) {
    # Warn if major versions do not align
    if (patter_version$major != Patter.jl_version$major) {
      warn("The major `patter` ({patter_version}) `Patter.jl` ({Patter.jl_version}) versions do not align.",
           .envir = environment())
      if (patter_version$major > Patter.jl_version$major) {
        warn("It looks like you should run `patter::julia_connect(.pkg_update = TRUE)`. You might also need to update `JULIA_PATTER_SOURCE`.")
      } else {
        warn("It looks like you should update the `patter` R package.")
      }
    }
  } else {
    warn("Failed to check `patter` and `Patter.jl` version compatibility.")
  }
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Handle (install/update/load) Julia packages
julia_pkg_setup <- function(JULIA_PATTER_SOURCE,
                            .pkg_install, .pkg_update,
                            .pkg_load) {
  # List Julia packages for install/update
  pkg_full   <- julia_pkg_list_full(.pkg_install = .pkg_install)
  pkg_dep    <- pkg_full[!(pkg_full %in% "Patter")]
  pkg_update <- julia_pkg_list_update(.pkg_update)
  # Install and optionally update Patter.jl
  julia_pkg_install_Patter(JULIA_PATTER_SOURCE,
                           .pkg_update = pkg_update)
  # Install and optionally update dependencies
  julia_pkg_install_deps(.pkg_install = pkg_dep,
                         .pkg_update  = pkg_update)
  # Load relevant Julia packages
  # (Run julia_pkg_list_load() at this point to pick up newly installed Julia packages)
  pkg_load <- julia_pkg_list_load(.pkg_load = .pkg_load)
  julia_pkg_library(pkg_load)
  # Validate Patter.jl/patter compatibility
  julia_pkg_compat()
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Get the number of threads used by Julia
julia_threads <- function(JULIA_NUM_THREADS) {
  nthreads <- julia_eval("Threads.nthreads()")
  if (!is.null(JULIA_NUM_THREADS) && JULIA_NUM_THREADS != "auto" && nthreads != JULIA_NUM_THREADS) {
    warn("`JULIA_NUM_THREADS` could not be set.")
  }
  invisible(nthreads)
}

#' @rdname julia_helper
#' @keywords internal

# Glimpse an R object in Julia
julia_glimpse <- function(.x) {
  julia_assign("x", .x)
  julia_command("println(x);")
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Print an object in Julia
julia_print <- function(.x) {
  julia_command(glue('println({.x})'))
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Print the summary of object in Julia
julia_summary <- function(.x) {
  julia_command(glue('summary({.x})'))
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Save an object from Julia
julia_save <- function(.x, .file = .x) {
  .file <- normalizePath(.file, winslash = "/", mustWork = FALSE)
  .file <- glue("{tools::file_path_sans_ext(.file)}.jld2")
  julia_command(glue('@save "{.file}" {.x};'))
  tools::file_path_as_absolute(.file)
}

#' @rdname julia_helper
#' @keywords internal

# Load an object into Julia
julia_load <- function(.file, .x = basename(tools::file_path_sans_ext(.file))) {
  .file <- normalizePath(.file, winslash = "/", mustWork = TRUE)
  julia_command(glue('@load "{.file}" {.x};'))
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Format time stamps for Julia
# See https://github.com/edwardlavender/patter/issues/17
julia_timeline <- function(.x) {

  #### Check .x
  check_inherits(.x, "POSIXct")
  .x <- check_tz(.x)

  #### Handle timelines from seq.POSIXt(... length.out)
  # These timelines are encodes as integers rather than numeric values
  # This leads to integer vectors rather than timelines in Julia
  # Here, we format timelines as required for Julia
  # This step is slow & therefore implemented only if the timeline is encoded as an integer internally
  # If available {fasttime} is used, though this can still be slow for long time series (> 10 s)
  # Otherwise, as.POSIXct(format()) is used, which can be very slow (> 60 s)
  if (inherits(unclass(.x), "integer")) {
    warn("Use `seq.POSIXt()` with `from`, `to` and `by` rather than `length.out` for faster handling of time stamps.")
    if (lubridate::tz(.x) %in% c("GMT", "UTC") && requireNamespace("fasttime", quietly = TRUE)) {
      .x <- fasttime::fastPOSIXct(.x, tz = lubridate::tz(.x))
    } else {
      warn("Use `fasttime` for faster formatting of time stamps.")
      .x <- as.POSIXct(format(.x, "%Y-%m-%d %H:%M:%S"), tz = lubridate::tz(.x))
    }
    check_inherits(unclass(.x), "numeric")
  }

  #### Return timeline
  .x
}

#' @rdname julia_helper
#' @keywords internal

# Check if Julia object(s) have been set
julia_check_exists <- function(...) {
  x <- list(...)
  lapply(x, function(xi) {
    if (!julia_exists(xi)) {
      abort("'{xi}' does not exist in Julia.", .envir = environment())
    }
  })
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Run multi-line sections of Julia code
julia_code <- function(.x) {
  file <- tempfile(fileext = ".jl")
  on.exit(unlink(file), add = TRUE)
  writeLines(.x, file)
  # readLines(file)
  julia_source(file)
}

#' @rdname julia_helper
#' @keywords internal

# Define the number of particles for the smoother (nothing or integer)
# (This avoids setting n_particle for the smoother in Julia)
julia_n_particle <- function(.n_particle) {
  if (is.null(.n_particle)) {
    .n_particle <- "nothing"
  } else {
    .n_particle <- as.integer(.n_particle)
  }
  .n_particle
}
