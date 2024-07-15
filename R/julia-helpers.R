#' @title Julia: helpers
#' @description A set of `Julia` helper functions.
#' @details
#' The following functions are exported:
#' * [`julia_run()`] defines whether or not to run examples;
#'
#' @author Edward Lavender
#' @name julia_helper

#' @rdname julia_helper
#' @export

# Choose whether or not to run Julia examples
julia_run <- function() {
  identical(Sys.getenv("AUTO_JULIA_INSTALL"), "true")
}

#' @rdname julia_helper
#' @export

# Choose whether or not to skip patter tests
# * If TRUE, skip
# * This is a temporary solution to skipping tests with covr, CI etc.
julia_skip <- function() {
  !identical(Sys.getenv("PATTER_TEST"), "true")
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

# Find the path to a Julia Project
# * If missing, search global options & environmental variables
# * If specified, return as inputted
julia_proj_path <- function(JULIA_PROJ) {
  if (missing(JULIA_PROJ)) {
    # Scan global option
    path <- getOption("JULIA_PROJ")
    if (!is.null(path)) {
      JULIA_PROJ <- path
    }
    # Scan environmental variables
    if (missing(JULIA_PROJ)) {
      path <- Sys.getenv("JULIA_PROJ")
      if (path != "") {
        JULIA_PROJ <- path
      }
    }
    # If still missing, use `NULL` with a warning
    if (missing(JULIA_PROJ)) {
      warn("`JULIA_PROJ` not found in global options or environmental variables: using `JULIA_PROJ = NULL`.")
      JULIA_PROJ <- NULL
    }
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
  file.path(tempdir(), "Julia")
}

#' @rdname julia_helper
#' @keywords internal

# Install Patter.jl as a development package if PATTER.JL_DEV is set
julia_packages_dev_Patter.jl <- function() {
  Patter.jl_path <- Sys.getenv("PATTER.JL_DEV")
  if (Patter.jl_path != "") {
    check_dir_exists(Patter.jl_path)
    julia_command(glue('Pkg.develop(path = "{Patter.jl_path}")'))
    return(TRUE)
  }
  FALSE
}

#' @rdname julia_helper
#' @keywords internal

# Install/update Julia packages
julia_packages_install <- function(.packages, .update) {
  # (optional) Install Patter.jl as a development package
  use_dev_Patter.jl <- julia_packages_dev_Patter.jl()
  # Handle remaining package(s)
  lapply(.packages, function(.package) {
    # Check whether or not we need to install or update the package
    if (.package == "Patter" && use_dev_Patter.jl) {
      return(NULL)
    }
    install  <- ifelse(julia_installed_package(.package) == "nothing", TRUE, FALSE)
    update   <- ifelse(isFALSE(install) & .update, TRUE, FALSE)
    .package <- ifelse(.package == "Patter",
                       "https://github.com/edwardlavender/Patter.jl.git",
                       .package)
    # Run installation/update
    if (install) {
      julia_install_package(.package)
    }
    if (update) {
      julia_update_package(.package)
    }
    NULL
  })
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Load Julia packages
julia_packages_library <- function(.packages) {
  lapply(.packages, \(.package) julia_library(.package))
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Handle (install/update/load) Julia packages
julia_packages <- function(.packages, .update) {
  julia_packages_install(.packages, .update)
  julia_packages_library(.packages)
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Get the number of threads used by Julia
julia_threads <- function(.threads) {
  nthreads <- julia_eval("Threads.nthreads()")
  if (.threads != "auto" && nthreads != .threads) {
    warn("`JULIA_NUM_THREADS` could not be set via `.threads`.")
  }
  nthreads
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
  .file <- glue("{tools::file_path_sans_ext(.file)}.jld2")
  julia_command(glue('@save "{.file}" {.x};'))
  tools::file_path_as_absolute(.file)
}

#' @rdname julia_helper
#' @keywords internal

# Load an object into Julia
julia_load <- function(.file, .x = basename(tools::file_path_sans_ext(.file))) {
  julia_command(glue('@load "{.file}" {.x};'))
  nothing()
}

#' @rdname julia_helper
#' @keywords internal

# Format time stamps for Julia
julia_timeline <- function(.x) {
  check_inherits(.x, "POSIXct")
  .x <- check_tz(.x)
  as.POSIXct(format(.x, "%Y-%m-%d %H:%M:%S"), tz = lubridate::tz(.x))
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
