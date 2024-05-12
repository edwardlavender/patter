# Test if Julia works
julia_works <- function(.action = abort) {
  works <- isTRUE(try(julia_eval('true'), silent = TRUE))
  if (isFALSE(works)) {
    .action("Julia is not connected.")
  }
  works
}

# Glimpse an R object in Julia
julia_glimpse <- function(x) {
  julia_assign("x", x)
  julia_command("println(x);")
  nothing()
}

# Print an object in Julia
julia_print <- function(x) {
  julia_command(glue('println({x})'))
  nothing()
}

# Print the summary of object in Julia
julia_summary <- function(x) {
  julia_command(glue('summary({x})'))
  nothing()
}

# Save an object from Julia
julia_save <- function(x, file = x) {
  file <- glue("{tools::file_path_sans_ext(file)}.jld2")
  julia_command(glue('@save "{file}" {x};'))
  tools::file_path_as_absolute(file)
}

# Format time stamps for julia
julia_timeline <- function(x) {
  check_inherits(x, "POSIXct")
  x <- check_tz(x)
  as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = lubridate::tz(x))
}

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

# Run multi-line sections of Julia code
julia_code <- function(x) {
  file <- tempfile(fileext = ".jl")
  on.exit(unlink(file), add = TRUE)
  writeLines(x, file)
  # readLines(file)
  julia_source(file)
}

# Julia threads
set_threads <- function(.threads) {
  if (.threads != "auto" &&
      Sys.getenv("JULIA_NUM_THREADS") != "" &&
      Sys.getenv("JULIA_NUM_THREADS") != .threads) {
    warn("Restart R to update the number of threads in Julia.")
  }
  Sys.setenv(JULIA_NUM_THREADS = .threads)
  nothing()
}

# Generate a Julia project (if required)
julia_proj_generate <- function(JULIA_PROJ) {
  if (!dir.exists(JULIA_PROJ) &&
      !file.exists(file.path(JULIA_PROJ, "Project.toml"))) {

    julia_command(glue('Pkg.generate("Julia");'))
  }
  nothing()
}

# Activate a Julia Project
julia_proj_activate <- function(JULIA_PROJ) {
  julia_command("using Revise")
  julia_command(glue('Pkg.activate("{JULIA_PROJ}");'))
  nothing()
}

# Install/update Julia packages
julia_packages_install <- function(.packages, .update) {
  lapply(.packages, function(.package) {
    # Check whether or not we need to install or update the package
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

# Load Julia packages
julia_packages_library <- function(.packages) {
  lapply(.packages, \(.package) julia_library(.package))
  nothing()
}

# Handle (install/update/load) Julia packages
julia_packages <- function(.packages, .update) {
  julia_packages_install(.packages, .update)
  julia_packages_library(.packages)
  nothing()
}

# Get the number of threads used by Julia
julia_threads <- function(.threads) {
  nthreads <- julia_eval("Threads.nthreads()")
  if (.threads != "auto" && nthreads != .threads) {
    warn("`JULIA_NUM_THREADS` could not be set via `.threads`.")
  }
  nthreads
}
