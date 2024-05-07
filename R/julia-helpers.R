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
