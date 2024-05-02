# Print an R object in Julia
julia_print <- function(x) {
  julia_assign("x", x)
  julia_command("println(x);")
  nothing()
}

# Format time stamps for julia
julia_timestamp <- function(x) {
  check_inherits(x, "POSIXct")
  x <- check_tz(x)
  as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = lubridate::tz(x))
}
