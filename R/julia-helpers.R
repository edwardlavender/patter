# Print an R object in Julia
julia_print <- function(x) {
  julia_assign("x", x)
  julia_command("println(x);")
  nothing()
}
