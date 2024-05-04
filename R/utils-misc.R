# Return invisible(NULL)
nothing <- function() {
  invisible(NULL)
}

# Return the difference between two time stamps as a string (e.g., "2 mins")
diffstep <- function(x) {
  step <- difftime(x[2], x[1])
  paste(as.numeric(step), attr(step, "units"))
}
