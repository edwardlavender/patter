# Return invisible(NULL)
nothing <- function() {
  invisible(NULL)
}

# Return the difference between two time stamps as a string (e.g., "2 mins")
diffstep <- function(x) {
  step <- difftime(x[2], x[1])
  paste(as.numeric(step), attr(step, "units"))
}

# Return the units
diffunit <- function(x) {
  step <- difftime(x[2], x[1])
  attr(step, "units")
}

# Convert a `character` (e.g., "StateXY") to a class for dispatch
char_to_class <- function(.x) {
  structure(list(), class = .x)
}

# Convert a `character` vector to a list of classes for dispatch
# (e.g., c("ModelObsAcousticLogisTrunc", "ModelObsDepthUniformSeabed"))
chars_to_classes <- Vectorize(char_to_class)
