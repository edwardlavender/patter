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
# (e.g., c("ModelObsAcousticLogisTrunc", "ModelObsDepthUniform"))
chars_to_classes <- Vectorize(char_to_class)

#' @title Utilities: clean up
#' @description This function deletes temporary files and or directories recursively.
#' @param x A `character` vector (see [`unlink()`]).
#' @details This function is a simple wrapper for [`unlink()`] that is used at the end of examples.
#' @author Edward Lavender
#' @export

cleanup <- function(x) {
  unlink(x, recursive = TRUE)
  nothing()
}
