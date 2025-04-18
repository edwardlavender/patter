dist_2d <- function(xy0, xy1, pairwise = FALSE) {
  if (pairwise) {
    sqrt((xy0[, 1] - xy1[, 1])^2 + (xy0[, 2] - xy1[, 2])^2)
  } else {
    sqrt(outer(xy0[, 1], xy1[, 1], FUN = function(a, b) (a - b)^2) +
           outer(xy0[, 2], xy1[, 2], FUN = function(a, b) (a - b)^2))
  }
}
