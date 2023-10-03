#' @title Plot helper: collate plots on one page
#' @description This function is imported from `prettyGraphics::par_mf()`.
#' @param .n An `integer` that defines the number of plots.
#' @return The function returns an integer vector of two numbers.
#' @source `prettyGraphics::par_mf()`
#' @author Edward Lavender
#' @examples
#' # par_mf(10)
#' @keywords internal

par_mf <- function(.n){
  n <- as.integer(.n)
  c <- r <- trunc(sqrt(n))
  if (c < 1)
    r <- c <- 1
  if (c * r < n)
    c <- c + 1
  if (c * r < n)
    r <- r + 1
  c(r, c)
}
