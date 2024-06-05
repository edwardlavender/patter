#' @title [`prettyGraphics`](https://github.com/edwardlavender/prettyGraphics) in [`patter`]
#' @description These functions are internal plotting routines in [`patter`] taken or modified from the [`prettyGraphics`](https://github.com/edwardlavender/prettyGraphics) package.
#' @details Unlike the predecessor [`flapper`](https://github.com/edwardlavender/flapper) package, [`patter`] does not import [`prettyGraphics`], so the default plotting routines are much less advanced. This may be improved in future, depending on user feedback.
#' @author Edward Lavender
#' @name prettyGraphics

#' @rdname prettyGraphics
#' @examples
#' \dontrun{
#' #### Example (1): one_page() implementation
#' f <- function() {
#'   pp <- one_page(TRUE, 2)
#'   on.exit(par(pp), add = TRUE)
#'   hist(1:10, main = "1")
#'   hist(1:10, main = "2")
#'   }
#' f()
#' plot(1)
#' }
#' @keywords internal

one_page <- function(.one_page, .n = 1L) {
  if (.one_page) {
    if (.n > 25) {
      warn("`.one_page = TRUE` but there are more than 25 plots.")
    }
    pp <- par(mfrow = par_mf(.n), no.readonly = TRUE)
  } else {
    pp <- NULL
  }
  invisible(pp)
}

#' @rdname prettyGraphics
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

#' @rdname prettyGraphics
#' @keywords internal

add_sp_path <- function(x, y = NULL, col = hcl.colors(length(x)), ...){
  if (!is.null(y)) {
    x <- cbind(x, y)
  }
  s <- 1:(nrow(x) - 1)
  param     <- list(...)
  param$col <- col
  param$x0  <- x[s, 1]
  param$x1  <- x[s + 1, 1]
  param$y0  <- x[s, 2]
  param$y1  <- x[s + 1, 2]
  do.call(arrows, param)
}


