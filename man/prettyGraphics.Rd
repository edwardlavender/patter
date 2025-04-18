% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-helpers.R
\name{prettyGraphics}
\alias{prettyGraphics}
\alias{one_page}
\alias{par_mf}
\alias{par_mf_n}
\alias{add_sp_path}
\title{\href{https://github.com/edwardlavender/prettyGraphics}{\code{prettyGraphics}} in \code{\link{patter}}}
\usage{
one_page(.one_page, .n = 1L)

par_mf(.n)

par_mf_n(...)

add_sp_path(x, y = NULL, col = hcl.colors(length(x)), ...)
}
\description{
These functions are internal plotting routines in \code{\link{patter}} taken or modified from the \href{https://github.com/edwardlavender/prettyGraphics}{\code{prettyGraphics}} package.
}
\details{
Unlike the predecessor \href{https://github.com/edwardlavender/flapper}{\code{flapper}} package, \code{\link{patter}} does not import \code{\link{prettyGraphics}}, so the default plotting routines are much less advanced. This may be improved in future, depending on user feedback.
}
\examples{
\dontrun{
#### Example (1): one_page() implementation
f <- function() {
  pp <- one_page(TRUE, 2)
  on.exit(par(pp), add = TRUE)
  hist(1:10, main = "1")
  hist(1:10, main = "2")
  }
f()
plot(1)
}
}
\author{
Edward Lavender
}
\keyword{internal}
