#' @title [`patter`] options: progress
#' @description [`patter`] functions enable progress monitoring via function arguments and global options. At the time of writing, there are three main tools to monitor and enhance function progress:
#' * User output messages (via the `patter.verbose` option and the `.verbose` argument);
#' * Progress bars (via [`pbapply::pboptions()`]);
#' * Parallelisation (via `JULIA_NUM_THREADS` and `.cl_` arguments);
#'
#' Only selected [`patter`] functions support these options but this may expand in the future, depending on user feedback.
#'
#' @details
#'
#' # User outputs
#'
#' User output messages are controlled via the `.verbose` argument. There is a global option `patter.verbose` that can be set to suppress user output messages. See the internal [`cat_`] documentation for permitted inputs to `.verbose`.
#'
#' # Progress bars
#'
#' For `R` functions with a `Julia` backend, progress bars are implemented via `Julia` and cannot be modified by the user.
#'
#' For pure `R` functions, progress bars are implemented via the `pbapply` package and controlled globally via [`pbapply::pboptions()`]. See the internal [`pb_`] function documentation for examples.
#'
#' # Parallelisation
#'
#' For `R` functions with a `Julia` backend, parallelisation is implemented via the `JULIA_NUM_THREADS` argument to [`julia_connect()`]. This can only be set once per `R` session.
#'
#' For pure `R` functions, parallelisation is implemented via `.cl_` arguments passed to [`cl_lapply()`], which wraps [`pbapply::pblapply()`]. See the [`cl_lapply()`] function documentation for full details.
#'
#' @author Edward Lavender
#' @name patter-progress
NULL
