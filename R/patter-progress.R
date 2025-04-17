#' @title [`patter`] options: progress
#' @description [`patter`] functions enable progress monitoring via function arguments and global options. At the time of writing, there are three main tools to monitor and enhance function progress:
#' * User output messages (via the `patter.verbose` option and the `.verbose` argument);
#' * Progress bars (via [`pbapply::pboptions()`] in `R`, `ProgressMeter.Progress` in `Julia`, [`julia_progress()`], the `patter.progress` option and the `.progress` argument);
#' * Parallelisation (via `.cl_` or `.fterra` arguments in `R` or `JULIA_NUM_THREADS` in `Julia`);
#'
#' Only selected [`patter`] functions support these options but this may expand in the future, depending on user feedback.
#'
#' @param enabled,dt,showspeed Arguments for [`julia_progress()`], passed to `ProgressMeter.Progress` in `Julia`:
#' * `enabled` is a `logical` variable that defines whether or not to display a progress bar. By default, `enabled` is set from the `patter.progress` option.
#' * `dt` is a `double` that defines the duration (s) between progress bar updates (updates are generated at intervals at least `dt` seconds apart).
#' * `showspeed` is a `logical` variable that defines whether or not to append a per-iteration average duration, (e.g., `12.34 ms/it`) to the progress bar.
#'
#' @details
#'
#' # User outputs
#'
#' User output messages are controlled via the `.verbose` argument. There is a global option `patter.verbose` that can be set to suppress user output messages. See the internal [`cat_`] documentation for permitted inputs to `.verbose`.
#'
#' # Progress bars
#'
#' For pure `R` functions, progress bars are implemented via the `pbapply` package and controlled globally via [`pbapply::pboptions()`]. See the internal [`pb_`] function documentation for examples.
#'
#' For `R` functions with a `Julia` backend, progress bars are implemented via `Julia`. Set progress options via [`julia_progress`]. There is a global option this function inherits (`patter.progress`) that can be used to suppress progress bars. This is set to `TRUE` during package [`.onLoad`] unless `pbapply::pboptions()$type == "none"`. Note that `Julia` progress bars are more limited on Windows than MacOS/Linux. There may also be a speed penalty on Windows. Please report your experience.
#'
#' # Parallelisation
#'
#' For pure `R` functions, parallelisation is implemented via `.cl_` arguments passed to [`cl_lapply()`], which wraps [`pbapply::pblapply()`]. See the [`cl_lapply()`] function documentation for full details.
#'
#' For `R` functions that use [`terra::terra-package`], the 'fast-terra' `.fterra = TRUE` argument instructs [`terra::terra-package`] to use parallelisation.
#'
#' For `R` functions with a `Julia` backend, parallelisation is implemented via the `JULIA_NUM_THREADS` argument to [`julia_connect()`]. This can only be set once per `R` session.
#'
#' @author Edward Lavender
#' @name patter-progress

#' @rdname patter-progress
#' @export

julia_progress <- function(enabled = getOption("patter.progress"),
                           dt = 0.1,
                           showspeed = FALSE) {
  list(enabled = enabled, dt = dt, showspeed = showspeed)
}
