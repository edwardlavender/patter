#' @title Parallelisation helpers
#'
#' @description A set of wrappers for [`parallel::parallel`] functions that facilitate the implementation of parallel routines in functions via [`pbapply::pblapply()`].
#'
#' @param .x A `list` over which to iterate.
#' @param .fun,... A function that is applied to elements of `.x` alongside any optional arguments to `.fun`.
#' @param .cl (optional) A cluster from [`parallel::makeCluster()`] or an integer that defines the number of child processes (see [`pbapply::pblapply()`]).
#' @param .varlist (optional) A character vector of objects for export (see [`parallel::clusterExport()`]). If `.cl` is a cluster, this may be required. Exported objects must be located in the global environment.
#' @param .use_chunks A logical vector that defines whether to parallelise over 'chunks' (`TRUE`) or over the elements of `.x` (`FALSE`). If `.use_chunks = TRUE`, `.x` is split into \emph{n} chunks (one per core) that are processed in parallel; within each chunk `.x` is updated iteratively.
#' @param .length An integer that defines the number of elements in the iteration.
#'
#' @details
#'
#' [`cl_lapply()`] is a wrapper for [`pbapply::pblapply()`] that handles cluster checking, set up and closure, using the following functions:
#' * [`cl_check()`] checks `.cl` and `.varlist` arguments, as inputted to a parent function. For example, if `.cl = NULL`, `.varlist` should also be `NULL`.
#' * [`cl_cores()`] identifies the number of cores specified.
#' * [`cl_chunks()`] defines a list, with one element for core specified, that contains an integer vector of the positions of an object over which to iterate serially in each chunk.
#' * [`cl_export()`] implements [`parallel::clusterExport()`] if both `.cl` and `.varlist` are specified.
#' * [`cl_stop()`] implements [`parallel::stopCluster`] if `.cl` is a `cluster` object from [`parallel::makeCluster()`].
#'
#' These routines evolved from the [`cl_*()`](https://edwardlavender.github.io/flapper/reference/cl.html) functions in [`flapper`](https://github.com/edwardlavender/flapper).
#'
#' @return
#' * [`cl_lapply()`] returns a `list`.
#' * [`cl_cores()`] returns an `integer`.
#' * [`cl_chunks()`] returns a list of `integer`s.
#' * [`cl_check()`], [`cl_export()`] and [`cl_stop()`] return `invisible(NULL)`.
#'
#' @seealso See [`flapper-tips-parallel`](https://edwardlavender.github.io/flapper/reference/flapper-tips-parallel.html) for further information about parallelisation.
#'
#' @author Edward Lavender
#' @name cl
NULL


#' @rdname cl
#' @export

cl_lapply <- function(.x, .fun, ..., .cl = NULL, .varlist = NULL, .use_chunks = FALSE) {
  # Check cluster
  cl_check(.cl = .cl, .varlist = .varlist)
  if (.use_chunks) {
    # Define list of indices by chunk
    rlang::check_installed("purrr")
    index_by_chunk <- cl_chunks(.cl = .cl, .length = length(.x))
    # Loop over chunks in parallel
    cl_export(.cl = .cl, .varlist = .varlist)
    y_by_chunks <- pbapply::pblapply(index_by_chunk, cl = .cl, function(index_for_chunk) {
      # Get indices for chunk
      x_for_chunk <- .x[index_for_chunk]
      # Loop over chunk in serial
      y_for_chunk <- lapply(x_for_chunk, function(xi) {
        .fun(xi, ...)
      })
      y_for_chunk
    })
    # Close cluster
    cl_stop(.cl = .cl)
    # Flatten list-by-chunk into a single level list
    y <- purrr::flatten(y_by_chunks)
  } else {
    # Loop over x elements in parallel
    cl_export(.cl = .cl, .varlist = .varlist)
    y <- pbapply::pblapply(.x, cl = .cl, function(xi) {
      .fun(xi, ...)
    })
    cl_stop(.cl = .cl)
  }
  y
}

#' @rdname cl
#' @export

cl_check <- function(.cl = NULL, .varlist = NULL) {
  if (is.null(.cl)) {
    if (!is.null(.varlist)) {
      warning("`.cl` is NULL: input to `.varlist` ignored.",
              immediate. = TRUE, call. = FALSE
      )
    }
  } else {
    if (!inherits(.cl, "cluster")) {
      if (.Platform$OS.type == "windows") {
        warning("Integer specifications for `.cl` (i.e., forking) on Windows are not supported.",
                immediate. = TRUE, call. = FALSE
        )
      }
      if (!is.null(.varlist)) {
        warning("`.cl` is an integer: input to `.varlist` ignored.",
                immediate. = TRUE, call. = FALSE
        )
      }
    }
  }
  invisible(NULL)
}

#' @rdname cl
#' @export

cl_cores <- function(.cl = NULL) {
  if (is.null(.cl)) {
    n <- 1L
  } else {
    if (inherits(.cl, "cluster")) n <- length(.cl) else n <- .cl
  }
  n
}

#' @rdname cl
#' @export

cl_chunks <- function(.cl = NULL, .length) {
  chunks <- cl_cores(.cl)
  parallel::splitIndices(.length, chunks)

}

#' @rdname cl
#' @export

cl_export <- function(.cl = NULL, .varlist = NULL) {
  if (!is.null(.cl) && inherits(.cl, "cluster") && !is.null(.varlist)) {
    parallel::clusterExport(cl = .cl, varlist = .varlist)
  }
  invisible(NULL)
}

#' @rdname cl
#' @export

cl_stop <- function(.cl = NULL) {
  if (!is.null(.cl) && inherits(.cl, "cluster")) {
    parallel::stopCluster(cl = .cl)
  }
  invisible(NULL)
}
