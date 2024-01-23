#' @title Parallelisation helpers (internal)
#'
#' @description A set of internal wrappers for [`parallel::parallel`] functions that facilitate the implementation of parallel routines in functions via [`pbapply::pblapply()`].
#'
#' @details
#'
#' [`cl_lapply()`] is the main function. This is a wrapper for [`pbapply::pblapply()`] that handles cluster checking, set up, batch processing and cluster closure, using the following functions:
#' * [`cl_lapply_chunk()`] implements a function over chunks;
#' * [`cl_lapply_elm()`] implements a function over individual elements;
#' * [`cl_check()`] checks `.cl` and `.varlist` arguments, as inputted to a parent function. For example, if `.cl = NULL`, `.varlist` should also be `NULL`.
#' * [`cl_check_chunk()`] checks `.chunk` arguments.
#' * [`cl_cores()`] identifies the number of cores specified.
#' * [`cl_chunks()`] defines a `list`, with one element for each 'chunk' that contains an `integer` vector of the positions of an object over which to iterate serially in each chunk.
#' * [`cl_export()`] implements [`parallel::clusterExport()`] if both `.cl` and `.varlist` are specified
#' * [`cl_stop()`] implements [`parallel::stopCluster()`] if `.cl` is a `cluster` object from [`parallel::makeCluster()`].
#'
#' These routines evolved from [`flapper::cl_*()`](https://edwardlavender.github.io/flapper/reference/cl.html) functions.
#'
#' @return
#' * [`cl_lapply_chunk()`] returns a `list`.
#' * [`cl_lapply_elm()`] returns a `list`.
#' * [`cl_cores()`] returns an `integer`.
#' * [`cl_chunks()`] returns a list of `integer`s.
#' * [`cl_check()`], [`cl_check_chunk()`], [`cl_export()`] and [`cl_stop()`] return `invisible(NULL)`.
#'
#' @author Edward Lavender
#' @name cl

#' @rdname cl
#' @keywords internal

cl_lapply_chunk <- function(.x, .fun, ...,
                            .chunk_fun,
                            .cl, .varlist, .envir) {
  # Define list of indices by chunk
  index_by_chunk <- cl_chunks(.cl, length(.x))
  ncl         <- cl_cores(.cl)
  nchunk      <- length(index_by_chunk)
  nchunkpercl <- ceiling(nchunk / ncl)
  msg("`cl_lapply()` implemented on {ncl} core(s) using a total of {nchunk} chunk(s) (~{nchunkpercl} per core & progress gradations.).",
      .envir = environment())
  # Loop over chunks in parallel
  cl_export(.cl, .varlist, .envir)
  y_by_chunks <- pbapply::pblapply(index_by_chunk, cl = .cl, function(index_for_chunk) {
    # Get indices for chunk
    x_for_chunk <- .x[index_for_chunk]
    # Loop over chunk in serial
    if (is.null(.chunk_fun)) {
      y_for_chunk <- lapply(x_for_chunk, function(.xi) {
        .fun(.xi, ...)
      })
    } else {
      # Run chunk-level code
      cargs <- .chunk_fun(x_for_chunk, ...)
      # Run loop
      y_for_chunk <- lapply(x_for_chunk, function(.xi) {
        .fun(.xi, .chunkargs = cargs, ...)
      })
    }
    y_for_chunk
  })
  # Close cluster
  cl_stop(.cl)
  # Simplify list-by-chunk
  do.call(c, y_by_chunks)
}

#' @rdname cl
#' @keywords internal

cl_lapply_elm <- function(.x, .fun, ...,
                          .cl, .varlist, .envir) {
  cl_export(.cl, .varlist, .envir)
  on.exit(cl_stop(.cl), add = TRUE)
  pbapply::pblapply(.x, cl = .cl, function(.xi) {
    .fun(.xi, ...)
  })
}

#' @rdname cl
#' @keywords internal

cl_check <- function(.cl = NULL, .varlist = NULL) {
  if (is.null(.cl)) {
    if (!is.null(.varlist)) {
      warn("`.cl` is NULL: input to `.varlist` ignored.")
    }
  } else {
    if (!inherits(.cl, "cluster")) {
      if (.Platform$OS.type == "windows") {
        warn("Integer specifications for `.cl` (i.e., forking) on Windows are not supported.")
      }
      if (!is.null(.varlist)) {
        warn("`.cl` is an integer: input to `.varlist` ignored.")
      }
    }
  }
  invisible(NULL)
}

#' @rdname cl
#' @keywords internal

cl_check_chunk <- function(.fun,
                           .cl, .chunk,
                           .chunk_fun) {
  if (.chunk && cl_cores(.cl) == 1L) {
    warn("cores = 1L: `.chunk = TRUE` is inefficient on one core.")
  }
  if (.chunk) {
    if (!is.null(.chunk_fun)) {
      if (!(".chunkargs" %in% names(formals(.fun)))) {
        abort("`.fun` should include a `.chunkargs` argument when `.chunk = TRUE` and `.chunk_eval` is supplied.")
      }
    }
  } else {
    if (!is.null(.chunk_fun)) {
      warn(".chunk = FALSE`: `.chunk_eval` ignored.")
    }
  }
  invisible(NULL)
}

#' @rdname cl
#' @keywords internal

cl_cores <- function(.cl = NULL) {
  if (is.null(.cl)) {
    n <- 1L
  } else {
    if (inherits(.cl, "cluster")) n <- length(.cl) else n <- .cl
  }
  nmax <- parallel::detectCores()
  nmax <- ifelse(is.na(nmax), 1L, nmax)
  if (n > nmax) {
    warn("The number of CPU cores exceeds the number of detected cores.")
  }
  n
}

#' @rdname cl
#' @keywords internal

cl_chunks <- function(.cl = NULL, .length, .nout = getOption("pboptions")$nout) {
  # Define chunks as cores * nout
  # > In cl_lapply(), this produces a progress bar with nout increments
  cores  <- cl_cores(.cl)
  .nout <- max(c(1L, .nout))
  chunks <- min(c(.length, cores * .nout))
  parallel::splitIndices(.length, chunks)
}

#' @rdname cl
#' @keywords internal

cl_export <- function(.cl = NULL, .varlist = NULL, .envir = .GlobalEnv) {
  if (!is.null(.cl) && inherits(.cl, "cluster") && !is.null(.varlist)) {
    parallel::clusterExport(cl = .cl, varlist = .varlist, envir = .envir)
  }
  invisible(NULL)
}

#' @rdname cl
#' @keywords internal

cl_stop <- function(.cl = NULL) {
  if (!is.null(.cl) && inherits(.cl, "cluster")) {
    parallel::stopCluster(cl = .cl)
  }
  invisible(NULL)
}
