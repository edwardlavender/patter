#' @title Parallelisation via [`lapply()`]
#'
#' @description [`cl_lapply()`] is a wrapper for [`pbapply::pblapply()`] that handles cluster checking, set up, batch processing and cluster closure.
#'
#' @param .x A `list` or sequence of objects over which to iterate.
#' @param .fun A `function` that is applied to elements of `.x`. This must accept:
#' * Elements of `.x` as the first argument;
#' * A `.chunkargs` argument if `.chunk_fun` (below) is supplied;
#'
#' @param .cl,.varlist,.envir (optional) Cluster arguments.
#' * `.cl` is the cluster argument passed to [`pbapply::pblapply()`], supplied as:
#'    * A `cluster` object from [`parallel::makeCluster()`] or a sister function;
#'    * An `integer` that defines the number of child processes;
#' * `.varlist` is a `character` vector of objects for export (see [`parallel::clusterExport()`]).
#' * `.envir` is the `environment` from which to export variables (see [`parallel::clusterExport()`]).

#' @param .chunk,.chunk_fun (optional) Chunk arguments.
#' * `chunk` is a `logical` vector that defines whether to parallelise over `.x` or batches of `.x` (chunks).
#'   * If `.chunk = FALSE`, function behaviour matches [`pbapply::pblapply()`].
#'   * If `.chunk = TRUE`, `.x` is split into a series of chunks that are processed in parallel; within each chunk, `.fun` is applied to each `.x` element. This reduces the parallelisation overhead.
#' * `.chunk_fun` is a `function` implemented once for each chunk (unlike `.fun` which is implemented for every element of every chunk). This must accept the following argument(s):
#'    * Elements of `.x` for a specific chunk as the first argument;
#'
#' The output of `.chunk_fun` is made available to `.fun` via `.chunkargs`.
#'
#' @param .use_names A `logical` variable that defines whether or use `.x`'s names to name output elements. This is silently ignored if `.combine` is specified.
#'
#' @param .combine (optional) A `function` that defines how to combine `list` elements. If `.combine = NULL`, a `list` is returned. Other suitable options are, for example, `purrr::list_flatten()` and [`data.table::rbindlist()`].
#'
#' @param ... Additional arguments passed to `.fun` and `.chunk_fun` (if supplied). Since `...` is passed to both functions, `.fun` and `.check_fun` must be able to handle unused arguments.
#'
#' @details
#'
#' [`cl_lapply()`] is exported, as we have found it useful in other projects, but primarily intended for internal use. Use [`pbapply::pboptions()`] to control the progress bar, including the number of gradations (`nout`). `nout` also controls the number of chunks on each core. Fewer chunks reduce parallelisation overhead but also the number of gradations on the progress bar.
#'
#' [`cl_chunk()`] sets the default chunk behaviour of [`cl_lapply()`] in wrapper functions:
#' * If a single core is specified, [`cl_chunk()`] returns `FALSE`.
#' * Otherwise, [`cl_chunk()`] returns `TRUE`.
#'
#' [`cl_lapply()`] and associated (internal) functions evolved from [`flapper::cl_*()`](https://edwardlavender.github.io/flapper/reference/cl.html) functions.
#'
#' @return
#' * [`cl_lapply()`] invisibly returns a `list` or a combined `list` (defined by `.combine`).
#' * [`cl_chunk()`] returns a `logical` variable.
#'
#' @seealso See [`flapper-tips-parallel`](https://edwardlavender.github.io/flapper/reference/flapper-tips-parallel.html) for further information about parallelisation, including the differences between socket clusters and forking.
#'
#' @author Edward Lavender
#' @name cl_lapply

#' @rdname cl_lapply
#' @export

cl_lapply <- function(.x, .fun, ...,
                      .cl = NULL, .varlist = NULL, .envir = .GlobalEnv,
                      .chunk = FALSE,
                      .chunk_fun = NULL,
                      .use_names = TRUE,
                      .combine = NULL) {

  # Check user inputs
  cl_check(.cl, .varlist)
  cl_check_chunk(.fun = .fun,
                 .cl = .cl,
                 .chunk = .chunk,
                 .chunk_fun = .chunk_fun)
  rlang::check_dots_used(error = function(cnd) rlang::inform(paste(cnd)))

  # Implement parallel loop
  if (.chunk) {
    y <- cl_lapply_chunk(.x = .x, .fun = .fun, ...,
                         .cl = .cl, .varlist = .varlist, .envir = .envir,
                         .chunk_fun = .chunk_fun)
  } else {
    y <- cl_lapply_elm(.x = .x, .fun = .fun, ...,
                       .cl = .cl, .varlist = .varlist, .envir = .envir)
  }

  # (optional) Name or combine outputs
  if (!is.null(names(.x)) && .use_names && is.null(.combine)) {
    names(y) <- names(.x)
  }
  if (!is.null(.combine)) {
    y <- .combine(y)
  }

  # Return outputs
  invisible(y)

}

#' @rdname cl_lapply
#' @export

cl_chunk <- function(.cl) {
  ifelse(cl_cores(.cl) == 1L, FALSE, TRUE)
}
