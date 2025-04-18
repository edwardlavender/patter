#' @title Utilities: list helpers
#' @description These are internal list helpers in [`patter`].
#' @author Edward Lavender
#' @name utils-lists

#' @rdname utils-lists
#' @keywords internal

# plyr::compact()
list_compact <- function(l) l[which(!sapply(l, is.null))]

#' @rdname utils-lists
#' @keywords internal

# rlist::list.merge() inspired function
list_merge <- function(...) {
  lists <- list(...)
  if (any(vapply(lists, function(x) length(x) > 0L && is.null(names(x)), logical(1L)))) {
    stop("A named list is expected.", call. = FALSE)
  }
  list_modify <- function(x, val) {
    utils::modifyList(x, val, keep.null = TRUE)
  }
  Reduce(list_modify, x = lists, init = list())
}

#' @rdname utils-lists
#' @keywords internal

# List arguments
# * Merge a default list of arguments, dots and user provided options
# * This function is meant for use with do.call()
# * And situations when the user can provide arguments via a list
list_args <- function(.default = list(), .dots = list(), .user = list()) {
  if (is.null(.user)) {
    return(NULL)
  }
  list_merge(.default, .dots, .user)
}
