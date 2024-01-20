#' @title Utilities: signal messages, warnings or errors
#' @description These functions are wrappers for [`message()`], [`warning()`] and [`stop()`].
#' @param ... Arguments passed to [`glue::glue()`].
#' @details
#' * [`msg()`] is a [`message()`] wrapper;
#' * [`warn()`] is a [`warning()`] wrapper for immediate, clean warnings;
#' * [`abort()`] is a [`stop()`] wrapper for clean errors;
#'
#' @return Returned values follow parent functions.
#' @author Edward Lavender
#' @name utils-signal
NULL

#' @rdname utils-signal
#' @keywords internal

msg <- function(...) {
  message(glue::glue(...))
}

#' @rdname utils-signal
#' @keywords internal

warn <- function(...) {
  warning(glue::glue(...), immediate. = TRUE, call. = FALSE)
}

#' @rdname utils-signal
#' @keywords internal

abort <- function(...) {
  stop(glue::glue(...), call. = FALSE)
}

#' @title Utilities: `cat_*()` functions
#' @description `cat_*()` functions control user output in [`patter`] functions with the `.verbose` argument.
#' @param .verbose A `logical` variable or a `string` that defines the path to a text file.
#' * `.verbose = FALSE` suppresses user outputs;
#' * `.verbose = TRUE` sends user outputs to the console;
#' * `.verbose = file.path("path/to/text/file.txt")` sends user outputs to a `.txt` file;
#'
#' @details These are internal functions.
#' * [`cat_log_file()`] validates `.verbose` and, if necessary, creates the `.txt` file;
#' * [`cat_init()`] wraps [`cat_log_file()`] and defines an appropriate [`cat()`] wrapper for use in [`patter`] function(s) according to the input to `.verbose`;
#'
#' @examples
#' \dontrun{
#' # Define example function
#' wrap <- function(.verbose) {
#'   cat_log <- cat_init(.verbose)
#'   cat_log("Hello world")
#'   invisible(NULL)
#' }
#'
#' # `.verbose = TRUE` sends user output to the console
#' wrap(.verbose = TRUE)
#'
#' # `.verbose = {log}.txt` sends user output to file
#' log.txt <- tempfile(fileext = ".txt")
#' wrap(.verbose = log.txt)
#' readLines(log.txt)
#' unlink(log.txt)
#'
#' # `.verbose = TRUE` suppresses user output
#' wrap(.verbose = FALSE)
#' }
#' @author Edward Lavender
#' @name cat_

#' @rdname cat_
#' @keywords internal

# Create a log.txt file
cat_log_file <- function(.verbose) {
  # Check .verbose input
  if (!is.logical(.verbose) & !is.character(.verbose)) {
    abort("`.verbose` should be a logical variable or a file path.")
  }
  # Return .verbose if TRUE/FALSE
  if (is.logical(.verbose)) {
    return("")
    # Handle file path inputs
  } else if (is.character(.verbose)) {
    # Check .verbose is a text file
    if (tools::file_ext(.verbose) != "txt") {
      abort("`.verbose` ('{.verbose}') should be the path to a text (.txt) file.",
            .envir = environment())
    }
    # Check the directory exists
    if (!dir.exists(dirname(.verbose))) {
      abort("`dirname(.verbose)` ('{dirname(.verbose)}') does not exist.",
            .envir = environment())
    }
    if (!file.exists(.verbose)) {
      # Create the text file
      success <- file.create(.verbose)
      if (!success) {
        abort("Failed to create log file ('{.file}').",
              .envir = environment())
      }
    } else {
      # Warn if the text file exists and is not empty
      if (length(readLines(.verbose)) > 0L) {
        warn("`.verbose` ('{.verbose}`) already exists and is not empty!",
             .envir = environment())
      }
    }
  }
  .verbose
}

#' @rdname cat_
#' @keywords internal

# Initiate cat() options and get an appropriate cat() function
cat_init <- function(.verbose) {
  # Define log file
  log_file <- cat_log_file(.verbose = .verbose)
  # Define function to send messages to console or file
  append_messages <- ifelse(log_file == "", FALSE, TRUE)
  function(..., message = !isFALSE(.verbose), file = log_file, append = append_messages) {
    if (message) cat(paste(..., "\n"), file = file, append = append)
  }
}

#' @title Utilities: strings
#' @name utils-str

#' @rdname utils-str
#' @keywords internal

str_items <- function(.items, quo = "`") {
  if (all(.items == "")) {
    rlang::warn("`.items` is an empty string ('').")
    out <- .items
  } else {
    collap <- paste0(quo, ", ", quo)
    out <- paste0(quo, paste0(.items, collapse = collap), quo)
  }
  out
}

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

#' @title Utilities: data helpers
#' @description These are internal helpers for data structures (such as [`.data.table`]s and related objects) in [`patter`].
#' @author Edward Lavender
#' @name utils-dt

#' @rdname utils-dt
#' @keywords internal

# Add missing columns
# * .data A `data.table` or similar.
# * .col A `character` string.
add_col_real <- function(.data, .col) {
  if (!rlang::has_name(.data, .col)) {
    .data[[.col]] <- NA_real_
  }
  .data
}

#' @rdname utils-dt
#' @keywords internal

# Calculate column products for each row in a [`matrix`].
# * .data A [`matrix`].
# * .cols A vector that defines the columns in `.data` to multiply.
colProds.matrix <- function(.data, .cols = seq_col(.data)) {
  if (length(.cols) == 1L) {
    return(.data[, .cols])
  }
  eval(parse(text = paste0(".data[, ", .cols, "]", collapse = " * ")))
}
