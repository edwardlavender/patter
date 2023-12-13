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
#' @name utils-cats

#' @rdname utils-cats
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

#' @rdname utils-logs
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

#' @title Utilities: list helpers
#' @name utils-lists

#' @rdname utils-lists
#' @keywords internal

# plyr::compact()
list_compact <- function(l) l[which(!sapply(l, is.null))]

#' @rdname utils-lists
#' @keywords internal

# rlist::list.merge()
list_merge <- function(...) {
  lists <- list(...)
  if (any(vapply(lists, function(x) is.null(names(x)), logical(1L)))) {
    stop("All arguments must be named list", call. = FALSE)
  }
  Reduce(utils::modifyList, lists, list())
}

#' @rdname utils-lists
#' @keywords internal

# List arguments
list_args <- function(.args, .defaults) {
  if (is.null(.args)) {
    abort("`{deparse(substitute(.args))}` must be specified.",
          .envir = environment())
  }
  if (length(.args) == 0L) {
    return(.defaults)
  }
  check_named_list(.args)
  bool <- names(.args) %in% names(.defaults)
  if (!all(bool)) {
    unsupp <- names(.args)[!bool]
    abort("`{deparse(substitute(.args))}` contains unsupported argument(s): {str_items(unsupp)}.",
          .envir = environment())
  }
  list_merge(.defaults, .args)
}

#' @title Utilities: calculate column products
#' @description This function calculates column products for each row in a [`matrix`].
#' @param .data A [`matrix`].
#' @param .cols A vector that defines the columns in `.data` to multiply.
#' @return The function returns a `numeric` vector.
#' @author Edward Lavender
#' @keywords internal

colProds.matrix <- function(.data, .cols = seq_col(.data)) {
  if (length(.cols) == 1L) {
    return(.data[, .cols])
  }
  eval(parse(text = paste0(".data[, ", .cols, "]", collapse = " * ")))
}

