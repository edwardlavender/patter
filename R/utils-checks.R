#' @title Utility checks
#' @description These functions check user inputs.
#' @author Edward Lavender
#' @name check_utils
NULL

#' @rdname check_utils
#' @keywords internal

check_dir <- function(input) {
  if (!dir.exists(input)) {
    abort("The directory '{input}' does not exist.",
          .envir = environment())
  }
  invisible(input)
}

#' @rdname check_utils
#' @keywords internal

check_inherits <- function(input, class) {
  if (!inherits(input, class)) {
    abort("`{deparse(substitute(input))}` must be a {class}.",
          .envir = environment())
  }
  invisible(input)
}

#' @rdname check_utils
#' @keywords internal

check_names <- function(input, req, extract_names = names, type = all,
                        arg = deparse(substitute(input))) {
  input_names <- extract_names(input)
  if (!type(req %in% input_names)) {
    req_names_missing <- req[which(!(req %in% input_names))]
    msg <- paste0(
      "Argument '", arg, "' does not contain ", deparse(substitute(type)),
      " required names. One or more of the following name(s) are missing: ",
      paste0("'", req_names_missing, collapse = "', "),
      "'."
    )
    stop(msg, call. = FALSE)
  }
  invisible(input)
}

#' @rdname check_utils
#' @keywords internal

check_named_list <- function(input, ignore_empty = TRUE, arg = deparse(substitute(input))) {
  if (!any("list" %in% class(input))) stop(paste0("Argument '", arg, "' must be of class list."), call. = FALSE)
  list_is_empty <- (length(input) == 0)
  if (!list_is_empty | !ignore_empty) {
    if (is.null(names(input)) | any(names(input) %in% "")) {
      msg <- paste0("Argument '", arg, "' must be a named list.")
      stop(msg, call. = FALSE)
    }
  }
  invisible(input)
}

#' @rdname check_utils
#' @keywords internal

check_tz <-
  function(input, arg = deparse(substitute(input))) {
    if (inherits(input, "Date") | inherits(input, "POSIXct")) {
      if (lubridate::tz(input) == "") {
        msg <- paste0("Argument '", arg, "' time zone currently ''; tz forced to UTC.")
        warning(msg, immediate. = TRUE, call. = FALSE)
        lubridate::tz(input) <- "UTC"
      }
    }
    invisible(input)
  }
