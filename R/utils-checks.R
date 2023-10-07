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

#' @rdname check_utils
#' @keywords internal

check_dots_allowed <- function(not_allowed, ...) {
  l <- list(...)
  if (any(names(l) %in% not_allowed)) {
    trouble <- names(l)[names(l) %in% not_allowed]
    msg <- paste0(
      "Additional argument(s) (", paste0("`", trouble, collapse = "`, "),
      "`) have been passed to the function via `...` which are not permitted."
    )
    abort(msg)
  }
}


#' @rdname check_utils
#' @keywords internal

check_dots_for_missing_period <- function(args, dots) {
  # List argument names
  args <- names(args)
  # Drop dots ("...")
  args <- args[!(args %in% "...")]
  # Drop leading period
  args <- sub("^\\.", "", args)
  # Name arguments passed via dots
  dots <- names(dots)
  # Check for any arguments in dots that match args without the periods
  # * This is likely due to the missing period (e.g. 'prompt' instead of '.prompt')
  dots_bool <- dots %in% args
  if (any(dots_bool)) {
    dots_in_args <- dots[dots_bool]
    warn("There are argument(s) passed via `...` that are identical, except for the missing leading period, to the function's main arguments: {paste0(paste0('`', dots_in_args, collapse = '`, '), '`')}. Did you mean to use: {paste0(paste0('`.', dots_in_args, collapse = '`, '), '`')}?",
         .envir = environment())
  }
}

#' @rdname check_utils
#' @keywords internal

check_new_colnames <- function(.data, .new) {
  bool <- .new %in% colnames(.data)
  if (any(bool)) {
    replace <- .new[which(bool)]
    warn("Column(s) in `.data` are being replaced: {paste0(paste0('`', replace, collapse = '`, '), '`')}.",
         .envir = environment())
  }
  NULL
}
