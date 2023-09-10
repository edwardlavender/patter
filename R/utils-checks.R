#' @title Check a directory exists
#' @description This function checks that a directory exists.
#' @param x A string that defines a directory.
#' @param type A character that defines whether or not to throw a warning or an error.
#' @return If the directory `x` does not exist, the function returns a warning or error; otherwise `x` is invisibly returned.
#' @examples
#' \dontrun{
#' check_dir_exists(tempdir()) # works
#' check_dir_exists(file.path(tempdir(), "blah")) # error
#' }
#' @author Edward Lavender
#' @export

check_dir_exists <- function(x, type = c("abort", "warn")) {
  type <- match.arg(type)
  exist <- sapply(x, dir.exists)
  if (any(!exist)) {
    msg <-
      glue::glue(
        "The following input(s) to 'x' do not exist: '",
        glue::glue_collapse(x[!exist], sep = "', '"), "'."
      )
    switch(type,
           abort = abort(msg),
           warn  = warn(msg)
    )
  }
  invisible(x)
}


#' @title Check inherited class(es)
#' @description This function checks inherited class(es).
#' @param x An object.
#' @param class The required class(es).
#' @return The function checks whether `x` inherits `class` and, if not, returns an error.
#' @keywords internal

check_inherits <- function(x, class) {
  if (!inherits(x, class)) {
    abort("`{deparse(substitute(x))}` must be a {substitute(class)}.",
          .envir = environment())
  }
}


#' @title Check the names of an object contain required names
#' @description This function checks whether required names are contained within an object. If the object does not contain any/all required names (criteria are controlled by the user), the function returns a helpful error message.
#' @param arg A character string that defines the argument of the parent function.
#' @param input An object for which the names need to be checked.
#' @param req A character vector of required names.
#' @param extract_names A function that is used to extract names from `input`, such as [base::names()] or [base::colnames()].
#' @param type A function that defines the failure criteria. For example, if `type = all`, the function will return an error unless all the names in `req` are contained within `input`. This is the default. If `type = any`, the function will return an error only if none of the names in `req` are contained within `input`.
#' @return If the input fails the check, the function returns a helpful error message. Otherwise, nothing is returned.
#'
#' @author Edward Lavender
#' @keywords internal
#'

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
}


#' @title Check that a list is named
#' @description This function checks that the top level of a list is named (ignoring empty lists if requested). If the list is not named, the function returns a helpful error message. Otherwise, the list is returned unchanged. This is particularly useful within functions that use \code{\link[base]{do.call}} to evaluate lists of arguments.
#' @param arg (optional) A character string which defines the argument of a parent function.
#' @param input A list.
#' @param ignore_empty A logical input which defines whether or not to ignore empty lists.
#' @return The function returns a helpful error message for unnamed lists (ignoring empty lists if requested) or the inputted list unchanged.
#'
#' @source This function is taken from the `utils.add' package (https://github.com/edwardlavender/utils.add). It is defined separately in \code{\link[flapper]{flapper}} to reduce reliance on non-default packages.
#' @author Edward Lavender
#' @keywords internal

check_named_list <- function(arg = deparse(substitute(input)), input, ignore_empty = TRUE) {
  if (!any("list" %in% class(input))) stop(paste0("Argument '", arg, "' must be of class list."), call. = FALSE)
  list_is_empty <- (length(input) == 0)
  if (!list_is_empty | !ignore_empty) {
    if (is.null(names(input)) | any(names(input) %in% "")) {
      msg <- paste0("Argument '", arg, "' must be a named list.")
      stop(msg, call. = FALSE)
    }
  }
  return(input)
}


#' @title Check input datasets
#' @description These functions check input datasets.
#' @param .moorings,.services,.acoustics,.archival User inputs. `NULL` inputs are allowed for `.services` and `.archival`.
#' @return The functions invisibly return the inputted object if all checks are passed, with the following adjustments:
#' * `.moorings$receiver_id` is silently coerced to an integer if numeric;
#' @author Edward Lavender
#' @name check_data
NULL

#' @rdname check_data
#' @keywords internal

check_moorings <- function(.moorings, .class = "data.table") {
  check_inherits(.moorings, .class)
  check_names(
    input = .moorings, req = c("receiver_id", "receiver_start", "receiver_end"),
    extract_names = colnames, type = all
  )
  if (is.numeric(.moorings$receiver_id)) {
    .moorings$receiver_id <- as.integer(.moorings$receiver_id)
  }
  check_inherits(.moorings$receiver_id, "integer")
  if (any(.moorings$receiver_id <= 0)) {
    abort("Argument 'xy$receiver_id' cannot contain receiver IDs <= 0.")
  }
  if (any(duplicated(.moorings$receiver_id))) {
    abort("Argument '.moorings$receiver_id' contains duplicate elements.")
  }
  if (any(is.na(.moorings))) {
    warn("`.moorings` contains NAs and some functions may fail unexpectedly.")
  }
  invisible(.moorings)
}

#' @rdname check_data
#' @keywords internal

check_services <- function(.services, .moorings) {
  if (!is.null(.services)) {
    check_names(
      input = .services, req = c("receiver_id", "service_start", "service_end"),
      extract_names = colnames, type = all)
    if (is.numeric(.services$receiver_id)) {
      .services$receiver_id <- as.integer(.services$receiver_id)
    }
    check_inherits(.services$receiver_id, "integer")
    if (!all(unique(.services$receiver_id) %in% unique(.moorings$receiver_id))) {
      warn("Not all receivers in .services$receiver_id are in .moorings$receiver_id.")
    }
  }
  invisible(.services)
}

#' @rdname check_data
#' @keywords internal

check_acoustics <- function(.acoustics) {
  # Check class
  check_inherits(.acoustics, "data.table")
  # Check required columns
  # TO DO
  # Check sorted
  # TO DO
  invisible(.acoustics)
}

#' @rdname check_data
#' @keywords internal

check_archival <- function(.archival) {
  if (!is.null(.archival)) {
    # Check class
    check_inherits(.archival, "data.table")
    # Check required columns
    # TO DO
    # Check sorted
    # TO DO
  }
  invisible(.archival)
}
