#' @title Utilities: `cat_*()` functions
#' @description `cat_*()` functions control user output in [`patter`] functions with the `.verbose` argument.
#' @param .fun A `character` string that defines the name of the parent function.
#' @param .verbose A `logical` variable or a `string` that defines the path to a text file.
#' * `.verbose = FALSE` suppresses user outputs;
#' * `.verbose = TRUE` sends user outputs to the console;
#' * `.verbose = file.path("path", "to", "text", "file.txt")` sends user outputs to a `.txt` file;
#'
#' @details These are internal functions.
#' * [`cat_setup()`] sets up messages within functions;
#' * [`cat_init()`] defines a [`cat()`] wrapper based on `.verbose`;
#' * [`cat_log_file()`] validates `.verbose` and, if necessary, creates the `.txt` file;
#'
#' @examples
#' \dontrun{
#'
#' # Define example function
#' wrap <- function(.verbose = getOption("Patter.verbose")) {
#'   # Set up messages and exit handler
#'   cats <- cat_setup(.fun = "wrap", .verbose = .verbose)
#'   on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)
#'   # Run function
#'   cats$cat("Running function...")
#'   Sys.sleep(1)
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

# Set up messages for a function
cat_setup <- function(.fun, .verbose) {
  # Define start time
  t_onset <- Sys.time()
  # Define cat_log() function
  cat_log <- cat_init(.verbose = .verbose)
  # Provide startup message
  cat_log(call_start(.fun = .fun, .start = t_onset))
  # Define exit expression
  exit_expr <- expression(
    cat_log(call_end(.fun = .fun, .start = t_onset, .end = Sys.time()))
  )
  # Define a list of outputs
  list(cat = cat_log,
       exit = exit_expr,
       envir = environment())
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
