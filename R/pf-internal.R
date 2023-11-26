#' @title PF helper: internal checks
#' @description These are internal check functions.
#' @author Edward Lavender
#' @name pf_check
NULL

#' @rdname pf_check
#' @keywords internal

# Collate .pf_checks() for pf_forward_2()
.pf_checks <- function(inputs = match.call()[-1L], defaults = formals(), dots) {
  .pf_check_obs(inputs$.obs)
  if (!is.null(inputs$.moorings)) {
    rlang::check_installed("Rfast")
    check_names(inputs$.obs, c("date", "detection_id", "detection",
                               "receiver_id", "receiver_id_next",
                               "buffer_future_incl_gamma"))
  }
  if (!inputs$.save_history && is.null(inputs$.write_history)) {
    abort("`.save_history = FALSE` and `.write_history = NULL`. There is nothing to do.")
  }
  .pf_check_write_history(formals$.write_history)
  check_dots_for_missing_period(formals, dots)
}


#' @rdname pf_check
#' @keywords internal

.pf_check_obs <- function(.obs) {
  if (inherits(.obs, "data.frame") & !inherits(.obs, "data.table")) {
    .obs <- as.data.table(.obs)
  }
  check_inherits(.obs, "data.table")
  if (!rlang::has_name(.obs, "timestep")) {
    abort("`.obs` should be a data.table with a `timestep` column. ")
  }
  if (is.unsorted(.obs$timestep)) {
    abort("`.obs$timestep` is not sorted.")
  }
  .obs
}

#' @rdname pf_check
#' @keywords internal

.pf_check_write_history <- function(.write_history, .element = "sink") {
  if (!is.null(.write_history)) {
    check_named_list(.write_history)
    check_names(.write_history, .element)
    if (length(.write_history[[.element]]) != 1L) {
      abort("`.write_history${.element}` should be a single directory in which to write files.",
            .envir = environment())
    }
    check_dir(.write_history[[.element]])
    if (length(list.files(.write_history[[.element]])) != 0L) {
      warn("`.write_history${.element}` ('{.write_history[[.element]]}') is not an empty directory.",
           .envir = environment())
    }
  }
  .write_history[[.element]]
}

#' @rdname pf_check
#' @keywords internal

.pf_check_rows <- function(.data, .filter, .t) {
  fail <- FALSE
  if (collapse::fnrow(.data) == 0L) {
    fail <- TRUE
    msg("There are no particles that pass the {.filter} filter at time {.t}. `history` returned up to this point.", .envir = environment())
  }
  fail
}

#' @rdname pf_check
#' @keywords internal

.pf_path_pivot_checks <- function(.obs, .cols) {
  if (is.null(.obs) & !is.null(.cols)) {
    .cols <- NULL
    warn("`.obs = NULL` so input to `.cols` is ignored.")
  }
  if (!is.null(.obs)) {
    if (!rlang::has_name(.obs, "timestep")) {
      abort("`.obs` must have a `timestep` column.")
    }
    if (is.null(.cols)) {
      abort("You must specify the columns in `.obs` required in the output (via `.cols`).")
    }
    check_inherits(.cols, "character")
    if (!all(.cols %in% colnames(.obs))) {
      abort("All elements in `.cols` must be column names in `.obs`.")
    }
  }
}

#' @title PF: Compile particle histories into a [`data.table`]
#' @description This function compiles particle histories into a [`data.table`].
#' @param .history The particle samples, supplied as:
#' * A [`pf-class`] object;
#' * The `history` element of a [`pf-class`] object;
#' * A string that defines the directory containing parquet files;
#' * A list of strings that defines parquet files;
#' @author Edward Lavender
#' @keywords internal

.pf_history_dt <- function(.history, .collect = TRUE) {

  # Handle .pf objects
  if (inherits(.history, "pf")) {
    .history <- .history$history
  }

  # Handle list of data.table inputs
  if (inherits(.history, "list") && inherits(.history[[1]], "data.table")) {
    check_names(.history[[1]], "cell_now")
    out <- .history |> rbindlist()
    return(out)
  }

  # Handle list of file paths
  if (inherits(.history, "list") &&
      inherits(.history[[1]], "character") &&
      length(.history[[1]] == 1L) &&
      file.exists(.history[[1]])) {

    # Option 1: Read individual files
    out <-
      .history |>
      lapply(arrow::read_parquet) |>
      rbindlist()
    return(out)

    # Option 2: Open dataset
    # For speed, assume that all files are located in the same directory
    # .history <- dirname(.history[[1]])
  }

  # Handle folder input
  if (length(.history) == 1L) {
    check_dir(.history)
    check_contents_ext(.history, "parquet")
    out <-
      .history |>
      arrow::open_dataset()
    if (.collect) {
      out <-
        out |>
        arrange(.data$timestep) |>
        collect()
    }
    return(out)
  }

  # Return error on failure
  abort("`.history` should be the path to parquet files or a list of data.tables.")
}
