#' @title PF helper: internal checks
#' @description These are internal check functions.
#' @author Edward Lavender
#' @name pf_check
NULL

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
        dplyr::collect()
    }
    return(out)
  }

  # Return error on failure
  abort("`.history` should be the path to parquet files or a list of data.tables.")
}

#' @title PF: path reconstruction helpers
#' @description These functions facilitate the reconstruction of movement paths.
#' @param .data,.current.,t.,pb Arguments for `.pf_path_join`.
#' @param .history,.read Arguments for `.pf_path_chain`.
#' @author Edward Lavender
#' @name pf_path_

#' @rdname pf_path_
#' @keywords internal

.pf_path_join <-
  function(.data, .current, .t, .pb = NULL) {
    # Monitor progress
    # print(.t)
    # Use Sys.sleep() for testing (e.g., visualise progress bar)
    # Sys.sleep(0.1)
    if (!is.null(.pb)) {
      .pb$tick()
    }
    collapse::join(.data,
                   .current |>
                     select("x{.t - 1}" := "cell_past", "x{.t}" := "cell_now") |>
                     as.data.table(),
                   how = "right",
                   validate = "m:m",
                   on = paste0("x", .t - 1),
                   verbose = FALSE)
  }

#' @rdname pf_path_
#' @keywords internal

.pf_path_chain <-
  function(.history, .read){
    if (length(.history) <= 2L) {
      abort("There are <= 2 steps in the time series.")
    }
    index <- 2:length(.history)
    if (.read) {
      history_for_index <- paste0("arrow::read_parquet(.history[[", index, "]])")
    } else {
      history_for_index <- paste0(".history[[", index, "]]")
    }
    paste0(
      ".history[[1]] |> \n",
      paste(paste0("  .pf_path_join(", history_for_index, ", .t = ", index, ", .pb = .pb)"), collapse = " |> \n")
    )
  }
