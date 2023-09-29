#' @title PF: path reconstruction
#' @description This function implements the path-reconstruction algorithm.
#' @param .history Particle samples from the particle filter, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object. This must contain columns that define cell samples at each time step (`cell_now`) alongside previous samples (`cell_past`).
#' * An ordered list of file paths (from [`pf_setup_record()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .return A `character` that defines the return format:
#' * `long` specifies a long-format [`data.table`], with three columns:
#'    * `id`---An `integer` that defines each path;
#'    * `timestep`---An `integer` that defines each time step;
#'    * `cell`---An integer that defines each cell;
#' * `wide` specifies a wide-format [`data.table`], with:
#'    * one row for each path;
#'    * one column for each time step (named `x0`, `x1`, etc.);
#' @param .verbose,.con Arguments to monitor function progress (see [`pf_forward()`]).
#'
#' @details The path reconstruction algorithm 'chains' sequential particle samples into movement paths. This function evolved from [`pf_simplify()`](https://edwardlavender.github.io/flapper/reference/pf_simplify.html) in the [`flapper`](https://github.com/edwardlavender/flapper) package. This implementation uses the fast [`collapse::join()`] function.
#'
#' @return The function returns a long- or wide-format [`data.table`] (see `.return`).
#'
#' @example man/examples/pf_path-examples.R
#'
#' @seealso
#' * [`pf_forward()`] and [`pf_backward()`] implement particle filtering.
#' * [`pf_pou()`] maps probability-of-use from particle samples.
#' * [`pf_path()`] builds movement paths from particle samples and [`pf_path_pivot()`] converts wide-format paths into long-format.
#' @author Edward Lavender
#' @export

pf_path <- function(.history,
                    .verbose = TRUE, .con = "",
                    .return = c("long", "wide")){

  # Check user inputs
  t_onset <- Sys.time()
  check_inherits(.history, "list")
  rlang::check_installed("collapse")
  .return <- match.arg(.return)

  # Set up messages
  if (!.verbose & .con != "") {
    warn("Input to `.con` ignored since `.verbose = FALSE`.")
  }
  if (.verbose && .con != "") {
    create_log(.con)
  }
  append_messages <- ifelse(.con == "", FALSE, TRUE)
  cat_to_cf <- function(..., message = .verbose, file = .con, append = append_messages) {
    if (message) cat(paste(..., "\n"), file = .con, append = append)
  }
  cat_to_cf(paste0("patter::pf_path() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_path() call ended (@ ", Sys.time(), ").")), add = TRUE)

  # Set up chain
  cat_to_cf("... Setting up...")
  if (inherits(.history[[1]], "data.frame")) {
    check_names(.history[[1]], c("cell_past", "cell_now"))
    read <- FALSE
  } else {
    read <- TRUE
  }

  # Define history[[1]]
  cat_to_cf("... Processing history[[1]]...")
  if (read) {
    .history[[1]] <- arrow::read_parquet(.history[[1]])
  }
  .history[[1]] <-
    .history[[1]] |>
    select("x0" = "cell_past", "x1" := "cell_now")

  # Define chain text
  cat_to_cf("... Defining chain text...")
  txt <- .pf_path_chain(.history, .read = read)

  # Implement chain
  cat_to_cf("... Evaluating chain text...")
  .pb <- progress::progress_bar$new(total = length(.history))
  .pb$tick(0)
  paths <- eval(parse(text = txt))
  paths[, 1] <- NULL

  # Reorientate paths (long format)
  if (.return == "long") {
    cat_to_cf("... Reorientating matrix via pf_path_pivot()...")
    paths <- pf_path_pivot(paths)
  }

  # Return outputs
  paths
}


#' @title PF: pivot path matrices
#' @description This function converts paths in 'wide format' to paths in 'long format'.
#'
#' @param .mat A wide-format [`data.table`], from [`pf_path()`] with `.return = "wide"`, in which:
#' * columns (`x1`, etc.) represent time steps;
#' * rows represent time steps;
#'
#' @details
#' This function uses the fast [`collapse::pivot()`] function.
#'
#'
#' @return The function returns a [`data.table`] with three columns:
#' * `id`---An `integer` that defines each path (row in `.mat`);
#' * `timestep`---An `integer` that defines each time step (column in `.mat`);
#' * `cell`---An integer that defines each cell (value in `.mat`);
#'
#' @examples
#' # Define a hypothetical set of paths in wide-format
#' require(data.table)
#' mat <- matrix(1:25, ncol = 5)
#' mat <- as.data.table(mat)
#' colnames(mat) <- c("x0", "x1", "x2", "x3", "x4")
#'
#' # Convert to long format
#' pf_path_pivot(mat)
#'
#' @author Edward Lavender
#' @export

pf_path_pivot <- function(.mat) {
  .mat |>
    collapse::pivot() |>
    as.data.table() |>
    mutate(timestep = as.integer(rep(seq_len(ncol(.mat)), each = nrow(.mat))),
           id = as.integer(rep(seq_len(nrow(.mat)), ncol(.mat)))) |>
    select("id", "timestep", cell = "value") |>
    arrange(.data$id, .data$timestep) |>
    as.data.table()
}
