#' @title PF: path reconstruction
#' @description This function implements the path-reconstruction algorithm.
#' @param .history Particle samples from the particle filter, provided either as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object. This must contain columns that define cell samples at each time step (`cell_now`) alongside previous samples (`cell_past`).
#' * An ordered list of file paths (from [`pf_setup_record()`]) that define the directories in which particle samples were written from the forward simulation (as parquet files).
#' @param .bathy (optional) If `.return = "long"`, a bathymetry [`SpatRaster`] can be supplied to define cell coordinates (see [`pf_path_pivot()`]).
#' @param .obs,.cols (optional) If `.return = "long"`, `.obs` and `.cols` are a [`data.table`] and a `character` vector of column names in `.obs` to match onto the output (see [`pf_path_pivot()`]).
#' @param .verbose,.con Arguments to monitor function progress (see [`pf_forward()`]).
#' @param .return A `character` that defines the return format:
#' * `long` specifies a long-format [`data.table`] that defines path IDs, time steps and associated locations (see [`pf_path_pivot()`]).
#' * `wide` specifies a wide-format [`data.table`], with:
#'    * one row for each path;
#'    * one column for each time step (named `x1`, `x2`, etc.);
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
                    .bathy = NULL, .obs = NULL, .cols = NULL,
                    .verbose = TRUE, .con = "",
                    .return = c("long", "wide")){

  # Check user inputs
  t_onset <- Sys.time()
  check_inherits(.history, "list")
  rlang::check_installed("collapse")
  .return <- match.arg(.return)
  if (.return == "long") {
    .pf_path_pivot_checks(.obs, .cols)
  }

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
    select(x0 = "cell_past", x1 = "cell_now")

  # Define chain text
  cat_to_cf("... Defining chain text...")
  txt <- .pf_path_chain(.history, .read = read)

  # Implement chain
  cat_to_cf("... Evaluating chain text...")
  .pb <- progress::progress_bar$new(total = length(.history))
  .pb$tick(0)
  paths <- eval(parse(text = txt))
  paths$x0 <- NULL

  # Reorientate paths (long format)
  if (.return == "long") {
    cat_to_cf("... Reorientating matrix via pf_path_pivot()...")
    paths <- pf_path_pivot(paths, .bathy = .bathy,
                           .obs = .obs, .cols = .cols)
  }

  # Return outputs
  paths
}


#' @title PF: pivot path matrices
#' @description This function converts paths in 'wide format' to paths in 'long format'.
#'
#' @param .mat A wide-format [`data.table`], from [`pf_path()`] with `.return = "wide"`, in which:
#' * columns (`x1`, `x2`, etc.) represent time steps;
#' * rows represent paths;
#' @param .bathy (optional) The bathymetry [`SpatRaster`] (used to extract cell coordinates).
#' @param .obs,.cols (optional) A [`data.table`] and a `character` vector of column names in `.obs` to match onto the output. `.obs` must contain a `timestep` column for matching.
#'
#' @details
#' This function uses the fast [`collapse::pivot()`] function.
#'
#'
#' @return The function returns a [`data.table`] with at least three columns:
#' * `path_id`---An `integer` that defines each path (row in `.mat`);
#' * `timestep`---An `integer` that defines each time step (column in `.mat`);
#' * `cell_id`---An integer that defines each cell (value in `.mat`);
#'
#' If `.bathy` is supplied, the [`data.table`] contains three additional columns:
#' * `cell_x`---A `double` that defines the cell's x coordinate;
#' * `cell_y`---A `double` that defines the cell's y coordinate;
#' * `cell_z`---A `double` that defines the cell's z coordinate;
#'
#' If `.obs` is supplied, the output also contains any columns specified in `.cols`.
#'
#' @examples
#' # Define a hypothetical set of paths in wide-format
#' require(data.table)
#' mat <- matrix(1:25, ncol = 5)
#' mat <- as.data.table(mat)
#' colnames(mat) <- c("x1", "x2", "x3", "x4", "x5")
#'
#' # Convert to long format
#' pf_path_pivot(mat)
#'
#' @author Edward Lavender
#' @export

pf_path_pivot <- function(.mat, .bathy = NULL,
                          .obs = NULL, .cols = NULL) {
  # Check user inputs
  .pf_path_pivot_checks(.obs, .cols)
  # Pivot paths
  p <-
    .mat |>
    collapse::pivot() |>
    as.data.table() |>
    mutate(timestep = as.integer(rep(seq_len(ncol(.mat)), each = nrow(.mat))),
           path_id = as.integer(rep(seq_len(nrow(.mat)), ncol(.mat)))) |>
    select("path_id", "timestep", cell_id = "value") |>
    arrange(.data$path_id, .data$timestep) |>
    as.data.table()
  # Add cell coordinates & depth
  if (!is.null(.bathy)) {
    p <-
      p |>
      mutate(cell_x = terra::xFromCell(.bathy, .data$cell_id),
             cell_y = terra::yFromCell(.bathy, .data$cell_id),
             cell_z = terra::extract(.bathy, .data$cell_id)) |>
      as.data.table()
  }
  # Add columns from .obs by matching by timestep
  if (!is.null(.obs)) {
    for (col in .cols) {
      p[, (col) := .obs[[col]][match(p$timestep, .obs$timestep)]]
      if (any(is.na(p[[col]]))) {
        warn("There are NAs in the {col} column in the output.",
             .envir = environment())
      }
    }
  }
  # Return outputs
  p
}
