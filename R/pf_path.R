#' @title PF: path reconstruction
#' @description This function implements the path-reconstruction algorithm.
#' @param .history Particle samples from the particle filter, provided in any format accepted by [`.pf_history_list()`]. Particle samples must contain `cell_past` and `cell_now` columns.
#' @param .bathy (optional) If `.return = "long"`, a bathymetry [`SpatRaster`] can be supplied to define cell coordinates (see [`pf_path_pivot()`]).
#' @param .obs,.cols (optional) If `.return = "long"`, `.obs` and `.cols` are a [`data.table`] and a `character` vector of column names in `.obs` to match onto the output (see [`pf_path_pivot()`]).
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#' @param .return A `character` that defines the return format:
#' * `long` specifies a long-format [`data.table`] that defines path IDs, time steps and associated locations (see [`pf_path_pivot()`]).
#' * `wide` specifies a wide-format [`data.table`], with:
#'    * one row for each path;
#'    * one column for each time step (named `x1`, `x2`, etc.);
#'
#' @details The path reconstruction algorithm 'chains' sequential particle samples into movement paths. This function evolved from [`flapper::pf_simplify()`](https://edwardlavender.github.io/flapper/reference/pf_simplify.html). This implementation uses the fast [`collapse::join()`] function.
#'
#' @return The function returns a long- or wide-format [`data.table`] (see `.return`).
#'
#' @example man/examples/pf_path-examples.R
#'
#' @inherit pf_forward seealso
#'
#' @author Edward Lavender
#' @export

pf_path <- function(.history,
                    .bathy = NULL, .obs = NULL, .cols = NULL,
                    .return = c("long", "wide"),
                    .verbose = getOption("patter.verbose")){

  # Check user inputs
  t_onset <- Sys.time()
  check_inherits(.history, "list")
  rlang::check_installed("collapse")
  .return <- match.arg(.return)
  if (.return == "long") {
    .pf_path_pivot_checks(.obs, .cols)
  }

  # Set up messages
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_path", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_path", .start = t_onset, .end = Sys.time())), add = TRUE)

  # Set up chain
  cat_log("... Setting up...")
  .history <- .pf_history_list(.history)
  read     <- .pf_history_read(.history)
  check_names(.pf_history_elm(.history, .elm = 1L, .read = read),
              c("cell_past", "cell_now"))

  # Define history[[1]]
  cat_log("... Processing history[[1]]...")
  .history[[1]] <- .pf_history_elm(.history, .elm = 1L, .read = read,
                                  col_select = c("cell_past", "cell_now"))
  .history[[1]] <-
    .history[[1]] |>
    select(x0 = "cell_past", x1 = "cell_now") |>
    as.data.table()

  # Define chain text
  cat_log("... Defining chain text...")
  txt <- .pf_path_chain(.history, .read = read)

  # Implement chain
  cat_log("... Evaluating chain text...")
  .pb <- pb_init(.min = 0L, .max = length(.history) - 1L)
  paths <- eval(parse(text = txt))
  pb_close(.pb = .pb)
  paths$x0 <- NULL

  # Reorientate paths (long format)
  if (.return == "long") {
    cat_log("... Reorientating matrix via pf_path_pivot()...")
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
