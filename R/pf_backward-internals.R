#' @title PF: Internal functions for backwards pass
#' @description These functions support the backwards pass.
#' @author Edward Lavender
#' @name pf_backward_internals

#' @rdname pf_backward_internals
#' @keywords internal

.pf_backward_dens_mem <- function(.history, .step_dens, ...,
                                  .verbose, .txt) {

  # Set up messages
  t_onset <- Sys.time()
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_backward_dens() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_backward_dens() call ended (@ ", Sys.time(), ").")), add = TRUE)

  # Collate history
  cat_to_cf("... Collating history...")
  dt <- .pf_history_dt(.history)

  # Identify cells sampled at each time step (labelled 'previous')
  cat_to_cf("... Identifying cell pairs...")
  previous <-
    dt |>
    select("timestep", "cell_now", "x_now", "y_now") |>
    group_by(.data$timestep) |>
    dplyr::distinct() |>
    ungroup() |>
    as.data.table()

  # Identify cells sampled at previous time step (labelled 'current')
  # * We need to pair cell_now from t = 50 with cell_now from t = 49 etc.
  # * So we employ a trick & adjust the time steps
  # * We do timestep - 1
  # * So t = 50 becomes t = 49
  # * We then match t = 49 (which is really t 50) with t = 49
  # * And we have thus paired a 'current' cell with the previous cell(s)
  current <-
    previous |>
    mutate(timestep = .data$timestep - 1) |>
    filter(.data$timestep != 0) |>
    as.data.table()

  # Clean previous
  previous <-
    previous |>
    filter(.data$timestep != max(.data$timestep)) |>
    select("timestep", cell_past = "cell_now", x_past = "x_now", y_past = "y_now") |>
    as.data.table()

  # Identify unique cell pairs
  # * Pair by time step to identify cell pairs
  # * Drop time steps & focus on all unique cell pairs
  pairs <-
    current |>
    merge(previous, by = "timestep", allow.cartesian = TRUE) |>
    select(!"timestep") |>
    dplyr::distinct(.data$cell_now, .data$cell_past, .keep_all = TRUE) |>
    as.data.table()

  # Calculate densities between cell pairs
  cat_to_cf("... Calculating densities..")
    pairs |>
    .pf_backward_dens_calc(.step_dens = .step_dens, ...) |>
    .pf_backward_dens_nest()

}

#' @rdname pf_backward_internals
#' @keywords internal

.pf_backward_dens_calc <- function(.pairs, .step_dens, ..., .slice_now = FALSE) {
  # Get 'current' location(s)
  now  <-
    .pairs |>
    select(any_of("cell_now"), "x_now", "y_now") |>
    as.data.table()
  # If .slice_now is TRUE, we select the first row
  # * This option is used in .pf_backward_dens_spark() option 2B 7
  # * ... b/c the 'current' location is always the same
  if (.slice_now) {
    now <- now[1, , drop = FALSE]
  }
  # Get 'past' locations
  past <-
    .pairs |>
    select("cell_past", x_now = "x_past", y_now = "y_past") |>
    as.data.table()
  # Calculate densities
  # * The cell_past column is required for pf_setup_dens_spark() option 2B
  .pairs |>
    mutate(density = .step_dens(.data_now = now, .data_past = past, ...)) |>
    select(any_of("cell_now"), "cell_past", "density") |>
    as.data.table()
}

#' @rdname pf_backward_internals
#' @keywords internal

.pf_backward_dens_nest <- function(.pairs) {
  # Convert to nested list (for fast data extraction)
  split(.pairs,
        by = c("cell_now", "cell_past"),
        keep.by = FALSE, flatten = FALSE)
}

#' @rdname pf_backward_internals
#' @keywords internal

.pf_backward_dens_spark <- function(.history, .step_dens, ...,
                                    .collect = 1e9, .store = NULL,
                                    .cl, .varlist,
                                    .verbose, .txt) {

  #### Check user inputs
  t_onset <- Sys.time()
  if (!inherits(.history, "character") | length(.history) != 1L) {
    abort("`.history` must be a character string if `.in_memory = FALSE`.")
  }
  check_dir(.history)
  check_contents_ext(.history, "parquet")
  rlang::check_installed("sparklyr")
  check_dir_is_empty(.store)

  #### Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_backward_dens() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_backward_dens() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Define spark connection
  cat_to_cf("... Connecting to spark...")
  sc <- sparklyr::spark_connect(master = "local")
  on.exit(sparklyr::spark_disconnect(sc), add = TRUE)
  sparklyr::spark_read_parquet(sc, name = "pf", path = .history, memory = FALSE)

  #### Identify cell pairs
  # Code from .pf_backward_dens_mem() with the following modifications:
  # * Modify initial definition of `previous` to use `sc |> tbl("pf")`
  # * Drop collect (as.data.table()) statements
  # * Use dplyr::left_join() instead of data.table::merge()
  # * Use dplyr::distinct() without names to avoid error ('Can't convert a call to a string.')
  cat_to_cf("... Identifying cell pairs...")

  cat_to_cf("... ... Identifying 'previous' cells...")
  previous <-
    sc |>
    tbl("pf") |>
    select("timestep", "cell_now", "x_now", "y_now") |>
    group_by(.data$timestep) |>
    dplyr::distinct() |>
    ungroup()

  cat_to_cf("... ... Identifying 'current' cells...")
  current <-
    previous |>
    mutate(timestep = .data$timestep - 1) |>
    filter(.data$timestep != 0)

  cat_to_cf("... ... Updating 'previous' cells...")
  previous <-
    previous |>
    filter(.data$timestep != max(.data$timestep)) |>
    select("timestep", cell_past = "cell_now", x_past = "x_now", y_past = "y_now")

  cat_to_cf("... ... Implementing join...")
  pairs <-
    current |>
    left_join(previous, by = "timestep") |>
    select(!"timestep") |>
    dplyr::distinct()

  #### Optionally bring pairs into memory
  in_mem <- FALSE
  n <- pairs |> count() |> pull(.data$n)
  if (n <= .collect) {
    cat_to_cf("... Collecting pairs into memory...")
    in_mem <- TRUE
    pairs <- pairs |> collect()
  }

  #### Calculate densities between cell pairs
  # (A) Calculate densities in memory (copied from pf_setup_dens_mem())
  if (in_mem) {
    cat_to_cf("... Calculating densities...")
    return(
      pairs |>
        .pf_backward_dens_calc(.step_dens = .step_dens, ...) |>
        .pf_backward_dens_nest()
    )

  # (B) Calculate densities not-in-memory
  # NB: An alternative option is to use {spark} again here, but that seems slower
  } else {

    #### Define directories
    cat_to_cf("... Using {arrow}...")
    if (is.null(.store)) {
      abort("`.store` is required.")
    }
    .store_pairs <- file.path(.store, "pairs")
    .store_dens  <- file.path(.store, "density")
    dir.create(.store_pairs)
    dir.create(.store_dens)
    check_dir(.store_pairs)
    check_dir(.store_dens)

    #### Write parquet files (one per cell_now)
    cat_to_cf("... Writing parquet files...")
    pairs |>
      group_by(.data$cell_now) |>
      sparklyr::sdf_coalesce(1) |>
      sparklyr::spark_write_parquet(path = .store_pairs,
                                    partition_by = "cell_now",
                                    mode = "overwrite")

    #### Rename folders and files for convenience
    cat_to_cf("... Cleaning file names...")
    file.remove(file.path(.store_pairs, "_SUCCESS"))
    folder_path <- list.files(.store_pairs, full.names = TRUE)
    folder_name <- basename(folder_path) |> stringr::str_replace_all("cell_now=", "")
    file.rename(folder_path, file.path(.store_pairs, folder_name))
    file_path <- list.files(.store_pairs, full.names = TRUE, recursive = TRUE)
    file_name <- "pf.parquet"
    file.rename(file_path, file.path(.store_pairs, folder_name, file_name))

    #### Calculate densities & define list
    # Identify files for each cell
    cat_to_cf("... Calculating densities iteratively...")
    cells <- as.integer(folder_name)
    input <- file.path(.store_pairs, cells, "pf.parquet")
    output <- file.path(.store_dens, cells, "density.qs")
    # Loop over cells & run calculations
    cl_lapply(seq_len(length(cells)),
              .cl = .cl, .varlist = .varlist,
              .fun = function(i) {
      dens       <- .pf_backward_dens_calc(.pairs = arrow::read_parquet(input[i]),
                                           .step_dens = .step_dens, ...,
                                           .slice_now = TRUE)
      out        <- as.list(dens$density)
      names(out) <- as.character(dens$cell_past)
      dir.create(file.path(.store_dens, cells[i]))
      qs::qsave(out, output[i])
      NULL
    })

  }

  invisible(NULL)

}
