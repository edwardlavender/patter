#' @title PF: map probability-of-use
#' @description This function builds a 'probability-of-use' utilisation distribution from (processed) particle samples.
#' @param .history The (processed) particle samples, provided as:
#' * A `list` of [`data.table`]s that define cell samples; i.e., the `history` element of a [`pf-class`] object. This must contain a column that defines cell samples at each time step named `cell_now`.
#' * A `character` string that defines the directory in which particle samples were written (as parquet files).
#' @param .bathy A [`SpatRaster`] that defines the grid for the utilisation distribution. `NAs` on `.bathy` are used as a mask.
#' @param .plot A logical input that defines whether or not to plot the [`SpatRaster`].
#' @param ... If `.plot = TRUE`, `...` is a place holder for additional arguments passed to [`terra::plot()`].
#' @details Probability-of-use is the proportion of samples of each unique cell (out of the total number of samples across all time steps).
#' @return The function returns a [`SpatRaster`] (utilisation distribution) in which cell values define probability-of-use.
#' @example man/examples/pf-pou-examples.R
#' @author Edward Lavender
#' @export

pf_pou <-
  function(.history, .bathy, .plot = TRUE, ...) {

    #### Check user inputs
    check_inherits(.history, c("character", "list"))

    #### Tabulate cell frequencies
    # Prepare samples from parquet files
    if (inherits(.history, "character")) {
      check_dir(.history)
      if (!all((list.files(.history) |> tools::file_ext()) == "parquet")) {
        abort("`.history` contains non parquet files.")
      }
      samples <-
        arrow::open_dataset(.history, format = "parquet") # |> arrow::to_duckdb()
    } else {
      # Prepare samples from pf_*() list
      check_names(.history[[1]], req = "cell_now")
      samples <- .history |> rbindlist()
    }
    # Calculate POU from samples
    pou <-
      samples |>
      dplyr::select(cell = "cell_now") |>
      dplyr::count(.data$cell) |>
      dplyr::collect() |>
      dplyr::mutate(pr = .data$n / sum(.data$n))

    #### Build SpatRaster
    map <- terra::setValues(.bathy, 0)
    map <- terra::mask(map, .bathy)
    map[pou$cell] <- pou$pr
    if (.plot) {
      terra::plot(map, ...)
    }

    #### Return outputs
    map

}

