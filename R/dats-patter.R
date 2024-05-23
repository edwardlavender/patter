#' @title Datasets: example algorithm outputs
#'
#' @description These functions load example outputs from key [`patter`] functions. They are included in the package to streamline function examples and tests.
#'
#' @details
#' * [`dat_path()`] reads an example output from [`sim_path_walk()`];
#' * [`dat_coa()`] reads an example output from [`coa()`];
#' * [`dat_pff()`] reads an example output from [`pf_filter()`];
#'
#' @examples
#' # Load inbuilt datasets
#' dat_path() |> summary()
#' dat_coa() |> summary()
#' dat_pff() |> summary()
#'
#' @source For full details on the algorithm parameters used to generate these datasets, see \url{https://github.com/edwardlavender/patter/blob/main/data-raw/003-add-data-algorithms.R}.
#'
#' @return The functions return a dataset or a `character` string that defines the path to a dataset. See the corresponding function documentation for full details.
#'
#' @inherit assemble seealso
#' @author Edward Lavender
#' @name datasets-algorithms

#' @rdname datasets-algorithms
#' @export

dat_path <- function() {
  data <- system.file("extdata", "dat_path.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(data)
}

#' @rdname datasets-algorithms
#' @export

dat_coa <- function() {
  data <- system.file("extdata", "dat_coa.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(data)
}

#' @rdname  datasets-algorithms
#' @export

dat_pff <- function() {
  data <- system.file("extdata", "dat_pff.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(data)
}
