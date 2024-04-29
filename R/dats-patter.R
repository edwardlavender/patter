#' @title Datasets: example algorithm outputs
#'
#' @description These functions load example outputs from key [`patter`] functions. They are included in the package to streamline function examples and tests.
#'
#' @details
#' * [`dat_dlist()`] reads example output from [`pat_setup_data()`];
#' * [`dat_pff()`] reads an example output from [`pf_setup_obs()`];
#' * [`dat_pff()`] reads an example output from [`pf_forward()`];
#' * [`dat_pff_src()`] is the directory in which example `.parquet` files from [`pf_forward()`] are stored;
#' * [`dat_coa()`] reads an example output from [`coa()`];
#'
#' @examples
#' # Load inbuilt datasets
#' dat_dlist() |> summary()
#' dat_obs() |> dplyr::glimpse()
#' dat_pff() |> summary()
#' dat_coa() |> summary()
#'
#' # Directory of inbuilt parquet files from `pf_forward()`
#' dat_pff_src()
#' pf_files(dat_pff_src())
#'
#' @source For full details on the algorithm parameters used to generate these datasets, see \url{https://github.com/edwardlavender/patter/blob/main/data-raw/003-add-data-algorithms.R}.
#'
#' @return The functions return a dataset or a `character` string that defines the path to a dataset. See the corresponding function documentation for full details.
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @name datasets-algorithms

#' @rdname  datasets-algorithms
#' @export

dat_dlist <- function() {
  data <- system.file("extdata", "dat_dlist.rds",
                      package = "patter", mustWork = TRUE)
  data <- readRDS(data)
  data$spatial$bathy <- terra::unwrap(data$spatial$bathy)
  data
}

#' @rdname  datasets-algorithms
#' @export

dat_obs <- function() {
  data <- system.file("extdata", "dat_obs.rds",
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

#' @rdname datasets-algorithms
#' @export

dat_pff_src <- function() {
  system.file("extdata", "acpf", "forward",
              package = "patter", mustWork = TRUE)
}

#' @rdname datasets-algorithms
#' @export

dat_coa <- function() {
  data <- system.file("extdata", "dat_coa.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(data)
}
