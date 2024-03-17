#' @title Datasets: example algorithm outputs
#'
#' @description These functions load example outputs from key [`patter`] functions, including those required to implement the forward filtering–backward sampling algorithm. They are included in the package to streamline function examples and tests.
#'
#' @param .folder For [`dat_pff_src()`], `.folder` is `NULL` or a `character` that defines the name of the folder in which outputs are stored (`history` for particle samples or `diagnostics` for particle diagnostics). `NULL` returns the path to the directory containing both `history/` and `diagnostics/`. See the documentation for [`pf_forward()`] and [`pf_particles-class`] objects for further details.
#'
#' @details
#' * [`dat_dlist()`] reads example output from [`pat_setup_data()`];
#' * [`dat_pff()`] reads an example output from [`pf_setup_obs()`];
#' * [`dat_pff()`] reads an example output from [`pf_forward()`];
#' * [`dat_pff_src()`] is the directory in which example `.parquet` files from [`pf_forward()`] are stored;
#' * [`dat_pfbk()`] reads an example output from [`pf_backward_killer()`];
#' * [`dat_pff_src()`] is the directory in which example `.parquet` files from [`pf_backward_killer()`] are stored;
#' * [`dat_pfbs()`] reads an example output from [`pf_backward_sampler_v()`];
#' * [`dat_pfbs_src()`] is the directory in which example `.parquet` files from [`pf_backward_sampler_v()`] are stored;
#' * [`dat_pfp()`] reads an example output from [`pf_path()`];
#' * [`dat_coa()`] reads an example output from [`coa()`];
#'
#' @examples
#' # Load inbuilt datasets
#' dat_dlist() |> summary()
#' dat_obs() |> dplyr::glimpse()
#' dat_pff() |> summary()
#' dat_pfbk() |> summary()
#' dat_pfbs() |> summary()
#' dat_pfp() |> summary()
#' dat_coa() |> summary()
#'
#' # Directory of inbuilt parquet files from `pf_forward()`
#' dat_pff_src()
#' pf_files(dat_pff_src())
#'
#' # Directory of inbuilt parquet files from `pf_backward_killer()`
#' dat_pfbk_src()
#'
#' # Directory of inbuilt parquet files frim `pf_backward_sampler_v()`
#' dat_pfbs_src()
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

dat_pfbk <- function() {
  data <- system.file("extdata", "dat_pfbk.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(data)
}

#' @rdname datasets-algorithms
#' @export

dat_pfbk_src <- function() {
  system.file("extdata", "acpf", "backward", "killer",
              package = "patter", mustWork = TRUE)
}

#' @rdname datasets-algorithms
#' @export

dat_pfbs <- function() {
  data <- system.file("extdata", "dat_pfbs.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(data)
}

#' @rdname datasets-algorithms
#' @export

dat_pfbs_src <- function() {
  system.file("extdata", "acpf", "backward", "sampler",
              package = "patter", mustWork = TRUE)
}

#' @rdname datasets-algorithms
#' @export

dat_pfp <- function() {
  data <- system.file("extdata", "dat_pfp.rds",
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
