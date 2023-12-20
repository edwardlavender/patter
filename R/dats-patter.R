#' @title Datasets: example algorithm outputs
#'
#' @description These functions load example outputs from key [`patter`] functions. They are included in the package to streamline function examples and tests.
#'
#' @details
#' * [`dat_pff()`] reads an example output from [`pf_setup_obs()`];
#' * [`dat_pff()`] reads an example output from [`pf_forward()`];
#' * [`dat_pfbk()`] reads an example output from [`pf_backward_killer()`];
#' * [`dat_pfp()`] reads an example output from [`pf_path()`];
#' * [`dat_coa()`] reads an example output from [`coa()`];
#'
#' @examples
#' dat_obs() |> dplyr::glimpse()
#' dat_pff() |> summary()
#' dat_pfbk() |> summary()
#' dat_pfp() |> summary()
#' dat_coa() |> summary()
#'
#' @source For full details on the algorithm parameters used to generate these datasets, see \url{https://github.com/edwardlavender/patter/blob/main/data-raw/003-add-data-algorithms.R}.
#'
#' @author Edward Lavender
#' @name datasets-algorithms

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

dat_pfbk <- function() {
  data <- system.file("extdata", "dat_pfbk.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(data)
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
