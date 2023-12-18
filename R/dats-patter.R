#' @title Datasets: example algorithm outputs
#'
#' @description These functions load example outputs from key [`patter`] functions. They are included in the package to streamline function examples and tests.
#'
#' @details
#' * [`dat_pff()`] reads an example output from [`pf_forward()`];
#' * [`dat_pfbk()`] reads an example output from [`pf_backward_killer()`];
#' * [`dat_pfp()`] reads an example output from [`pf_path()`];
#'
#' @examples
#' dat_pff() |> summary()
#' dat_pfbk() |> summary()
#' dat_pfp() |> summary()
#'
#' @source For full details on the algorithm parameters used to generate these datasets, see \url{https://github.com/edwardlavender/patter/blob/main/data-raw/002-add-data-algorithms.R}.
#'
#' @author Edward Lavender
#' @name datasets-algorithms

#' @rdname  datasets-algorithms
#' @export

dat_pff <- function() {
  path <- system.file("extdata", "dat_pff.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(path)
}

#' @rdname datasets-algorithms
#' @export

dat_pfbk <- function() {
  path <- system.file("extdata", "dat_pfbk.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(path)
}

#' @rdname datasets-algorithms
#' @export

dat_pfp <- function() {
  path <- system.file("extdata", "dat_pfp.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(path)
}
