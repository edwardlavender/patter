#' @title Datasets: example algorithm outputs
#'
#' @description These functions load example outputs from key [`patter`] functions. They are included in the package to streamline function examples and tests.
#'
#' @details
#' * [`dat_obs()`] reads an example input dataset from [`acs_setup_obs()`];
#' * [`dat_containers()`] reads an example output from [`acs_setup_detection_containers()`];
#' * [`dat_overlaps()`] reads an example output from [`acs_setup_detection_overlaps()`];
#' * [`dat_kernels()`] reads an example output from [`acs_setup_detection_kernels()`];
#' * [`dat_ac()`] reads an example output from [`acs()`];
#' * [`dat_pff()`] reads an example output from [`pf_forward_1()`];
#' * [`dat_pfb()`] reads an example output from [`pf_backward()`];
#' * [`dat_pfp()`] reads an example output from [`pf_path()`];
#'
#' @examples
#' dat_obs() |> summary()
#' dat_containers() |> summary()
#' dat_overlaps() |> summary()
#' dat_kernels() |> summary()
#' dat_ac() |> summary()
#' dat_pff() |> summary()
#' dat_pfb() |> summary()
#' dat_pfp() |> summary()
#'
#' @source For full details on the algorithm parameters used to generate these datasets, see \url{https://github.com/edwardlavender/patter/blob/main/data-raw/002-add-data-algorithms.R}.
#'
#' @author Edward Lavender
#' @name datasets-algorithms

#' @rdname datasets-algorithms
#' @export

dat_obs <- function() {
  path <- system.file("extdata", "dat_obs.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(path)
}

#' @rdname datasets-algorithms
#' @export

dat_containers <- function() {
  path <- system.file("extdata", "dat_containers.rds",
                      package = "patter", mustWork = TRUE)
  data <- readRDS(path)
  lapply(data, unwrap_elm)
}

#' @rdname datasets-algorithms
#' @export

dat_overlaps <- function() {
  path <- system.file("extdata", "dat_overlaps.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(path)
}

#' @rdname datasets-algorithms
#' @export

dat_kernels <- function() {
  path <- system.file("extdata", "dat_kernels.rds",
                      package = "patter", mustWork = TRUE)
  data <- readRDS(path)
  data$receiver_specific_kernels <-
    lapply(data$receiver_specific_kernels, unwrap_elm)
  data$receiver_specific_inv_kernels <-
    lapply(data$receiver_specific_inv_kernels, unwrap_elm)
  data$bkg_surface_by_design <-
    lapply(data$bkg_surface_by_design, unwrap_elm)
  data$bkg_inv_surface_by_design <-
    lapply(data$bkg_inv_surface_by_design, unwrap_elm)
  data
}

#' @rdname datasets-algorithms
#' @export

dat_ac <- function() {
  path <- system.file("extdata", "dat_ac.rds",
                      package = "patter", mustWork = TRUE)
  data <- readRDS(path)
  data$record <- lapply(data$record, unwrap_elm)
  data
}

#' @rdname  datasets-algorithms
#' @export

dat_pff <- function() {
  path <- system.file("extdata", "dat_pff.rds",
                      package = "patter", mustWork = TRUE)
  readRDS(path)
}

#' @rdname datasets-algorithms
#' @export

dat_pfb <- function() {
  path <- system.file("extdata", "dat_pfb.rds",
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

#' @title Unwrap [`SpatRaster`]s `list` elements
#' @keywords internal

unwrap_elm <- function(e) {
  if (is.null(e)) {
    return(NULL)
  } else {
    return(terra::unwrap(e))
  }
}
