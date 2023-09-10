#' @title  [`patter`]: Movement Modelling For Passive Acoustic Telemetry
#' @description [`patter`] is a re-implementation of the [flapper](https://github.com/edwardlavender/flapper) family of algorithms for passive acoustic telemetry that simpler, faster and better tested than its [predecessor](https://github.com/edwardlavender/flapper).
#'
#' # Workflow
#'
#' To implement the depth-contour algorithm, use:
#'
#' * [`dc()`] to implement the algorithm;
#'
#' To implement the acoustic-centroid (AC) or acoustic-container depth-contour (ACDC) algorithms, use:
#'
#' * [`acs_setup_obs()`] to set up observations;
#' * [`acs_setup_detection_containers()`] to set up detection containers;
#' * [`acs_setup_detection_overlaps()`] to set up detection overlaps;
#' * [`acs_setup_detection_kernels()`] to set up detection kernels;
#' * [`ac()`] or [`acdc()`] to implement the algorithm(s);
#'
#' To implement particle filtering, use:
#'
#' * [`pf_setup_record()`] to set up the record;
#' * [`pf()`] to implement particle filtering;
#'
#' @author Edward Lavender ([ORCID](https://orcid.org/0000-0002-8040-7489))
#' @seealso
#' * For information on the [flapper](https://github.com/edwardlavender/flapper) algorithms, see Lavender et al. ([2023](https://doi.org/10.1111/2041-210X.14193)).
#' * For information on [`patter`]'s predecessor, see \url{https://github.com/edwardlavender/flapper}.
#' * For further information on  [`patter`], see \url{https://github.com/edwardlavender/patter}.
#' * For feature requests and bug reports, see \url{https://github.com/edwardlavender/patter/issues}.
#' * For support, contact [edward.lavender@eawag.ch](mailto:[edward.lavender@eawag.ch).
#'
#' @references Lavender, E. et al. (2023). An integrative modelling framework for passive acoustic telemetry. Methods in Ecology and Evolution. \url{https://doi.org/10.1111/2041-210X.14193}.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr slice
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#'
#' @importFrom dtplyr lazy_dt
#'
#' @importFrom data.table :=
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
NULL
