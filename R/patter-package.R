#' @title  [`patter`]: Movement modelling for passive acoustic telemetry
#' @description [`patter`] is a re-implementation of the [`flapper`](https://github.com/edwardlavender/flapper) family of algorithms for passive acoustic telemetry that simpler, faster and better tested than its [predecessor](https://github.com/edwardlavender/flapper).
#'
#' # Datasets
#'
#' For example datasets, see:
#'
#' * [`dat_moorings`] for receiver locations and associated information;
#' * [`dat_acoustics`] for acoustic time series;
#' * [`dat_archival`] for archival (depth) time series;
#' * [`dat_gebco()`] for a bathymetry grid;
#'
#' # Workflow
#'
#' To implement the depth-contour algorithm, use:
#'
#' * [`dc_setup_model()`] for an example DC model;
#' * [`dc()`] to implement the algorithm;
#'
#' To implement the AC* algorithms, use:
#'
#' * [`acs_setup_obs()`] to set up observations;
#' * [`acs_setup_detection_containers()`] to set up detection containers;
#' * [`acs_setup_detection_overlaps()`] to set up detection overlaps;
#' * [`acs_setup_detection_kernels()`] to set up detection kernels;
#' * [`acs()`] to implement the algorithm(s);
#'
#' To implement particle filtering, use:
#'
#' * [`pf_setup_files()`] to set up files;
#' * [`pf_kick()`] for an example movement model;
#' * [`pf_forward()`] to implement the forward simulation;
#' * [`pf_backward()`] to implement the backward pass;
#' * [`pf_pou()`] to map probability-of-use;
#' * [`pf_path()`] (and [`pf_path_pivot()`]) to reconstruct movement paths;
#'
#' For home ranges, use:
#'
#' * [`get_hr_prop()`] for a custom range;
#' * [`get_hr_core()`] for the 'core' range;
#' * [`get_hr_home()`] for the 'home' range;
#' * [`get_hr_full()`] for the full range;
#'
#' For supporting functions, see `help(package = 'patter')`:
#'
#' * [`make_matrix_receivers()`] matricises receiver deployment time series;
#' * [`normalise()`] normalises a [`SpatRaster`];
#' * `cl_*()` functions for parallelisation helpers;
#'
#' @author Edward Lavender ([ORCID](https://orcid.org/0000-0002-8040-7489))
#' @seealso
#' * For information on the [`flapper`](https://github.com/edwardlavender/flapper) algorithms, see Lavender et al. ([2023](https://doi.org/10.1111/2041-210X.14193)).
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
#' @importFrom dplyr n
#' @importFrom dplyr row_number
#'
#' @importFrom dtplyr lazy_dt
#'
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @importFrom data.table :=
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD

NULL
