#' @title  [`patter`] for passive acoustic telemetry
#' @description [`patter`] is a re-implementation of the [`flapper`](https://github.com/edwardlavender/flapper) family of algorithms for passive acoustic telemetry that is simpler, faster and better tested than its [predecessor](https://github.com/edwardlavender/flapper). The aim of the package is to facilitate the reconstruction of movement paths and patterns of space use in passive acoustic telemetry systems.
#'
#' # Vignettes
#'
#' For an introduction to [`patter`], use:
#'
#' * `vignette("workflow", package = "patter")` for an overview of the analytical workflow;
#' * `vignette("faqs", package = "patter")` for FAQs;
#'
#' For a full list of all functions, see `help(package = "patter")`.
#'
#' # Datasets
#'
#' For example datasets from the Movement Ecology of Flapper Skate project ([`datasets-mefs`]), see:
#'
#' * [`dat_moorings`] for receiver locations and associated information;
#' * [`dat_acoustics`] for acoustic time series;
#' * [`dat_archival`] for archival (depth) time series;
#' * [`dat_gebco()`] for a bathymetry grid;
#'
#' For example algorithm outputs ([`datasets-algorithms`]), see:
#'
#' * [`dat_obs()`] for an example output dataset from [`acs_setup_obs()`];
#' * [`dat_overlaps()`] for an example output from [`acs_setup_detection_overlaps()`];
#' * [`dat_kernels()`] for an example output from [`acs_setup_detection_kernels()`];
#' * [`dat_pff()`] for an example output from [`pf_forward_1()`];
#' * [`dat_pfb()`] for an example output from [`pf_backward()`];
#' * [`dat_pfp()`] for an example output from [`pf_path()`];
#'
#' # Simulation
#'
#' To simulate passive acoustic telemetry data, see:
#'
#' * [`sim_array()`] to simulate an acoustic array;
#' * [`sim_path_walk()`] to simulate a movement path from a walk model, with the help of:
#'    * [`rtruncgamma()`] and [`rlen()`] to simulate step lengths;
#'    * [`rwn()`], [`rangrw()`] and [`rangcrw()`] to simulate turning angles;
#' * [`sim_detections()`] to simulate detections at receivers, with the help of:
#'    * [`calc_detection_pr_logistic()`] and [`calc_detection_pr()`], which represent example detection probability models;
#'
#' To evaluate model skill in reconstructing simulated patterns, see `skill_()` functions, specifically:
#' * [`skill_mb()`] to calculate mean bias;
#' * [`skill_me()`] to calculate mean error;
#' * [`skill_rmse()`] to calculate root mean squared error;
#' * [`skill_R()`] to calculate Spearman's rank correlation coefficient;
#' * [`skill_d()`] to calculate the index of agreement;
#'
#' # Data preparation
#'
#' For help with data acquisition, processing, checking and preliminary analyses, see the [`flapper`](https://github.com/edwardlavender/flapper) package. This facilitates:
#'
#'    * Simulation;
#'    * Data preparation;
#'    * Spatial operations;
#'    * Distance calculations;
#'    * Movement analyses;
#'
#' Please submit a [feature request](https://github.com/edwardlavender/patter/issues) if you would like functions from [`flapper`](https://github.com/edwardlavender/flapper) in [`patter`](https://github.com/edwardlavender/patter).
#'
#' # Modelling workflow
#'
#' The main thrust of [`patter`](https://github.com/edwardlavender/patter) is the provision of an integrated modelling workflow for reconstructing animal movement paths and emergent patterns of space use in passive acoustic telemetry systems.
#'
#' To implement the centre-of-activity algorithm use:
#'
#' * [`coa()`] to calculate centres of activity;
#'
#' To implement the depth-contour (DC) algorithm, use:
#'
#' * [`dc_setup_model()`] for an example DC model;
#' * [`dc()`] to implement the algorithm;
#'
#' To implement the acoustic-centroid* (AC*) algorithm(s) (e.g., AC and ACDC), use:
#'
#' * [`acs_setup_obs()`] to set up observations;
#' * [`acs_setup_detection_overlaps()`] to set up detection overlaps;
#' * [`acs_setup_detection_pr()`] to define a detection probability model;
#' * [`acs_setup_detection_kernels()`] to set up detection kernels;
#' * [`acs()`] to implement the algorithm(s);
#'
#' To implement particle filtering (PF), use:
#'
#' * [`pf_setup_files()`] to set up files;
#' * [`pf_kick()`] for an example movement model;
#' * [`pf_forward_*()`] functions to implement the forward simulation; namely:
#'      * [`pf_forward_1()`] for the original (two-step) implementation;
#'      * [`pf_forward_2()`] for an integrated implementation;
#' * [`pf_backward()`] to implement the backward pass;
#' * [`pf_coords()`] to collate particle coordinates;
#' * [`pf_path()`] (and [`pf_path_pivot()`]) to reconstruct movement paths;
#' * [`pf_map_pou()`] to map probability-of-use;
#' * [`pf_map_dens()`] to create smooth maps;
#'
#' For home ranges, use:
#'
#' * [`get_hr_prop()`] for a custom range;
#' * [`get_hr_core()`] for the 'core' range;
#' * [`get_hr_home()`] for the 'home' range;
#' * [`get_hr_full()`] for the full range;
#'
#' # Miscellaneous helpers
#'
#' The following convenience functions are also made available to users of [`patter`](https://github.com/edwardlavender/patter):
#'
#' * Operational statistics. See:
#'    * [`make_matrix_receivers()`] to matricise receiver deployment time series;
#'
#' * Distance calculations. See:
#'    * [`dist_along_path()`] to calculate distances along a movement path;
#'    * [`dist_btw_cells()`] to calculate distances between cells on a [`SpatRaster`];
#'    * [`degrees()`] to create circular angles;
#'
#' * `terra` helpers. See:
#'    * [`spatTemplate()`] to create a template [`SpatRaster`];
#'    * [`spatNormalise()`] to normalise a [`SpatRaster`];
#'    * [`as.im.SpatRaster`] and [`as.owin.SpatRaster`] for interfaces to `spatstat` function(s);
#'
#' * Parallelisation. See:
#'    * [`cl_lapply()`] and associated `cl_*() functions for parallelisation routines`;
#'
#' @author Edward Lavender ([ORCID](https://orcid.org/0000-0002-8040-7489))
#' @seealso
#' * For information on the [`flapper`](https://github.com/edwardlavender/flapper) algorithms, see Lavender et al. ([2023](https://doi.org/10.1111/2041-210X.14193)).
#' * For information on [`patter`]'s predecessor, see \url{https://github.com/edwardlavender/flapper}.
#' * For further information on  [`patter`], see \url{https://github.com/edwardlavender/patter}.
#' * For feature requests and bug reports, see \url{https://github.com/edwardlavender/patter/issues}.
#' * For support, contact [edward.lavender@eawag.ch](mailto:edward.lavender@eawag.ch).
#'
#' @references Lavender, E. et al. (2023). An integrative modelling framework for passive acoustic telemetry. Methods in Ecology and Evolution. \url{https://doi.org/10.1111/2041-210X.14193}.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom lifecycle deprecated
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr tbl
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr distinct
#' @importFrom dplyr slice
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr any_of
#' @importFrom dplyr all_of
#' @importFrom dplyr rename
#' @importFrom dplyr n
#' @importFrom dplyr row_number
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr count
#' @importFrom dplyr pull
#' @importFrom dplyr collect
#' @importFrom dplyr lead
#'
#' @importFrom dtplyr lazy_dt
#'
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @importFrom data.table :=
#' @importFrom data.table setcolorder
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table copy
#'
#' @importFrom collapse fnrow
#' @importFrom collapse seq_col
#' @importFrom collapse seq_row
#'
#' @importFrom lubridate `%within%`

NULL
