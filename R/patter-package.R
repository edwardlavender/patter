#' @title [`patter`] for passive acoustic telemetry
#' @description [`patter`] is a `R` implementation of a forward filtering--backward sampling algorithm for passive acoustic telemetry. This methodology enables the reconstruction of movement paths and patterns of space use in passive acoustic telemetry systems. [`patter`] unifies a suite of methods formerly known as the [`flapper`](https://github.com/edwardlavender/flapper) algorithms (Lavender et al., 2023) and supersedes the experimental [`flapper`](https://github.com/edwardlavender/flapper) package.
#'
#' # Vignettes
#'
#' For an introduction to [`patter`], use:
#'
#' * `vignette("a-methodology", package = "patter")` for a conceptual introduction to the methodology;
#' * `vignette("b-workflow-outline", package = "patter")` for an overview of the workflow;
#' * `vignette("c-workflow-example", package = "patter")` for an example workflow;
#' * `vignette("d-demos", package = "patter")` for more involved demonstrations;
#' * `vignette("e-faqs", package = "patter")` for FAQs;
#'
#' For a full list of all functions, see `help(package = 'patter')`.
#'
#' # Datasets
#'
#' For example datasets from the Movement Ecology of Flapper Skate project ([`datasets-mefs`]), which inspired [`patter`], see:
#'
#' * [`dat_moorings`] for receiver locations and associated information;
#' * [`dat_acoustics`] for acoustic time series;
#' * [`dat_archival`] for archival (depth) time series;
#' * [`dat_gebco()`] for a bathymetry grid;
#'
#' For example algorithm outputs ([`datasets-algorithms`]), see:
#'
#' * [`dat_obs()`] for an example output dataset from [`pf_setup_obs()`];
#' * [`dat_pff()`] for an example output from [`pf_forward()`];
#' * [`dat_pfbk()`] for an example output from [`pf_backward_killer()`];
#' * [`dat_pfp()`] for an example output from [`pf_path()`];
#' * [`dat_coa()`] for an example output from [`coa()`];
#'
#' # Simulation
#'
#' To simulate passive acoustic telemetry data, see:
#'
#' * [`sim_array()`] to simulate an acoustic array;
#' * [`sim_path_walk()`] to simulate a movement path from a walk model;
#' * [`sim_detections()`] to simulate detections at receivers;
#'
#' These functions are supported by a set of simulation helpers, including:
#'
#' * [`calc_detection_pr_logistic()`] and [`calc_detection_pr()`], which are example detection probability models;
#' * [`dtruncgamma()`] [`rtruncgamma()`] and [`rlen()`] for the simulation of step lengths;
#' * [`rwn()`], [`rangrw()`] and [`rangcrw()`] for the simulation of turning angles;
#' * [`cstep()`] and [`dstep()`] for the simulation of steps into new locations;
#'
#' To evaluate model skill in reconstructing simulated patterns, see `skill_()` functions, specifically:
#'
#' * [`skill_mb()`] to calculate mean bias;
#' * [`skill_me()`] to calculate mean error;
#' * [`skill_rmse()`] to calculate root mean squared error;
#' * [`skill_R()`] to calculate Spearman's rank correlation coefficient;
#' * [`skill_d()`] to calculate the index of agreement;
#'
#' # Data exploration
#'
#' For help with data acquisition, processing, checking and preliminary analyses, see the [`flapper`](https://github.com/edwardlavender/flapper) package. This facilitates:
#'
#'    * Data preparation;
#'    * Spatial operations;
#'    * Distance calculations;
#'    * Movement analyses;
#'
#' Please submit a [feature request](https://github.com/edwardlavender/patter/issues) if you would like functions from [`flapper`](https://github.com/edwardlavender/flapper) in [`patter`].
#'
#' # Modelling workflow
#'
#' The main thrust of [`patter`] is the provision of an integrated modelling workflow for reconstructing animal movement paths and emergent patterns of space use in passive acoustic telemetry systems.
#'
#' **To set up data for [`patter`]**, use
#'
#' * [`pat_setup_data()`] to set up data;
#'
#' **To implement the centre-of-activity algorithm**, use:
#'
#' * [`coa()`] to calculate centres of activity;
#'
#' **To implement the particle filter (PF)**, use:
#'
#' * [`pf_setup_obs()`] to set up a timeline of observations;
#' * [`pf_forward()`] to implement the forward simulation;
#'
#' **PF is supported by**:
#'
#' * Proposal functions (see [`pf_propose`]) for the generation of new (candidate) locations, including:
#'     * [`pf_rpropose_kick()`], which uses stochastic kicks;
#'     * [`pf_rpropose_reachable()`], which supports directed sampling;
#'
#' * Likelihood functions (see [`pf_lik`]) for evaluating the likelihood of the data, given proposal locations, including:
#'     * [`acs_filter_land`], which filters proposals on land;
#'     * [`acs_filter_container`], which filters proposals incompatible with acoustic container dynamics;
#'     * [`pf_lik_ac`], which calculates the likelihood of acoustic data;
#'     * [`pf_lik_dc`], which calculates the likelihood of depth observations;
#'
#' * Likelihood helpers, including:
#'     * [`acs_setup_detection_overlaps()`], which pre-calculates detection overlaps;
#'     * [`acs_setup_detection_kernels()`], which pre-calculates detection kernels;
#'     * [`acs_setup_detection_pr()`], which is an example detection probability model;
#'
#' * (Re)sampling functions (see [`pf_sample`]) for the (re)sampling of valid proposal locations, including:
#'     * [`pf_sample_multinomial()`], which implements multinomial resampling;
#'     * [`pf_sample_systematic()`], which implements systematic resampling;
#'
#' * Option functions ([`pf_opt`]) for tuning the forward simulation, including:
#'     * [`pf_opt_trial()`], which sets convergence parameters;
#'     * [`pf_opt_rerun_from()`], which sets re-run parameters;
#'     * [`pf_opt_control()`], which sets control parameters;
#'     * [`pf_opt_record()`], which sets output properties;
#'
#' * Downstream diagnostic functions, namely:
#'     * [`pf_forward_diagnostics()`], which collates particle diagnostics;
#'
#' **To implement the backward pass ([`pf_backward_*()`])**, use:
#'
#' * [`pf_backward_killer()`] to prune dead-ends;
#' * [`pf_backward_killer_diagnostics()`] to summarise particle diagnostics;
#' * [`pf_backward_sampler()`] to run the backward sampler;
#'
#' **For common utility functions**, see:
#'
#' * [`pf_files()`] to list particle-sample files;
#' * [`pf_files_size()`] to measure file size;
#'
#' **For downstream analyses**, use:
#'
#' * [`pf_path()`] (and [`pf_path_pivot()`]) to reconstruct movement paths;
#' * [`pf_coord()`] to collate particle coordinates for mapping;
#'
#' **To map utilisation distributions**, use:
#'
#' * [`map_pou()`] to map probability-of-use;
#' * [`map_dens()`] to create smooth maps using `spatstat`, plus the supporting functions:
#'     * [`as.im.SpatRaster()`], to convert [`SpatRaster`]s to pixel images;
#'     * [`as.owin.SpatRaster()`], to convert  [`SpatRaster`]s to observation windows;
#'     * [`as.owin.sf()`], to convert `sf` objects to observation windows;
#'
#' **For home ranges**, use:
#'
#' * [`map_hr_prop()`] for a custom range;
#' * [`map_hr_core()`] for the 'core' range;
#' * [`map_hr_home()`] for the 'home' range;
#' * [`map_hr_full()`] for the full range;
#'
#' # Miscellaneous helpers
#'
#' The following convenience functions are also made available to users of [`patter`]:
#'
#' * [`dist_along_path()`] to calculate distances along a movement path;
#' * [`degrees()`] to create circular angles;
#' * [`spatTemplate()`] to create a template [`SpatRaster`];
#'
#' # Options
#'
#' For additional options in [`patter`], see:
#'
#' * [`patter-progress`] to monitor function progress;
#'
#' @author Edward Lavender ([ORCID](https://orcid.org/0000-0002-8040-7489))
#' @seealso
#' * For information on the [`flapper`](https://github.com/edwardlavender/flapper) algorithms, see Lavender et al. ([2023](https://doi.org/10.1111/2041-210X.14193)).
#' * For information on [`patter`]'s predecessor, see \url{https://github.com/edwardlavender/flapper}.
#' * For further information on  [`patter`], including useful resources, see \url{https://github.com/edwardlavender/patter}.
#' * For feature requests and bug reports, see \url{https://github.com/edwardlavender/patter/issues}.
#' * For support, contact [edward.lavender@eawag.ch](mailto:edward.lavender@eawag.ch).
#'
#' @references Lavender, E. et al. (2023). An integrative modelling framework for passive acoustic telemetry. Methods in Ecology and Evolution. \url{https://doi.org/10.1111/2041-210X.14193}.
#'
#' @docType package
#' @name patter
NULL

#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils str
#'
#' @importFrom graphics par
#' @importFrom graphics arrows
#' @importFrom graphics points
#' @importFrom graphics hist
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
#'
#' @importFrom arrow schema
#' @importFrom arrow int32

NULL
