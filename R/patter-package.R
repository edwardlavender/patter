#' @title [`patter`]: particle algorithms for animal movement
#' @description [`patter`] provides particle filtering, smoothing and sampling algorithms for animal movement modelling, with a focus on passive acoustic telemetry systems. This wraps and enhances a fast [`Julia`](https://julialang.org) backend ([`Patter.jl`](https://edwardlavender.github.io/Patter.jl)). The methodology enables the reconstruction of movement paths and patterns of space use. [`patter`] unifies a suite of methods formerly known as the [`flapper`](https://github.com/edwardlavender/flapper) algorithms  and supersedes the experimental [`flapper`](https://github.com/edwardlavender/flapper) package (Lavender et al., [2023](https://doi.org/10.1111/2041-210X.14193)).
#'
#' # Vignettes
#'
#' For an introduction to [`patter`], use:
#'
#' * `vignette("a-methodology", package = "patter")` for a conceptual introduction to the methodology;
#' * `vignette("b-workflow-outline", package = "patter")` for an overview of the workflow;
#'
#' For a full list of all functions, see `help(package = 'patter')`.
#'
#' For a glossary of key arguments, see [`glossary`].
#'
#' # Datasets
#'
#' For example datasets from the Movement Ecology of Flapper Skate project ([`datasets-mefs`]), which inspired [`patter`], see:
#'
#' * [`dat_moorings`] for acoustic receiver deployments;
#' * [`dat_acoustics`] for acoustic time series;
#' * [`dat_archival`] for archival (depth) time series;
#' * [`dat_gebco()`] for a bathymetry grid;
#'
#' To validate new datasets for use with [`patter`], use TO DO.
#'
#' For example algorithm outputs (`datasets-algorithms`), see:
#'
#' * [`dat_path()`] for an example output from [`sim_path_walk()`];
#' * [`dat_coa()`] for an example output from [`coa()`];
#' * [`dat_pff()`] for an example output from [`pf_filter()`];
#'
#' # Set up `Julia`
#'
#' To link [`patter`] and the [`Patter.jl`](https://edwardlavender.github.io/Patter.jl) `Julia` backend, use:
#' * [`julia_connect()`] to connect to `R` to `Julia`;
#' * [`set_seed()`] to set the seed in `R` and `Julia`;
#' * [`set_map()`] to make a [`SpatRaster`] of the study area available in `Julia`;
#'
#' These functions should be run at the start of every `R` session.
#'
#' # Abstract Types
#'
#' [`patter`] is based on three Abstract Types, defined in `Julia`:
#'
#' * [`State`] structures hold the state (location) of an animal at a given time step;
#' * [`ModelMove`] structures hold movement model, used to simulate new states;
#' * [`ModelObs`] structures hold observation model parameters, used to evaluate the correspondence between simulated states and observations;
#'
#' # Simulation
#'
#' To simulate animal movement time series, see:
#'
#' * [`sim_path_walk()`] to simulate a movement path from a walk model (via [`ModelMove`]);
#' * [`sim_array()`] to simulate an acoustic array;
#' * [`sim_observations()`] to simulate observational time series (via [`ModelObs`]);
#'
#'To evaluate model skill in reconstructing simulated patterns, see `skill_*()` functions:
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
#' # Algorithms
#'
#' The main thrust of [`patter`] is the provision of fast, integrated modelling workflow based on particle filtering for reconstructing animal movement paths and emergent patterns of space use from observational time series (with a focus on passive acoustic telemetry systems).
#'
#' **To assemble datasets for particle filtering**, use [`assemble`]`_*()` functions:
#'
#' * [`assemble_timeline()`] assembles a timeline;
#' * [`assemble_acoustics()`] assembles an acoustic time series;
#' * [`assemble_archival()`] assembles an archival time series;
#'
#' Ancillary time series should be structured in the same way for inclusion in the particle filter.
#'
#' **To implement particle filtering (PF) routines**, use:
#'
#' * [`pf_filter()`] to implement the particle filter;
#' * [`pf_smoother_two_filter()`] to implement the two-filter smoother;
#'
#' These functions return [`pf_particles-class`] objects.
#'
#' **For convenience plotting functions**, see:
#'
#' * [`pf_plot_xy()`] to plot particle locations;
#'
#' **For mapping utilisation distributions**, use:
#'
#' * [`map_pou()`] to map probability-of-use;
#' * [`map_dens()`] to create smooth maps using `spatstat`, plus the supporting functions:
#'    * [`as.im.SpatRaster()`], to convert `SpatRaster`s to pixel images;
#'    * [`as.owin.SpatRaster()`], to convert  `SpatRaster`s to observation windows;
#'    * [`as.owin.sf()`], to convert `sf` objects to observation windows;
#' * `map_hr_*()` to map home ranges, specifically:
#'    * [`map_hr_prop()`] for a custom range;
#'    * [`map_hr_core()`] for the 'core' range;
#'    * [`map_hr_home()`] for the 'home' range;
#'    * [`map_hr_full()`] for the full range;
#'
#' # Options
#'
#' For additional options in [`patter`], see:
#'
#' * [`patter-progress`] to monitor function progress;
#'
#' # Miscellaneous
#'
#' * See [`cl_lapply()`] for `R` parallelisation;
#' * See `file_*()` functions (e.g., [`file_list()`]) for system file helpers;
#' * See [`example_setup()`] for functions used to streamline examples;
#'
#' @author Edward Lavender ([ORCID](https://orcid.org/0000-0002-8040-7489))
#' @seealso
#' * For information on the [`flapper`](https://github.com/edwardlavender/flapper) algorithms, see Lavender et al. ([2023](https://doi.org/10.1111/2041-210X.14193)).
#' * For information on [`patter`]'s predecessor, see \url{https://github.com/edwardlavender/flapper}.
#' * For further information on  [`patter`], including useful resources, see \url{https://github.com/edwardlavender/patter}.
#' * For feature requests and bug reports, see \url{https://github.com/edwardlavender/patter/issues}.
#' * For the `Julia` backend, see \url{https://edwardlavender.github.io/Patter.jl}.
#' * For support, contact [edward.lavender@eawag.ch](mailto:edward.lavender@eawag.ch).
#'
#' @references Lavender, E. et al. (2023). An integrative modelling framework for passive acoustic telemetry. Methods in Ecology and Evolution. \url{https://doi.org/10.1111/2041-210X.14193}.
#'
#' @name patter
"_PACKAGE"

#' @importFrom collapse join
#' @importFrom collapse fndistinct
#' @importFrom collapse fmatch
#' @importFrom collapse fnrow
#' @importFrom collapse seq_row
#'
#' @importFrom data.table copy
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table .N
#' @importFrom data.table rbindlist
#' @importFrom data.table rleid
#' @importFrom data.table :=
#'
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr reframe
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#'
#' @importFrom dtplyr lazy_dt
#'
#' @importFrom glue glue
#'
#' @importFrom graphics arrows
#' @importFrom graphics hist
#' @importFrom graphics par
#' @importFrom graphics points
#'
#' @importFrom grDevices hcl.colors
#'
#' @importFrom JuliaCall julia_assign
#' @importFrom JuliaCall julia_command
#' @importFrom JuliaCall julia_eval
#' @importFrom JuliaCall julia_exists
#' @importFrom JuliaCall julia_installed_package
#' @importFrom JuliaCall julia_install_package
#' @importFrom JuliaCall julia_update_package
#' @importFrom JuliaCall julia_library
#' @importFrom JuliaCall julia_setup
#' @importFrom JuliaCall julia_source
#'
#' @importFrom lubridate interval
#' @importFrom lubridate int_overlaps
#' @importFrom lubridate int_start int_end
#' @importFrom lubridate round_date
#' @importFrom lubridate tz
#' @importFrom lubridate %within%
#'
#' @importFrom rlang .data
#'
#' @importFrom stats runif
#'
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils str

NULL
