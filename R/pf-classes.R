#' @title PF: [`pf_particles-class`] objects
#' @description An S3 class that defines the named `list` returned by [`pf_forward()`] and [`pf_backward_*()`].
#'
#' @details
#'
#' # Structure
#'
#' [`pf_particles-class`] is a label used to denote outputs from selected functions in [`patter`]. The structure of this class is not strictly defined and primarily exists to streamline documentation. At the time of writing, [`pf_particles-class`] objects may comprise the following elements:
#'
#' * `history`---a `list` of particle samples;
#' * `paths`---a [`data.table`] of paths;
#' * `diagnostics`---a [`data.table`] of diagnostics;
#' * `internal`---a `list` of internal objects;
#' * `convergence`---a `logical` value that defines convergence success;
#' * `time`---a `list` of timings;
#'
#' # `history`
#'
#' `history` is a `list` with one element for each time step. Each element is a [`data.table`] that contains location (particle) samples. Each row is a specific sample.
#'
#' In [`pf_forward()`], the columns in this table depend on [`pf_forward()`]'s `.likelihood` function(s) and the `.record$cols` argument (see [`pf_opt_record()`]). By default, the following columns are computed:
#'
#' * `timestep`---an `integer` vector that defines the time step;
#' * `cell_past`---an `integer` vector that identifies the grid cells of previous particle samples;
#' * `x_past`, `y_past`---`numeric` vectors that define the coordinates of previous particle samples;
#' * `cell_now`---an `integer` vector that identifies the grid cells of current particle samples;
#' * `x_now`, `y_now`---`numeric` vectors that define the coordinates of current particle samples;
#' * `lik`---a `numeric` vector of likelihoods;
#' * `weight`---a `numeric` vectors of weights;
#'
#' Coordinates are defined in continuous space by [`pf_rpropose_kick()`] and on the grid by [`pf_rpropose_reachable()`].
#'
#' The likelihood column (`lik`) is the product of the likelihood scores from each likelihood function. (Successive likelihood functions should _update_ this column.)
#'
#' The weight column (`weight`) contains particle weights. Weights are proportional to the product of weights from the previous time step, the likelihood at the current time step (and the movement density, in the case of directed sampling). After resampling, we obtain `.n` equally weighted particles. Weights are normalised to sum to one at each time step.
#'
#' Additional columns may be included in `history` if computed by inputted likelihood functions. At the time of writing, most default likelihood functions calculate likelihoods without defining additional columns. However, [`pf_lik_dc()`] adds `bathy` column, with values extracted from `.dlist$spatial$bathy`.

#' If directed sampling is used, `bathy` and `dens` columns are included where required (after the first time step). `dens` is `numeric` vector that defines the probability density of moving from `(x_past, y_past`) to `(x_now, y_now)`.
#'
#' Since columns are recorded only when required, not all columns are recorded at each time step. To combine [`data.table`]s, use [`.pf_history_dt()`].
#'
#' If `.record$save = FALSE`, `.history` is an empty `list` and the individual [`data.table`]s are written to file in `{.record$sink}/history/` (see [`pf_opt_record()`]).
#'
#' [`pf_backward_killer()`] simply drops dead-ends (rows) from the time series and leaves the columns from [`pf_forward()`] intact (unless `.record$cols` is supplied).
#'
#' [`pf_backward_sampler_v()`] retains or updates the following columns, or a subset thereof as specified by `.record$cols`:
#' * `timestep`
#' * `cell_past`, `x_past`, `y_past`
#' * `cell_now`, `x_now`, `y_now`
#' * `dens`
#'
#' [`pf_backward_sampler_p()`] tracks paths, not particle histories, so this element is empty.
#'
#' # `paths`
#'
#' `paths` is a [`data.table`] of movement paths. Currently, this is only populated by [`pf_backward_sampler_p()`]. By default, the following columns are retained:
#' * `path_id`---an `integer` vector of path IDs;
#' * `timestep`---an `integer` vector of time steps;
#' * `cell_past`, `x_past`, `y_past`, `cell_now`, `x_now`, `y_now`---Cell IDs and coordinates for particle samples;
#' * `dens`---a `numeric` vector of movement densities from `cell_past` into `cell_now`;
#'
#' Additional columns may be included from the forward run, as specified by `.record$cols`.
#'
#' # `diagnostics`
#'
#' `diagnostics` is a `NULL` element. In [`pf_forward()`], we formerly tracked metrics of particle diversity ('particle diagnostics') through time for analysis of convergence issues, sampling sufficiency and other properties (see [`pf_diag-internal`]). This was expensive and is no longer implemented by default and `diagnostics` is now set to `NULL`. You can still track convergence diagnostics following particle proposals and likelihood evaluations by modifying the proposal and/or likelihood functions to write diagnostic statistics to file on the fly (see [`pf_diag-internal`]).
#'
#' For stochastic kicks (implemented via [`.pf_particles_kick()`]), we record:
#' * Proposal diagnostics, following stochastic kicks;
#' * Likelihood diagnostics, following the evaluation of each likelihood function;
#' * Sampling diagnostics, following sampling;
#'
#' For directed sampling (implemented via [`.pf_particles_sampler()`]), we record:
#' * Proposal/likelihood diagnostics, following a combined proposal plus likelihood step;
#' * Sampling diagnostics, following sampling;
#'
#' Diagnostics are tracked for all trials, reversions and reruns (see [`pf_opt_trial()`]) and never replaced. (This is so we can check, for instance, how many trials of a process, such as stochastic kick-based sampling, are required at each time step to generate a sufficient number of valid particles and, in so doing, improve algorithm efficiency.)
#'
#' If `.record$save = TRUE`, particle diagnostics are collated in a single [`data.table`], with the following columns:
#'
#' * `iter_m`---an `integer` that identifies 'manual' iterations. `iter_m` is incremented when `.rerun` is used.
#' * `iter_i`---an `integer` that identifies 'internal' iterations. `iter_i` is incremented when the algorithm jumps back to an earlier time step (see [`pf_opt_record()`]).
#' * `timestep`---an `integer` vector that defines time steps.
#' * `component`---a `character` vector that defines algorithm components:
#'    * `proposal`---the proposal step (at the first time step and for stochastic kicks only);
#'    * `lik_*`---the likelihood step;
#'        * For stochastic kicks, particle diagnostics are given following each (`i`) likelihood evaluation and the components are named by `paste0("lik-", names(.likelihood)[i])`;
#'        * For directed sampling, particle diagnostics for the proposal and likelihood stages are combined under a single `lik-directed` label and not split by likelihood component. A `move-directed` label is included after evaluating movement densities;
#'    * `sample`---the sampling step;
#' * `n`, `nu`, `ess`---particle diagnostics immediately following completion of each algorithm component, computed by internal `.pf_diag_*()` functions:
#'    * `n`---an `integer` vector that defines the number of locations;
#'    * `nu`---an `integer` vector that defines the number of unique locations (see [`.pf_diag_nu()`]).
#'    * `ess`---a `numeric` vector that defines the effective sample size (see [`.pf_diag_ess()`]).
#'
#' In [`pf_forward()`], if `.record$save = FALSE`, `diagnostics` is `NULL` and the individual [`data.table`]s are written to file in `{.record$sink}/diagnostics/` (see [`pf_opt_record()`]). To collate convergence diagnostics from file, use [`pf_diag_convergence()`]. Summary diagnostics can be computed for both [`pf_forward()`] and [`pf_backward_*()`] via [`pf_diag_summary()`].
#'
#' # `internal`
#'
#' For [`pf_forward()`], `internal` is named list of internal objects for algorithm reruns (see `.rerun` and [`.pf_forward_startup()`]).
#'
#' # `convergence`
#'
#' For [`pf_forward()`], `convergence` is a `logical` variable that defines whether or not the algorithm successfully converged (i.e., reached the end of the time series).
#'
#' # `time`
#'
#' A `list` that records the `start`, `end` and `duration` of algorithm runs (as `POSIXct` and [`difftime`] objects). For [`pf_forward()`], this is a nested `list`, with one element for each manual algorithm run (see `.rerun`).
#'
#' @author Edward Lavender
#' @inherit pf_forward seealso
#' @name pf_particles-class
NULL
