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
#' * `convergence`---a `logical` value that defines convergence success;
#' * `time`---a `data.table` of timings;
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

#' # `convergence`
#'
#' For [`pf_forward()`], `convergence` is a `logical` variable that defines whether or not the algorithm successfully converged (i.e., reached the end of the time series).
#'
#' # `time`
#'
#' A `data.table` that records the `start`, `end` and `duration` of algorithm runs (as `POSIXct` and [`difftime`] objects).
#'
#' @author Edward Lavender
#' @inherit pf_forward seealso
#' @name pf_particles-class
NULL
