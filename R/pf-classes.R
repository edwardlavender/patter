#' @title PF: [`pf_particles-class`] objects
#' @description An `S3` [`class`] that defines the named `list` returned by [`pf_filter()`] and [`pf_smoother_two_filter()`].
#'
#' @details
#' # Structure
#'
#' [`pf_particles-class`] is a label used to denote outputs from selected functions in [`patter`]. The structure of this class is not strictly defined and primarily exists to streamline documentation. At the time of writing, [`pf_particles-class`] objects may comprise the following elements:
#'
#' * `xinit`---A [`data.table`] of initial particle samples;
#' * `states`---A [`data.table`] of simulated states;
#' * `diagnostics`---A [`data.table`] of diagnostic statistics;
#' * `convergence`---A `logical` variable that defines whether or not the algorithm converged;
#'
#' # `xinit`
#'
#' `xinit` is a [`data.table`] that defines initial particles (from [`sim_states_init()`]). This includes one row for each particle and one column for each state dimension (e.g., `map_value`, `x`, `y`).
#'
#' # `states`
#'
#' `states` is a [`data.table`] that defines simulated particle states, with the following columns:
#' * `path_id`---An `integer` vector that defines the particle index;
#' * `timestep`---An `integer` vector that defines the time step;
#' * `timestamp`---A `POSIXct` vector of time stamps;
#' * Additional columns with the values of each state dimension;
#'
#' Particles are equally weighted, as the `.n_record` particles recorded at each time step are selected by resampling (see [`pf_filter()`]).
#'
#' # `diagnostics`
#'
#' `diagnostics` is a [`data.table`] that stores diagnostic statistics for each time step. This includes `timestep`, `timestamp` and the following columns:
#' * `ess`---A `numeric` vector that defines the effective sample size;
#' * `maxlp`---A `numeric` vector that defines the maximum log posterior;
#'
#' # `convergence`
#'
#' `convergence` is a `logical` variable that defines whether or not the algorithm converged (that is, reached the end of the time series).
#'
#' @author Edward Lavender
#' @inherit assemble seealso
#' @name pf_particles-class
NULL
