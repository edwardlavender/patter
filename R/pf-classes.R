#' @title PF: [`pf_particles-class`] objects
#' @description An `S3`-[`class`] that defines the named `list` returned by [`pf_filter()`] and [`pf_smoother_two_filter()`].
#'
#' @details
#' # Structure
#'
#' [`pf_particles-class`] is a label used to denote outputs from selected functions in [`patter`]. The structure of this class is not strictly defined and primarily exists to streamline documentation. At the time of writing, [`pf_particles-class`] objects may comprise the following elements:
#'
#' * `states`---A [`data.table`] of simulated states;
#' * `diagnostics`---A [`data.table`] of diagnostic statistics;
#' * `callstats`---A [`data.table`] of call statistics;
#'
#' # `states`
#'
#' `states` is a [`data.table`] that defines simulated particle states, with the following columns:
#' * `path_id`---An `integer` vector that defines the particle index;
#' * `timestep`---An `integer` vector that defines the time step;
#' * `timestamp`---A `POSIXct` vector of time stamps;
#' * Additional columns with the values of each state dimension (e.g., `map_value`, `x`, `y`);
#'
#' Particles are equally weighted, as the `.n_record` particles recorded at each time step are selected by resampling (see [`pf_filter()`]).
#'
#' # `diagnostics`
#'
#' `diagnostics` is a [`data.table`] that stores diagnostic statistics for each time step. This includes `timestep`, `timestamp` and the following columns:
#' * `ess`---A `numeric` vector that defines the effective sample size;
#' * `maxlp`---A `numeric` vector that defines the maximum log posterior;
#'
#' # `callstats`
#'
#' `callstats` is a one-row [`data.table`] that stores call statistics for the function call. This includes the following columns:
#' * `timestamp`---A `POSIXct` value that defines the start time of the function call;
#' * `routine`---A `character` vector that defines the routine (e.g., `"filter: forward"`, `"filter: backward"`, `"smoother: two-filter"`);
#' * `n_particle`---An `integer` that defines the number of particles;
#' * `n_iter`---An `integer` that defines the number of iterations (trials);
#' * `error`---A `character` vector of error message(s);
#' * `convergence`---A `logical` variable that defines whether or not the algorithm converged (that is, reached the end of the time series);
#' * `time`---A `numeric` value that defines the duration (s) of the function call;
#'
#' @author Edward Lavender
#' @inherit assemble seealso
#' @name pf_particles-class
NULL

# Build a `pf_particles` class object from Patter.jl outputs
pf_particles <- function(.pf_obj, .call_start = NULL, call_end = Sys.time()) {

  # Initialise output list
  out <- list()

  # Add Julia outputs
  # * states, diagnostics, convergence
  out <- append(out, julia_eval(glue('Patter.r_get_particles({.pf_obj});')))

  # Process `states` data.table
  if (!is.null(out$states)) {
    out$states <-
      out$states |>
      setDT()
  }

  # Process `diagnostics` data.table
  out$diagnostics <-
    out$diagnostics |>
    setDT()

  # Process `callstats` data.table
  # * Set `n_iter` as integer
  # * This may be passed to R as a float b/c the smoother sets n_iter to NaN
  out$callstats <-
    out$callstats |>
    mutate(n_iter = as.integer(.data$n_iter)) |>
    setDT()

  # (optional) Update timings
  # * In Julia, time stamps do not account for data input/export & other steps
  # * Julia time stamps are updated here to account for the duration of a routine from R
  if (!is.null(.call_start)) {
    timestamp <- time <- NULL
    out$callstats[, timestamp := .call_start]
    out$callstats[, time := as.numeric(difftime(Sys.time(), .call_start, units = "secs"))]
  }

  # Update object class
  out        <- unclass(out)
  class(out) <- c(class(out), "pf_particles")
  out

}
