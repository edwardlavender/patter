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
#' * `convergence`---A `logical` variable that defines whether or not the algorithm converged;
#' * `trials`---An integer that defines the number of trials;
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
#' # `trials`
#'
#' `trials` is an `integer` that defines the number of trials.
#'
#' @author Edward Lavender
#' @inherit assemble seealso
#' @name pf_particles-class
NULL


# Build a `pf_particles` class object from Patter.jl outputs
pf_particles <- function(.pf_obj) {

  # Initialise output list
  out <- list()

  # Add Julia outputs
  # * states, diagnostics, convergence
  out <- append(out, julia_eval(glue('Patter.r_get_particles({.pf_obj});')))

  # Process diagnostics data.table
  out$diagnostics <-
    out$diagnostics |>
    as.data.table()

  # Process states data.table
  timestep <- timestamp <- NULL
  out$states <-
    out$states |>
    collapse::join(out$diagnostics[, list(timestep, timestamp)],
                   on = "timestep", verbose = FALSE) |>
    select("path_id", "timestep", "timestamp", everything()) |>
    as.data.table()

  # Update object class
  out <- unclass(out)
  class(out) <- c(class(out), "pf_particles")
  out

}
