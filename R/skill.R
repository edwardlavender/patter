#' @title Skill: evaluation metrics
#' @description These functions are standard model skill metrics. In [`patter`], they support comparisons of simulated and reconstructed patterns of space use.
#' @param .obs The 'observed' (true) pattern of space use, as a [`SpatRaster`].
#' @param .mod The 'modelled' (reconstructed) pattern of space use, as a [`SpatRaster`].
#' @param .summarise A function, passed to [`terra::global()`], used to summarise [`SpatRaster`] values.
#'
#' @details
#' We follow the mathematical definitions in Lavender et al. ([2022](https://www.doi.org/1038/s41598-022-20254-z)) Supplementary Information Sect. [3.2.1](https://static-content.springer.com/esm/art%3A10.1038%2Fs41598-022-20254-z/MediaObjects/41598_2022_20254_MOESM2_ESM.pdf).
#'
#' * [`skill_mb()`] computes mean bias (if `.summarise = "mean"`).
#' * [`skill_me()`] computes mean error (if `.summarise = "mean"`).
#' * [`skill_rmse()`] computes root mean squared error.
#' * [`skill_R()`] computes Spearman's rank correlation coefficient.
#' * [`skill_d()`] computes the index of agreement.
#'
#' These functions are not memory safe. On Linux, they cannot be used within a Julia session.
#'
#' @return The functions return a number.
#'
#' @example man/examples/example-skill.R
#' @references Lavender, E. et al. (2022). Benthic animal-borne sensors and citizen science combine to validate ocean modelling. Sci. Rep. 12: 16613. \url{https://www.doi.org/1038/s41598-022-20254-z}
#'
#' @seealso
#' * To simulate observations, see `sim_*()` functions (especially [`sim_path_walk()`], [`sim_array()`] and [`sim_observations()`]);
#' * To translate observations into coordinates for mapping patterns of space use, see:
#'     * [`coa()`] to calculate centres of activity;
#'     * [`pf_filter()`] and associates to implement particle filtering algorithms;
#' * To estimate utilisation distributions from simulated data and algorithm outputs, use `map_*()` functions (see [`map_pou()`], [`map_dens()`] and [`map_hr()`]);
#' @author Edward Lavender
#' @name skill

#' @rdname skill
#' @export

skill_mb <- function(.obs, .mod, .summarise = "mean") {
  check_inherits(.obs, "SpatRaster")
  check_inherits(.mod, "SpatRaster")
  bias <- function(.obs, .mod) {
    .mod - .obs
  }
  terra::global(bias(.obs, .mod),
                fun = .summarise,
                na.rm = TRUE)[1, 1]
}

#' @rdname skill
#' @export

skill_me <- function(.obs, .mod, .summarise = "mean") {
  check_inherits(.obs, "SpatRaster")
  check_inherits(.mod, "SpatRaster")
  abs_error <- function(.mod, .obs) {
    abs(.mod - .obs)
  }
  terra::global(abs_error(.mod, .obs),
                fun = .summarise,
                na.rm = TRUE)[1, 1]
}

#' @rdname skill
#' @export

skill_rmse <- function(.obs, .mod) {
  check_inherits(.obs, "SpatRaster")
  check_inherits(.mod, "SpatRaster")
  se <- (.mod - .obs)^2
  mse <- terra::global(se,
                       fun = "mean",
                       na.rm = TRUE)[1, 1]
  sqrt(mse)
}

#' @rdname skill
#' @export

skill_R <- function(.obs, .mod) {
  check_inherits(.obs, "SpatRaster")
  check_inherits(.mod, "SpatRaster")
  dt <- data.table(obs = .obs[], mod = .mod[])
  dt <- dt[stats::complete.cases(dt), ]
  stats::cor(dt$obs, dt$mod, method = "spearman")
}

#' @rdname skill
#' @export

skill_d <- function(.obs, .mod) {
  check_inherits(.obs, "SpatRaster")
  check_inherits(.mod, "SpatRaster")
  # Define helper function, simplified from ie2misc::dr()
  dr <- function(predicted, observed, na.rm = FALSE) {
    c <- 2
    if (anyNA(predicted) | anyNA(observed)) {
      NA
    }
    else {
      if (sum(abs(predicted - observed), na.rm = na.rm) <=
          (c * sum(abs(observed - mean(observed, na.rm = na.rm)),
                   na.rm = na.rm))) {
        1 - (sum(abs(predicted - observed), na.rm = na.rm)/(c *
                                                              sum(abs(observed - mean(observed, na.rm = na.rm)),
                                                                  na.rm = na.rm)))
      }
      else {
        ((c * sum(abs(observed - mean(observed, na.rm = na.rm)),
                  na.rm = na.rm))/sum(abs(predicted - observed),
                                      na.rm = na.rm)) - 1
      }
    }
  }
  # Calculate r
  dt <- data.table(obs = .obs[], mod = .mod[])
  dt <- dt[stats::complete.cases(dt), ]
  dr(dt$mod, dt$obs)
}
