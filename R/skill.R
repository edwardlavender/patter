#' @title Skill: evaluation metrics
#' @description These function are designed to support comparisons of simulated and reconstructed patterns of space use.
#' @param .obs The 'observed' (true) pattern of space use, as a [`SpatRaster`].
#' @param .mod The 'modelled' (reconstructed) pattern of space use, as a [`SpatRaster`].
#' @param .summarise A function, passed to [`terra::global()`], used to summarise [`SpatRaster`] values.
#'
#' @details
#' * [`skill_mb()`] computes mean bias.
#' * [`skill_me()`] computes mean error.
#' * [`skill_rmse()`] computes root mean squared error.
#' * [`skill_R()`] computes Spearman's rank correlation coefficient.
#' * [`skill_d()`] computes the index of agreement.
#'
#' @return The functions return a number.
#'
#' @examples
#' # Define template SpatRasters
#' mod <- obs <- spatTemplate(.res = 1)
#' n <- terra::ncell(mod)
#' mod[] <- runif(n)
#' obs[] <- mod[] + rnorm(n)
#'
#' # Calculate skill metrics
#' skill_mb(mod, obs)
#' skill_me(mod, obs)
#' skill_rmse(mod, obs)
#' skill_R(mod, obs)
#' skill_d(mod, obs)
#'
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
  # Define helper function from ie2misc::dr()
  dr <- function(predicted, observed, na.rm = FALSE) {
    c <- 2
    if (na.rm == TRUE) {
      if (sum(abs(predicted - observed), na.rm = na.rm) <=
          (c * sum(abs(observed - mean(observed, na.rm = na.rm)), na.rm = na.rm))) {
        1 - (sum(abs(predicted - observed), na.rm = na.rm)/(c * sum(abs(observed - mean(observed, na.rm = na.rm)), na.rm = na.rm)))
      }
      else {
        ((c * sum(abs(observed - mean(observed, na.rm = na.rm)),
                  na.rm = na.rm))/sum(abs(predicted - observed),
                                      na.rm = na.rm)) - 1
      }
    }
    else {
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
  }
  # Calculate r
  dt <- data.table(obs = .obs[], mod = .mod[])
  dt <- dt[stats::complete.cases(dt), ]
  dr(dt$mod, dt$obs)
}
