if (rlang::is_installed(c("circular", "truncdist"))) {

  require(data.table)
  require(dtplyr)
  require(dplyr, warn.conflicts = FALSE)
  require(circular)
  require(graphics)

  #### Example (1): Implement function with default options
  # The function returns a data.table with simulated locations
  set.seed(1)
  p <- sim_path_walk()
  p

  #### Example (2): Update region for simulation
  p <- sim_path_walk(dat_gebco())
  p

  #### Example (3): Specify origin
  origin <- cbind(710275.3, 6259763)
  p <- sim_path_walk(dat_gebco(), .origin = cbind(710275.3, 6259763))

  #### Example (4): Update number of steps/number of paths
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 100L, .n_path = 5L,
                     .plot = TRUE, .one_page = TRUE)
  # The output contains 5 paths, each of 100 steps
  p |>
    group_by(path_id) |>
    summarise(n = n()) |>
    as.data.table()

  #### Example (5): Modify step length parameters
  hist(rtruncgamma(1e3, .shape = 5, .mobility = 100))
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 100L,
                     .shape = 5, .mobility = 100)
  p

  #### Example (6): Modify .step_length() model
  # Use time-varying step lengths dependent upon some behavioural state
  b <- data.table(x = c(0, 0, 1, 1, 1, 1, 1))
  hist(rtruncgamma(.n = 1e3, .shape = 5, .scale = 5))
  hist(rtruncgamma(.n = 1e3, .shape = 15, .scale = 15))
  sim_length <- function(.n = 1,
                         .prior = NULL, .t = NULL, .state, ...) {
    if (.state$x[.t] == 0L) {
      rtruncgamma(.n = .n, .shape = 5, .scale = 5, .mobility = 50)
    } else if (.state$x[.t] == 1L) {
      rtruncgamma(.n = .n, .shape = 15, .scale = 15, .mobility = 500)
    }
  }
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = nrow(b) + 1,
                     .sim_length = sim_length, .state = b)
  p

  #### Example (7): Update model for turning angles
  # Use biased random walk by modifying the .mu parameter in sim_angle_rw()
  # E.g., To simulate movement north in a straight line, we use .mu = -99
  # (accounting for the longitude of natural origin = -9 in dat_gebco())
  # and .rho = 1:
  rwn(.n = 10, .mu =  -(90 + 9), .rho = 1)
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 10L,
                     .sim_angle = sim_angle_rw, .mu = -(90 + 9), .rho = 1,
                     .one_page = FALSE)
  p

  #### Example (8): Use a correlated random walk model
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 1000L, .n_path = 1L,
                     .sim_angle = sim_angle_crw, .rho = 0.9,
                     .one_page = FALSE)
  # The correlation between sequential angles is close to the simulated value:
  adt <- data.table(a0 = p$angle, a1 = dplyr::lead(p$angle)) |> na.omit()
  cor.circular(degrees(adt$a0), degrees(adt$a1))

  #### Example (9): Use custom step/length or turning angle models
  sim_angle_vmd <- function(.n = 1L, .prior = NULL, ...) {
    as.numeric(
      circular::rvonmises(
        n = .n,
        mu = degrees(0),
        kappa = 0,
        control.circular = list(units = "degrees")
      )
    )
  }
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 1000L,
                     .sim_angle = sim_angle_vmd, .rho = 0.5,
                     .one_page = FALSE)
  p

}
