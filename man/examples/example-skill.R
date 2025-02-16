if (patter_run(.julia = FALSE, .geospatial = TRUE)) {

  set.seed(123L)

  # Generate hypothetical 'observed' utilisation distribution
  obs        <- terra::setValues(dat_gebco(), NA)
  obs[24073] <- 1
  obs        <- terra::distance(obs)
  obs        <- terra::mask(obs, dat_gebco())
  obs        <- obs / terra::global(obs, "sum", na.rm = TRUE)[1, 1]

  # Generate hypothetical modelled' distribution
  mod    <- obs
  mod[]  <- mod[] + rnorm(n = terra::ncell(mod), mean = 0, sd = 1e-5)
  mod    <- terra::mask(mod, dat_gebco())
  mod    <- mod / terra::global(mod, "sum", na.rm = TRUE)[1, 1]

  # Visualise 'observed' versus 'modelled' distributions
  pp <- par(mfrow = c(1, 2))
  terra::plot(obs)
  terra::plot(mod)
  par(pp)

  # Calculate skill metrics
  skill_mb(mod, obs)
  skill_me(mod, obs)
  skill_rmse(mod, obs)
  skill_R(mod, obs)
  skill_d(mod, obs)

}
