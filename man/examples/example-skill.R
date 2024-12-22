if (patter_run(.julia = FALSE, .geospatial = TRUE)) {

  # Define template SpatRasters
  mod <- obs <- terra::rast()
  n <- terra::ncell(mod)
  mod[] <- runif(n)
  obs[] <- mod[] + rnorm(n)

  # Normalise template SpatRasters
  mod   <- mod / terra::global(mod, "sum")[1, 1]
  obs   <- obs / terra::global(obs, "sum")[1, 1]

  # Visualise 'observed' versus 'modelled' distributions
  pp <- par(mfrow = c(1, 2))
  terra::plot(mod)
  terra::plot(obs)
  par(pp)

  # Calculate skill metrics
  skill_mb(mod, obs)
  skill_me(mod, obs)
  skill_rmse(mod, obs)
  skill_R(mod, obs)
  skill_d(mod, obs)

}
