test_that("skill_*() functions work", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))
  set.seed(1)

  mod <- obs <- terra::rast()
  n <- terra::ncell(mod)
  mod[] <- runif(n)

  obs[] <- mod[] - 10
  expect_equal(skill_mb(mod, obs), -10)
  expect_equal(skill_me(mod, obs), 10)
  expect_equal(skill_rmse(mod, obs), 10)
  expect_equal(skill_R(mod, obs), 1)
  expect_equal(skill_d(mod, obs), -0.94990544)

  mod <- obs <- terra::rast(res = 0.1)
  n <- terra::ncell(mod)
  mod[] <- runif(n)
  obs[] <- mod[] + 0.5 * rnorm(n, mean = 100)

  expect_approx(skill_mb(mod, obs), 50)
  expect_approx(skill_me(mod, obs), 50)
  expect_approx(skill_rmse(mod, obs), 50)
  expect_approx(skill_R(mod, obs), 0.5)

})
