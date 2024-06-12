test_that("move_*() functions work", {

  # Functions fail when the map has not been exported
  # move_xy() |>
  #   expect_error("'env' does not exist in Julia.")

  # move_xyzd() |>
  #   expect_error("'env' does not exist in Julia.")

  # Set map
  map <- dat_gebco()
  set_map(map)

  # Test move_xy()
  output <- move_xy(dbn_length = "truncated(Gamma(2, 300.0), upper = 500.0)",
                    dbn_angle = "Uniform(0, 2 * pi)")
  expected <- "ModelMoveXY(env, truncated(Gamma(2, 300.0), upper = 500.0), Uniform(0, 2 * pi));"
  expect_equal(output, expected)

  # Test move_xyzd()
  output   <- move_xyzd(dbn_length = "truncated(Gamma(2, 300.0), upper = 500.0)",
                        dbn_angle_delta = "Normal(3, 1.1)",
                        dbn_z_delta = "Normal(3, 3)")
  expected <- "ModelMoveXYZD(env, truncated(Gamma(2, 300.0), upper = 500.0), Normal(3, 1.1), Normal(3, 3));"

  expect_equal(output, expected)

})

