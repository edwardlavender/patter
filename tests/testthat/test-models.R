test_that("move_*() functions work", {

  # Functions fail when the map has not been exported
  # move_xy() |>
  #   expect_error("'env' does not exist in Julia.")
  # move_xyz() |>
  #   expect_error("'env' does not exist in Julia.")
  # move_cxy() |>
  #   expect_error("'env' does not exist in Julia.")
  # move_cxyz() |>
  #   expect_error("'env' does not exist in Julia.")

  # Set map
  map <- dat_gebco()
  set_map(map)

  # Test move_xy()
  output <- move_xy(.mobility = "500.0",
                    .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                    .dbn_heading = "Uniform(0.0, 2 * pi)")
  expected <- "ModelMoveXY(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Uniform(0.0, 2 * pi));"
  expect_equal(output, expected)

  # Test move_xyz()
  output <- move_xyz(.mobility = "500.0",
                     .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                     .dbn_heading = "Uniform(0.0, 2 * pi)",
                     .dbn_z = "truncated(Normal(0.0, 500.0), lower = 0.0, upper = 500.0)")
  expected <- "ModelMoveXYZ(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Uniform(0.0, 2 * pi), truncated(Normal(0.0, 500.0), lower = 0.0, upper = 500.0));"
  expect_equal(output, expected)

  # Test move_czy()
  output   <- move_cxy(.mobility = "500.0",
                        .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                        .dbn_heading_delta = "Normal(3.0, 1.1)")
  expected <- "ModelMoveCXY(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Normal(3.0, 1.1));"
  expect_equal(output, expected)

  # Test move_cxyz()
  output   <- move_cxyz(.mobility = "500.0",
                        .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                        .dbn_heading_delta = "Normal(3.0, 1.1)",
                        .dbn_z_delta = "Normal(3.0, 3.0)")
  expected <- "ModelMoveCXYZ(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Normal(3.0, 1.1), Normal(3.0, 3.0));"
  expect_equal(output, expected)

})
