test_that("move_*() functions work", {

  # Functions fail when the map has not been exported
  # move_xy() |>
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
  # TO DO

  # Test move_czy()
  # TO DO

  # Test move_cxyz()
  output   <- move_cxyz(.mobility = "500.0",
                        .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                        .dbn_heading_delta = "Normal(3.0, 1.1)",
                        .dbn_z_delta = "Normal(3.0, 3.0)")
  expected <- "ModelMoveCXYZ(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Normal(3.0, 1.1), Normal(3.0, 3.0));"

  expect_equal(output, expected)

})

