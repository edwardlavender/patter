test_that("model_move_*() functions work", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = FALSE))

  # Functions fail when the map has not been exported
  if (!julia_exists("env")) {
    model_move_xy() |>
      expect_error("'env' does not exist in Julia.")
    model_move_xyz() |>
      expect_error("'env' does not exist in Julia.")
    model_move_cxy() |>
      expect_error("'env' does not exist in Julia.")
    model_move_cxyz() |>
      expect_error("'env' does not exist in Julia.")
  }

  # Set map
  map <- dat_gebco(.return = "character")
  set_map(map)

  # Test model_move_xy()
  output <- model_move_xy(.mobility = "500.0",
                    .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                    .dbn_heading = "Uniform(0.0, 2 * pi)")
  expected <- "ModelMoveXY(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Uniform(0.0, 2 * pi));"
  expect_equal(output, expected)

  # Test model_move_xyz()
  output <- model_move_xyz(.mobility = "500.0",
                     .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                     .dbn_heading = "Uniform(0.0, 2 * pi)",
                     .dbn_z = "truncated(Normal(0.0, 500.0), lower = 0.0, upper = 500.0)")
  expected <- "ModelMoveXYZ(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Uniform(0.0, 2 * pi), truncated(Normal(0.0, 500.0), lower = 0.0, upper = 500.0));"
  expect_equal(output, expected)

  # Test model_move_czy()
  output   <- model_move_cxy(.mobility = "500.0",
                        .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                        .dbn_heading_delta = "Normal(3.0, 1.1)")
  expected <- "ModelMoveCXY(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Normal(3.0, 1.1));"
  expect_equal(output, expected)

  # Test model_move_cxyz()
  output   <- model_move_cxyz(.mobility = "500.0",
                        .dbn_length = "truncated(Gamma(2.0, 300.0), upper = 500.0)",
                        .dbn_heading_delta = "Normal(3.0, 1.1)",
                        .dbn_z_delta = "Normal(3.0, 3.0)")
  expected <- "ModelMoveCXYZ(env, 500.0, truncated(Gamma(2.0, 300.0), upper = 500.0), Normal(3.0, 1.1), Normal(3.0, 3.0));"
  expect_equal(output, expected)

})
