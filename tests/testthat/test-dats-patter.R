test_that("Algorithm datasets can be loaded", {

  # Activate tests
  expect_true(1 == 1)

  # Check algorithm outputs can be loaded
  # Check SpatRasters have been correctly unwrapped
  # (* Plot SpatRasters: this will fail if not unwrapped

  x <- dat_obs()
  x |> check_inherits("data.table")

  x <- dat_overlaps()
  x |> check_inherits("list")

  x <- dat_kernels()
  x$array_design |> check_inherits("data.frame")
  x$receiver_specific_kernels[[4]] |> terra::plot()
  x$receiver_specific_inv_kernels[[4]] |> terra::plot()
  x$array_design_by_date |> check_inherits("list")
  x$bkg_surface_by_design[[1]] |> terra::plot()
  x$bkg_inv_surface_by_design[[1]] |> terra::plot()

  x <- dat_ac()
  x |> check_inherits("list")
  x$record[[1]] |> terra::plot()

  x <- dat_pff()
  x |> check_inherits("list")

  x <- dat_pfb()
  x |> check_inherits("list")

  x <- dat_pfp()
  x |> check_inherits("data.table")

})
