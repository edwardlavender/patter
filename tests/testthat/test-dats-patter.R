test_that("Algorithm datasets can be loaded", {

  # Activate tests
  expect_true(1 == 1)

  x <- dat_obs()
  x |> check_inherits("data.table")

  x <- dat_pff()
  x |> check_inherits("list")

  x <- dat_pfbk()
  x |> check_inherits("list")

  x <- dat_pfp()
  x |> check_inherits("data.table")

})
