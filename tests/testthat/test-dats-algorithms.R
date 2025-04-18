test_that("Algorithm datasets can be loaded", {

  # Activate tests
  expect_true(1 == 1)

  x <- dat_path()
  x |> check_inherits("data.table")

  x <- dat_coa()
  x |> check_inherits("data.table")

  x <- dat_pff()
  x |> check_inherits("list")

  x <- dat_pfb()
  x |> check_inherits("list")

  x <- dat_tff()
  x |> check_inherits("list")
  a <- 2L
  b <- nrow(x$diagnostics) - 1L
  expect_true(!all(is.na(x$diagnostics$ess[a:b])))

})
