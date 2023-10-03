test_that("par_mf() works", {

  # Test selected examples
  expect_equal(c(2, 3), par_mf(6))
  expect_equal(c(3, 3), par_mf(7))
  expect_equal(c(10, 10), par_mf(100))
  expect_equal(c(10, 10), par_mf(99))
  # Show that the number of rows & columns is always sufficient
  set.seed(1)
  lapply(1:100, function(i) {
    # Sample a number of plots
    s <- sample(1:100, size = 1)
    # Calculate rows and columns
    m <- par_mf(s)
    # Show that the rows and columns are sufficient
    expect_true(prod(m) >= s)
  }) |> invisible()

})
