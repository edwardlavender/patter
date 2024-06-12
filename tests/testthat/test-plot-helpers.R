test_that("one_page() works", {
  expect_null(one_page(FALSE))
  pp <- one_page(TRUE, 5)
  expect_equal(list(mfrow = c(1, 1)), pp)
  par(pp)
  pp <- one_page(TRUE, 10)
  expect_equal(list(mfrow = c(1, 1)), pp)
  par(pp)
  one_page(TRUE, 30) |>
    expect_warning("`.one_page = TRUE` but there are more than 25 plots.",
                   fixed = TRUE)
})

test_that("par_mf() works", {

  # Test selected examples
  expect_equal(c(1, 1), par_mf(0))
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

test_that("add_sp_path() works", {
  skip_on_ci()
  skip_on_os(c("windows", "linux", "solaris"))
  code <- expression(
    {
      plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10))
      xy <- cbind(1:9, 1:9)
      add_sp_path(xy)
    }
  )
  expect_snapshot_file(snapshot_png(code),
                       "add_sp_path.png")
})
