test_that("list_compact() works", {
  expect_equal(list_compact(list(a = 1, NULL)), list(a = 1))
})
