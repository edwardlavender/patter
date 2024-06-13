test_that("list_*() functions work", {
  expect_equal(list_compact(list(a = 1, NULL)), list(a = 1))

  expect_equal(list_merge(list(a = 1), list(b = 2)),
               list(a = 1, b = 2))

  expect_equal(list_merge(list(a = 1), list(a = 2)),
               list(a = 2))

  list_merge(list(a = 1), list(2)) |>
    expect_error("A named list is expected.")

})
