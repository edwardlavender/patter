test_that("check_*() utility functions work", {

  #### check_inherits()
  check_inherits(iris, "data.frame")
  expect_error(check_inherits(iris, "data.table"))

  #### check_names()
  check_names(iris, req = c("Species", "Sepal.Width"))
  check_names(iris, req = c("Species", "blah"), type = any)
  expect_error(check_names(iris, req = c("Species", "blah"), type = all))

})
