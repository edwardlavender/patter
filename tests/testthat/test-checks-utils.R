test_that("check_*() utility functions work", {

  #### check_inherits()
  check_inherits(iris, "data.frame")
  check_inherits(iris, "data.table") |>
    expect_error("`iris` must be a data.table.", fixed = TRUE)

  #### check_names()
  check_names(iris, req = c("Species", "Sepal.Width"))
  check_names(iris, req = c("Species", "blah"), type = any)
  check_names(iris, req = c("Species", "blah"), type = all) |>
    expect_error("Argument 'iris' does not contain all required names. One or more of the following name(s) are missing: 'blah'.",
                 fixed = TRUE)

})
