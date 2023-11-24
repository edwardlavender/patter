test_that("check_*() utility functions work", {

  #### Check dir
  check_dir(tempdir())
  check_dir("blah") |>
    expect_error("The directory 'blah' does not exist.", fixed = TRUE)

  #### check_inherits()
  check_inherits(iris, "data.frame")
  check_inherits(iris, "data.table") |>
    expect_error("`iris` must be a data.table.", fixed = TRUE)

  #### check_names()
  check_names(iris, req = c("Species", "Sepal.Width"))
  check_names(iris, req = c("Species", "blah"), type = any)
  check_names(iris, req = c("Species", "blah"), type = all) |>
    expect_error("'iris' does not contain all required names. One or more of the following name(s) are missing: 'blah'.",
                 fixed = TRUE)
  check_names(iris, req = c("Species", "blah"), type = all, error = warn) |>
    expect_warning("'iris' does not contain all required names. One or more of the following name(s) are missing: 'blah'.", fixed = TRUE)

  #### check_named_list()
  check_named_list(list(a = 1))
  check_named_list(list())
  check_named_list("blah") |>
    expect_error("Argument '\"blah\"' must be of class list.", fixed = TRUE)
  check_named_list(list(1, 2)) |>
    expect_error("Argument 'list(1, 2)' must be a named list.", fixed = TRUE)

  #### check_dots_allowed()
  f <- function(x, ...) {
    check_dots_allowed(not_allowed = "bad", ...)
    x
  }
  f(1)
  f(1, good = 1)
  f(1, bad = 1) |>
    expect_error("Additional argument(s) (`bad`) have been passed to the function via `...` which are not permitted.", fixed = TRUE)

  #### check_new_colnames()
  check_new_colnames(data.table(x = 1, y = 1), .new = "z") |>
    expect_null()
  check_new_colnames(data.table(x = 1, y = 1, z = 1), .new = "z") |>
    expect_warning("Column(s) in `.data` are being replaced: `z`.", fixed = TRUE)
  check_new_colnames(data.table(x = 1, y = 1, z = 1), .new = c("x", "z")) |>
    expect_warning("Column(s) in `.data` are being replaced: `x`, `z`.", fixed = TRUE)


  })
