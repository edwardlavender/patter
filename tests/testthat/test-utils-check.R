test_that("check_*() functions work", {

  # check_dir_exists()
  con <- file.path(tempdir(), "check")
  check_dir_exists(tempdir())
  check_dir_exists(con) |> expect_error()

  # check_dir_empty() & check_dir_contents_ext()
  dir.create(con)
  check_dir_empty(con)
  file.create(file.path(con, "file.txt"))
  check_dir_empty(con) |> expect_error()
  check_dir_contents_ext(con, "txt")
  check_dir_contents_ext(con, "blah") |> expect_error()

  # check_inherits()
  check_inherits(iris, "data.frame")
  check_inherits(iris, "list") |> expect_error()

  # check_names()
  check_names(iris, c("Species", "Sepal.Width"))
  check_names(iris, c("blah")) |> expect_error()

  # check_named_list()
  check_named_list(list())
  check_named_list(list(a = "a"))
  check_named_list(list("a")) |> expect_error()
  check_named_list("blah") |> expect_error()

  # check_timeline()
  time <- as.POSIXct("2016-01-01")
  check_timeline(time) |> expect_warning()
  lubridate::tz(time) <- "UTC"
  check_timeline(time)

  # check_POSIXct()
  check_POSIXct(time)
  check_POSIXct(as.Date("2016-01-01")) |> expect_error()

  # check_new_colnames()
  check_new_colnames(iris, "blah")
  check_new_colnames(iris, "Species") |> expect_warning()

  # check_not_null()
  check_not_null(list(a = 1), req = "a")
  check_not_null(list(a = NULL), req = "a") |> expect_error()

  # check_dots_allowed()
  f <- function(...) {
    check_dots_allowed(not_allowed = "a", ...)
  }
  f()
  f(a = 1) |> expect_error()

  # check_dots_for_missing_period()
  check_dots_for_missing_period(list(.a = 1, .b = 1, .c = 1), list(c = 1)) |>
    expect_warning()

  file_cleanup(con)

})

