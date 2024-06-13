test_that("check_*() functions work", {

  con <- file.path(tempdir(), "check")

  check_dir_exists(tempdir())
  check_dir_exists(con) |> expect_error()

  dir.create(con)
  check_dir_empty(con)

  file.create(file.path(con, "file.txt"))
  check_dir_contents_ext(con, "txt")
  check_dir_contents_ext(con, "blah") |> expect_error()

  check_inherits(iris, "data.frame")
  check_inherits(iris, "list") |> expect_error()

  check_names(iris, c("Species", "Sepal.Width"))
  check_names(iris, c("blah")) |> expect_error()

  check_named_list(list())
  check_named_list(list(a = "a"))
  check_named_list(list("a")) |> expect_error()

  time <- as.POSIXct("2016-01-01")
  check_timeline(time) |> expect_warning()
  lubridate::tz(time) <- "UTC"
  check_timeline(time)

  check_POSIXct(time)
  check_POSIXct(as.Date("2016-01-01")) |> expect_error()

  check_new_colnames(iris, "blah")
  check_new_colnames(iris, "Species") |> expect_warning()

  check_not_null(list(a = 1), req = "a")
  check_not_null(list(a = NULL), req = "a") |> expect_error()

  # check_dots_allowed()
  # check_dots_for_missing_period()

  file_cleanup(con)

})

