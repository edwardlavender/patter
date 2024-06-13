test_that("file_*() functions work", {

  # file_path() returns the path, if valid
  con <- tempdir()
  expect_equal(file_path(con), con)

  # as above
  con <- file.path(tempdir(), "tmp")
  dir.create(con)
  expect_equal(file_path(con), con)

  # file_path() returns an error otherwise
  file_path(file.path(tempdir(), "blah")) |>
    expect_error("Path doesn't exist.", fixed = TRUE)

  # file_list() lists files
  file.create(file.path(con, "1.txt"))
  file.create(file.path(con, "2.txt"))
  expect_equal(basename(file_list(con)),
               c("1.txt", "2.txt"))
  expect_equal(file_list(con),
               file_list(tempdir(), .folder = "tmp"))

  # file_cleanup() removes files
  file_cleanup(con)
  expect_false(dir.exists(con))

  # file_size() calculates file sizes
  con <- system.file(package = "patter")
  mb <- file_size(con, recursive = TRUE)
  gb <- file_size(con, recursive = TRUE, .unit = "GB")
  tb <- file_size(con, recursive = TRUE, .unit = "TB")
  mb_true <-
    con |>
    list.files(recursive = TRUE, full.names = TRUE) |>
    file.size() |>
    sum() / 1e6
  expect_equal(mb, mb_true)
  expect_equal(gb, mb_true / 1e3)
  expect_equal(tb, mb_true / 1e6)


})

