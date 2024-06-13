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
  file_list(con) |>
    expect_error("No files identified in `.sink`.")
  file.create(file.path(con, "1.txt"))
  file.create(file.path(con, "2.txt"))
  expect_equal(basename(file_list(con)),
               c("1.txt", "2.txt"))
  expect_equal(file_list(con),
               file_list(tempdir(), .folder = "tmp"))
  csv <- file.path(con, "1.csv")
  file.create(csv)
  file_list(con) |>
    expect_error("Multiple file types (extensions) identified in `.sink`. Do you need to pass `pattern` to `list.files()`?", fixed = TRUE)
  file.remove(csv)
  blah <- file.path(con, "blah.txt")
  file.create(blah)
  file_list(con) |>
    suppressWarnings() |>
    expect_error("File names should be '1.{extension}', '2.{extension}', ..., 'N.{extension}'.", fixed = TRUE)

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

