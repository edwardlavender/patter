test_that("cat_log_file() works", {

  cat_log_file(.verbose = dat_gebco()) |>
    expect_error("`.verbose` should be a logical variable or a file path.", fixed = TRUE)

  cat_log_file(.verbose = "some-folder") |>
    expect_error("`.verbose` ('some-folder') should be the path to a text (.txt) file.", fixed = TRUE)

  cat_log_file(.verbose = "some-folder/log.txt") |>
    expect_error("`dirname(.verbose)` ('some-folder') does not exist.", fixed = TRUE)

  log.txt <- tempfile(fileext = ".txt")
  cat_log_file(.verbose = log.txt)
  expect_true(file.exists(log.txt))

  sink(log.txt)
  cat("hello world\n")
  sink()

  cat_log_file(.verbose = log.txt) |>
    expect_warning(paste0("`.verbose` ('", log.txt, "`) already exists and is not empty!"), fixed = TRUE)
  unlink(log.txt)

})

test_that("str_items() works", {

  str_items("") |>
    expect_warning("`.items` is an empty string ('').",
                   fixed = TRUE)

  expect_equal(str_items(c("a", "b")),
               "`a`, `b`")

  expect_equal(str_items(c("a", "b"), .quo = "'"),
               "'a', 'b'")

})
