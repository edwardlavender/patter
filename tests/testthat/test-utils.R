test_that("create_log() works", {

  create_log("some-folder") |>
    expect_error("`con` ('some-folder') should be the path to a text (.txt) file.", fixed = TRUE)

  create_log("some-folder/log.txt") |>
    expect_error("`dirname(con)` ('some-folder') does not exist.", fixed = TRUE)

  f <- tempfile(fileext = ".txt")
  create_log(f)
  expect_true(file.exists(f))

  sink(f)
  cat("hello world\n")
  sink()

  create_log(f) |>
    expect_warning(paste0("`con` ('", f, "`) already exists and is not empty!"), fixed = TRUE)
  unlink(f)

})


test_that("compact() works", {
  expect_equal(compact(list(a = 1, NULL)), list(a = 1))
})


test_that("msg(), warn() and abort() work", {
  m <- function(.x) msg("{.x}", .envir = environment())
  w <- function(.x) warn("{.x}", .envir = environment())
  e <- function(.x) abort("{.x}", .envir = environment())
  x <- "hello world"
  m(x) |> expect_message("hello world", fixed = TRUE)
  w(x) |> expect_warning("hello world", fixed = TRUE)
  e(x) |> expect_error("hello world", fixed = TRUE)
})
