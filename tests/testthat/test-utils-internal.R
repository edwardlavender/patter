#### Signals
test_that("msg(), warn() and abort() work", {
  m <- function(.x) msg("{.x}", .envir = environment())
  w <- function(.x) warn("{.x}", .envir = environment())
  e <- function(.x) abort("{.x}", .envir = environment())
  x <- "hello world"
  m(x) |> expect_message("hello world", fixed = TRUE)
  w(x) |> expect_warning("hello world", fixed = TRUE)
  e(x) |> expect_error("hello world", fixed = TRUE)
})

#### cat_*() functions
test_that("cat_log_file() works", {

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

#### list_*() functions
test_that("list_compact() works", {
  expect_equal(list_compact(list(a = 1, NULL)), list(a = 1))
})

#### colProds.matrix()
