test_that("msg(), warn() and abort() work", {
  m <- function(.x) msg("{.x}", .envir = environment())
  w <- function(.x) warn("{.x}", .envir = environment())
  e <- function(.x) abort("{.x}", .envir = environment())
  x <- "hello world"
  m(x) |> expect_message("hello world", fixed = TRUE)
  w(x) |> expect_warning("hello world", fixed = TRUE)
  e(x) |> expect_error("hello world", fixed = TRUE)
})
