test_that("call_*() functions work", {

  # log.txt <- tempfile(fileext = ".txt")
  # cat_to_cf <- cat_helper(.verbose = TRUE, .txt = )

  #### Test call_time()
  time <- "2016-01-01 00:00:00"
  expect_equal(call_time(as.POSIXct(time)), time)

  #### Test call_start()
  # No argument function
  t_onset <- Sys.time()
  f <- function() {
    call_start(.time = t_onset)
  }
  expect_identical(f(),
                   paste0("`patter::f()` called @ ", call_time(t_onset), "..."))
  # Multi-argument function
  f <- function(x, y, ...) {
    call_start(.time = t_onset)
  }
  expect_identical(f(x = 1, y = 2),
                   paste0("`patter::f()` called @ ", call_time(t_onset), "..."))

  #### Test call_end()
  # No argument function
  t_end <- Sys.time()
  g <- function() {
    call_end(.time = t_end)
  }
  expect_identical(g(),
                   paste0("`patter::g()` call ended @ ", call_time(t_end), "..."))
  # Multi-argument function
  g <- function(x = 1, y = 1) {
    call_end(.time = t_end)
  }
  expect_identical(g(),
                   paste0("`patter::g()` call ended @ ", call_time(t_end), "..."))
  expect_identical(g(x = 1),
                   paste0("`patter::g()` call ended @ ", call_time(t_end), "..."))
  expect_identical(g(x = 1, y = 2),
                   paste0("`patter::g()` call ended @ ", call_time(t_end), "..."))

  #### Test call_duration()
  t1 <- as.POSIXct("2016-01-01")
  t2 <- as.POSIXct("2016-02-01")
  expect_equal(
    call_duration(t1, t2),
    list(start = t1,
         end = t2,
         duration = difftime(t2, t1))
  )
  expect_equal(
    call_duration(t1, t2, units = "mins"),
    list(start = t1,
         end = t2,
         duration = difftime(t2, t1, units = "mins"))
  )

})
