test_that("call_*() functions work", {

  # log.txt <- tempfile(fileext = ".txt")
  # cat_log <- cat_init(.verbose = TRUE)

  #### Test call_time()
  time <- "2016-01-01 00:00:00"
  expect_equal(call_time(as.POSIXct(time)), time)

  #### Test call_start()
  # No argument function
  t_onset <- Sys.time()
  f <- function() {
    call_start(.start = t_onset)
  }
  expect_identical(f(),
                   paste0("`patter::f()` called @ ", call_time(t_onset), "..."))
  # Multi-argument function
  f <- function(x, y, ...) {
    call_start(.start = t_onset)
  }
  expect_identical(f(x = 1, y = 2),
                   paste0("`patter::f()` called @ ", call_time(t_onset), "..."))

  #### Test call_duration()
  expect_equal(call_duration(t_onset, t_onset + 60),
               "1 min(s)")
  expect_equal(call_duration(t_onset, t_onset + 61),
               "1.02 min(s)")

  #### Test call_end()
  # No argument function
  t_end <- t_onset + 60
  g <- function() {
    call_end(.start = t_onset, .end = t_end)
  }
  msg <- paste0("`patter::g()` call ended @ ", call_time(t_end), " (duration: ~1 min(s)).")
  expect_identical(g(), msg)
  # Multi-argument function
  g <- function(x = 1, y = 1) {
    call_end(.start = t_onset, .end = t_end)
  }
  expect_identical(g(), msg)
  expect_identical(g(x = 1), msg)
  expect_identical(g(x = 1, y = 2), msg)

  #### Test call_timings()
  t1 <- as.POSIXct("2016-01-01")
  t2 <- as.POSIXct("2016-02-01")
  expect_equal(
    call_timings(t1, t2),
    data.table(start = t1,
         end = t2,
         duration = difftime(t2, t1))
  )
  expect_equal(
    call_timings(t1, t2, units = "mins"),
    data.table(start = t1,
         end = t2,
         duration = difftime(t2, t1, units = "mins"))
  )

})
