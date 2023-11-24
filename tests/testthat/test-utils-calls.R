test_that("call_*() functions work", {

  # Test call_time()
  time <- "2016-01-01 00:00:00"
  expect_equal(call_time(as.POSIXct(time)), time)

  # Test call_start()
  t_onset <- Sys.time()
  f <- function() {
    call_start(.time = t_onset)
  }
  expect_identical(f(),
                   paste0("`patter::f()` called @ ", str_time(t_onset), "..."))

  # Test call_end()
  t_end <- Sys.time()
  f <- function() {
    call_end(.time = t_end)
  }
  expect_identical(f(),
                   paste0("`patter::f()` call ended @ ", str_time(t_end), "..."))

  # Test call_duration()
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
