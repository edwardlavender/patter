test_that("pb_*() functions work", {

  loop <- function() {
    n <- 10L
    pb <- pb_init(.min = 0L, .max = n)
    for (i in seq_len(n)) {
      Sys.sleep(0.1)
      pb_tick(pb, .t = i)
    }
    pb_close(pb)
  }

  output <- capture.output(loop())
  expect_snapshot_output(cat(output, sep = "\n"))

})


