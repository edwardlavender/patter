test_that("pb_*() functions work", {

  pbo <- pbapply::pboptions(type = "txt")

  loop <- function() {
    n <- 10L
    pb <- pb_init(.min = 0L, .max = n)
    for (i in seq_len(n)) {
      Sys.sleep(0.05)
      pb_tick(pb, .t = i)
    }
    pb_close(pb)
  }

  output <- capture.output(loop())
  expected <- "\r  |                                                        \r  |                                                  |   0%\r  |                                                        \r  |+++++                                             |  10%\r  |                                                        \r  |++++++++++                                        |  20%\r  |                                                        \r  |+++++++++++++++                                   |  30%\r  |                                                        \r  |++++++++++++++++++++                              |  40%\r  |                                                        \r  |+++++++++++++++++++++++++                         |  50%\r  |                                                        \r  |++++++++++++++++++++++++++++++                    |  60%\r  |                                                        \r  |+++++++++++++++++++++++++++++++++++               |  70%\r  |                                                        \r  |++++++++++++++++++++++++++++++++++++++++          |  80%\r  |                                                        \r  |+++++++++++++++++++++++++++++++++++++++++++++     |  90%\r  |                                                        \r  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%"

  # Check expected output
  expect_equal(output, expected)

  pbapply::pboptions(pbo)

})


