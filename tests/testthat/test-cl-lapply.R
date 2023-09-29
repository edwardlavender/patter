test_that("cl_*() helpers work", {

  # Check cl_check()
  cl_check(.cl = NULL, .varlist = "blah") |>
    expect_warning("`.cl` is NULL: input to `.varlist` ignored.",
                   fixed = TRUE)
  cl_check(.cl = 2L, .varlist = "blah") |>
    expect_warning("`.cl` is an integer: input to `.varlist` ignored.",
                   fixed = TRUE)
  if (.Platform$OS.type == "windows") {
    cl_check(.cl = 2L) |>
      expect_warning("Integer specifications for `.cl` (i.e., forking) on Windows are not supported.",
                     fixed = TRUE)
  }

  # Check cl_cores()
  cl_cores(NULL) |> expect_equal(1L)
  cl_cores(1L) |> expect_equal(1L)
  cl_cores(2L) |> expect_equal(2L)
  cl <- parallel::makeCluster(2L)
  cl_cores(parallel::makeCluster(2L)) |> expect_equal(2L)
  cl_stop(cl)
  cl <- parallel::makePSOCKcluster(2L)
  cl_cores(cl) |> expect_equal(2L)
  cl_stop(cl)
  cl <- parallel::makeForkCluster(2L)
  cl_cores(cl) |> expect_equal(2L)
  cl_stop()

  # Check cl_chunks()
  cl_chunks(NULL, 10) |>
    expect_equal(list(1:10))
  cl_chunks(2L, 10) |>
    expect_equal(list(1:5L, 6:10L))

  # Check cl_export()
  cl_export()
  f <- function(.input){
    .input + 1
  }
  cl <- parallel::makeCluster(2L)
  cl_export(cl, "f", environment(f))
  parallel::clusterEvalQ(cl, exists("f")) |> unlist() |> all() |> expect_true()
  pbapply::pblapply(1:10, \(.input) f(.input), cl = cl)
  cl_stop(cl)

  # Check cl_stop()
  cl_stop()

  # Check cl_lapply()
  cl_lapply(1:10, \(x) x + 1, .cl = 2L)
  cl_lapply(1:10, \(x) x + 1, .cl = 2L, .use_chunks = TRUE)
  cl_lapply(1:10, \(x) x + 1, .cl = parallel::makeCluster(2L))
  cl_lapply(1:10, \(x) x + 1, .cl = parallel::makeCluster(2L), .use_chunks = FALSE)

})
