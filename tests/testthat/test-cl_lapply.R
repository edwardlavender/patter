test_that("cl_lapply() works", {

  is_unix <- .Platform$OS.type == "unix"
  is_win  <- .Platform$OS.type == "windows"

  # cl_chunk()
  expect_false(cl_chunk(1))
  expect_true(cl_chunk(2))

  # cl_lapply() basic serial implementation
  output <- cl_lapply(1:10, .fun = function(i) i)
  expect_equal(output, as.list(1:10))
  output <- cl_lapply(list(a = 1, b = 2),
                      .fun = function(i) i,
                      .combine = unlist)
  expect_equal(output, c(a = 1, b = 2))
  cl_lapply(1:10, .fun = function(i) print(i), .chunk = TRUE) |>
    expect_warning("cores = 1L: `.chunk = TRUE` is inefficient on one core.",
                   fixed = TRUE)

  # cl_lapply() basic parallel implementation
  cl_lapply(1:10, \(x) x + 0, .cl = 2L) |>
    expect_equal(as.list(1:10L))
  if (is_unix) {
    cl_lapply(1:10, \(x) x + 0, .cl = 2L, .chunk = TRUE, .combine = unlist) |>
      expect_equal(c(1:10L))
  }
  cl_lapply(1:10, \(x) x + 0, .cl = parallel::makeCluster(2L)) |>
    expect_equal(as.list(1:10L))
  cl_lapply(1:10, \(x) x + 0, .cl = parallel::makeCluster(2L), .chunk = FALSE) |>
    expect_equal(as.list(1:10L))

  # cl_lapply() preserves names
  output    <- cl_lapply(list(a = 1, b = 2), .fun = function(i) i + 1)
  expected <- list(a = 2, b = 3)
  expect_equal(output, expected)

  # cl_lapply() with .chunk = TRUE and .chunk_fun (serial)
  mapw <- terra::wrap(terra::setValues(dat_gebco(), 0))
  output <- cl_lapply(1:10,
                      .fun = function(.i, .chunkargs) {
                        out <- .chunkargs$map + .i
                        out[1]
                      },
                      .chunk_fun = function(.chunkargs) {
                        list(map = terra::unwrap(mapw))
                      },
                      .chunk = TRUE,
                      .cl = 1L) |>
    suppressWarnings()
  expect_equal(unname(unlist(output)), 1:10)

  # As above (parallel)
  if (is_unix) {
    output <- cl_lapply(1:10,
                        .fun = function(.i, .chunkargs) {
                          out <- .chunkargs$map + .i
                          out[1]
                        },
                        .chunk_fun = function(.chunkargs) {
                          list(map = terra::unwrap(mapw))
                        },
                        .chunk = TRUE,
                        .cl = 2L)
    expect_equal(unname(unlist(output)), 1:10)
  }

})

