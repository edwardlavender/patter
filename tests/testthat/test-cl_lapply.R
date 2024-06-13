test_that("cl_lapply() works", {

  # cl_chunk()
  expect_false(cl_chunk(1))
  expect_true(cl_chunk(2))

  # cl_lapply() basic implementation
  output <- cl_lapply(1:10, .fun = function(i) i)
  expect_equal(output, as.list(1:10))
  output <- cl_lapply(list(a = 1, b = 2),
                      .fun = function(i) i,
                      .combine = unlist)
  expect_equal(output, c(a = 1, b = 2))

  # cl_lapply() basic implementation chunk-wise
  cl_lapply(1:10, .fun = function(i) print(i), .chunk = TRUE) |>
    expect_warning("cores = 1L: `.chunk = TRUE` is inefficient on one core.",
                   fixed = TRUE)

  # cl_lapply() with .chunk = TRUE and .chunk_fun
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
                      .cl = 2L)
  expect_equal(unname(unlist(output)), 1:10)

})

